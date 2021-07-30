//===--- MoveConstArgCheck.cpp - clang-tidy -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MoveConstArgCheck.h"

#include "clang/Lex/Lexer.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace performance {

static void replaceCallWithArg(const CallExpr *Call, DiagnosticBuilder &Diag,
                               const SourceManager &SM,
                               const LangOptions &LangOpts) {
  const Expr *Arg = Call->getArg(0);

  CharSourceRange BeforeArgumentsRange = Lexer::makeFileCharRange(
      CharSourceRange::getCharRange(Call->getBeginLoc(), Arg->getBeginLoc()),
      SM, LangOpts);
  CharSourceRange AfterArgumentsRange = Lexer::makeFileCharRange(
      CharSourceRange::getCharRange(Call->getEndLoc(),
                                    Call->getEndLoc().getLocWithOffset(1)),
      SM, LangOpts);

  if (BeforeArgumentsRange.isValid() && AfterArgumentsRange.isValid()) {
    Diag << FixItHint::CreateRemoval(BeforeArgumentsRange)
         << FixItHint::CreateRemoval(AfterArgumentsRange);
  }
}

void MoveConstArgCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "CheckTriviallyCopyableMove", CheckTriviallyCopyableMove);
}

void MoveConstArgCheck::registerMatchers(MatchFinder *Finder) {
  auto MoveCallMatcher =
      callExpr(callee(functionDecl(hasName("::std::move"))), argumentCountIs(1),
               unless(isInTemplateInstantiation()))
          .bind("call-move");

  Finder->addMatcher(MoveCallMatcher, this);

  Finder->addMatcher(
      returnStmt(
          hasReturnValue(anyOf(
              ignoringImplicit(ignoringParenCasts(MoveCallMatcher)),
              cxxConstructExpr(hasDeclaration(cxxConstructorDecl(allOf(
                                   isUserProvided(), isMoveConstructor()))),
                               hasAnyArgument(ignoringImplicit(
                                   ignoringParenCasts(MoveCallMatcher)))))))
          .bind("return-call-move"),
      this);

  Finder->addMatcher(
      invocation(forEachArgumentWithParam(
                     MoveCallMatcher,
                     parmVarDecl(anyOf(hasType(references(isConstQualified())),
                                       hasType(rValueReferenceType())))
                         .bind("invocation-parm")))
          .bind("receiving-expr"),
      this);
}

void MoveConstArgCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *CallMove = Result.Nodes.getNodeAs<CallExpr>("call-move");
  const auto *ReturnCallMove =
      Result.Nodes.getNodeAs<ReturnStmt>("return-call-move");
  const auto *ReceivingExpr = Result.Nodes.getNodeAs<Expr>("receiving-expr");
  const auto *InvocationParm =
      Result.Nodes.getNodeAs<ParmVarDecl>("invocation-parm");

  if (!ReturnCallMove && !ReceivingExpr && HasCheckedMoveSet.count(CallMove))
    return;

  if (ReturnCallMove || ReceivingExpr)
    HasCheckedMoveSet.insert(CallMove);

  const Expr *Arg = CallMove->getArg(0);
  SourceManager &SM = Result.Context->getSourceManager();

  CharSourceRange MoveRange =
      CharSourceRange::getCharRange(CallMove->getSourceRange());
  CharSourceRange FileMoveRange =
      Lexer::makeFileCharRange(MoveRange, SM, getLangOpts());
  if (!FileMoveRange.isValid())
    return;

  bool IsConstArg = Arg->getType().isConstQualified();
  bool IsTriviallyCopyable =
      Arg->getType().isTriviallyCopyableType(*Result.Context);

  if (IsConstArg || IsTriviallyCopyable) {
    if (const CXXRecordDecl *R = Arg->getType()->getAsCXXRecordDecl()) {
      // According to [expr.prim.lambda]p3, "whether the closure type is
      // trivially copyable" property can be changed by the implementation of
      // the language, so we shouldn't rely on it when issuing diagnostics.
      if (R->isLambda())
        return;
      // Don't warn when the type is not copyable.
      for (const auto *Ctor : R->ctors()) {
        if (Ctor->isCopyConstructor() && Ctor->isDeleted())
          return;
      }
    }

    if (!IsConstArg && IsTriviallyCopyable && !CheckTriviallyCopyableMove)
      return;

    bool IsVariable = isa<DeclRefExpr>(Arg);
    bool IsInvocationArg = false;
    StringRef FuncName;
    const auto *Var =
        IsVariable ? dyn_cast<DeclRefExpr>(Arg)->getDecl() : nullptr;
    if (ReceivingExpr &&
        InvocationParm->getOriginalType()->isRValueReferenceType() &&
        !ReceivingExpr->getType()->isRecordType() && Arg->isLValue()) {
      IsInvocationArg = true;
      const auto *ReceivingCallExpr = dyn_cast<CallExpr>(ReceivingExpr);
      if (!ReceivingCallExpr)
        return;
      const auto *FD = ReceivingCallExpr->getDirectCallee();
      FuncName = FD->getName();
    }
    auto Diag =
        diag(FileMoveRange.getBegin(),
             "std::move of the %select{|const }0"
             "%select{expression|variable %5}1 "
             "%select{|of the trivially-copyable type %6 }2"
             "has no effect; %select{remove std::move()|}3"
             "%select{| or make the variable non-const}4"
             "%select{|consider changing %7's parameter from %6&& to %6&}3")
        << IsConstArg << IsVariable << IsTriviallyCopyable << IsInvocationArg
        << (IsConstArg && IsVariable && !IsTriviallyCopyable &&
            !IsInvocationArg)
        << Var << Arg->getType() << FuncName;

    if (!IsInvocationArg)
      replaceCallWithArg(CallMove, Diag, SM, getLangOpts());
  } else if (ReturnCallMove || ReceivingExpr) {
    if (ReceivingExpr &&
        InvocationParm->getOriginalType()->isRValueReferenceType() &&
        Arg->isLValue())
      return;

    bool IsMoveConstruct = false;
    if (ReturnCallMove ||
        InvocationParm->getOriginalType()->isRValueReferenceType())
      IsMoveConstruct = true;
    auto Diag =
        diag(FileMoveRange.getBegin(),
             "%select{passing result of std::move() as a const reference "
             "argument; no move will actually happen|it's superfluous; a move "
             "will happen, with or without the std::move}0")
        << IsMoveConstruct;

    replaceCallWithArg(CallMove, Diag, SM, getLangOpts());
  }
}

} // namespace performance
} // namespace tidy
} // namespace clang
