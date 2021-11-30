//===--- ContainerPushBackCheck.cpp - clang-tidy --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ContainerPushBackCheck.h"
#include "../utils/DeclRefExprUtils.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include <iostream>

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace bugprone {

void ContainerPushBackCheck::registerMatchers(MatchFinder *Finder) {
  const auto ContainerDecl = cxxRecordDecl(hasAnyName("::std::vector", "::std::list"));
  const auto AppendMethodDecl =
      cxxMethodDecl(hasAnyName("push_back", "emplace_back"));
  const auto ContainerConstructorCall = cxxConstructExpr(
      hasType(ContainerDecl),
      hasDeclaration(cxxConstructorDecl(hasAnyName("vector", "list"))), argumentCountIs(1),
      hasArgument(0, hasType(qualType(isInteger()))));
  const auto TargetVarDecl =
      varDecl(hasInitializer(ContainerConstructorCall)).bind("container_var_decl");
  const auto TargetVarDefStmt =
      declStmt(hasSingleDecl(TargetVarDecl)).bind("container_var_decl_stmt");
  const auto AppendCallExpr =
      cxxMemberCallExpr(
          callee(AppendMethodDecl), on(hasType(ContainerDecl)),
          onImplicitObjectArgument(declRefExpr(to(TargetVarDecl))))
          .bind("container_append_call");
  const auto AppendCall = expr(AppendCallExpr);
  const auto CompoundStmtWithContainer =
      compoundStmt(has(TargetVarDefStmt)).bind("compound_stmt_with_container");
  const auto FunctionDeclWithContainer =
      functionDecl(
          hasBody(allOf(compoundStmt(has(AppendCall)), CompoundStmtWithContainer)))
          .bind("function_decl_with_container");
  Finder->addMatcher(FunctionDeclWithContainer, this);
}

void ContainerPushBackCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *FunctionDeclWithContainer =
      Result.Nodes.getNodeAs<FunctionDecl>("function_decl_with_container");
  if (!FunctionDeclWithContainer)
    return;
  const auto *TargetVarDecl =
      Result.Nodes.getNodeAs<VarDecl>("container_var_decl");
  const auto *TargetVarDefStmt =
      Result.Nodes.getNodeAs<DeclStmt>("container_var_decl_stmt");
  const auto *AppendCallExpr =
      Result.Nodes.getNodeAs<CXXMemberCallExpr>("container_append_call");
  const auto *CompoundStmtWithContainer =
      Result.Nodes.getNodeAs<CompoundStmt>("compound_stmt_with_container");
  if (!TargetVarDecl || !TargetVarDefStmt || !AppendCallExpr)
    return;
  auto *Context = Result.Context;
  llvm::SmallPtrSet<const DeclRefExpr *, 16> AllVarRefs =
      utils::decl_ref_expr::allDeclRefExprs(*TargetVarDecl,
                                            *CompoundStmtWithContainer, *Context);
  SourceManager &SM = Result.Context->getSourceManager();
  for (const auto *Ref : AllVarRefs) {
    if (Ref != AppendCallExpr->getImplicitObjectArgument())
      return;
    else
      break;
  }
  diag(AppendCallExpr->getExprLoc(),
       "Call function %0 after container resize may be faulty")
      << AppendCallExpr->getMethodDecl();
}

} // namespace bugprone
} // namespace tidy
} // namespace clang
