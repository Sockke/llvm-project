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
  const auto VectorDecl = cxxRecordDecl(hasName("::std::vector"));
  const auto AppendMethodDecl =
      cxxMethodDecl(hasAnyName("push_back", "emplace_back"));
  const auto VectorConstructorCall = cxxConstructExpr(
      hasType(VectorDecl),
      hasDeclaration(cxxConstructorDecl(hasName("vector"))), argumentCountIs(1),
      hasArgument(0, hasType(qualType(isInteger()))));
  const auto TargetVarDecl =
      varDecl(hasInitializer(VectorConstructorCall)).bind("vector_var_decl");
  const auto TargetVarDefStmt =
      declStmt(hasSingleDecl(TargetVarDecl)).bind("vector_var_decl_stmt");
  const auto AppendCallExpr =
      cxxMemberCallExpr(
          callee(AppendMethodDecl), on(hasType(VectorDecl)),
          onImplicitObjectArgument(declRefExpr(to(TargetVarDecl))))
          .bind("vector_append_call");
  const auto AppendCall = expr(AppendCallExpr);
  const auto CompoundStmtWithVector =
      compoundStmt(has(TargetVarDefStmt)).bind("compound_stmt_with_vector");
  const auto FunctionDeclWithVector =
      functionDecl(
          hasBody(allOf(compoundStmt(has(AppendCall)), CompoundStmtWithVector)))
          .bind("function_decl_with_vector");
  Finder->addMatcher(FunctionDeclWithVector, this);
}

void ContainerPushBackCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *FunctionDeclWithVector =
      Result.Nodes.getNodeAs<FunctionDecl>("function_decl_with_vector");
  if (!FunctionDeclWithVector)
    return;
  const auto *TargetVarDecl =
      Result.Nodes.getNodeAs<VarDecl>("vector_var_decl");
  const auto *TargetVarDefStmt =
      Result.Nodes.getNodeAs<DeclStmt>("vector_var_decl_stmt");
  const auto *AppendCallExpr =
      Result.Nodes.getNodeAs<CXXMemberCallExpr>("vector_append_call");
  const auto *CompoundStmtWithVector =
      Result.Nodes.getNodeAs<CompoundStmt>("compound_stmt_with_vector");
  if (!TargetVarDecl || !TargetVarDefStmt || !AppendCallExpr)
    return;
  auto *Context = Result.Context;
  llvm::SmallPtrSet<const DeclRefExpr *, 16> AllVarRefs =
      utils::decl_ref_expr::allDeclRefExprs(*TargetVarDecl,
                                            *CompoundStmtWithVector, *Context);
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
