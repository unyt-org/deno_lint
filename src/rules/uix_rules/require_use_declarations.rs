use std::collections::HashSet;

// Copyright 2023 unyt.org
use super::super::{Context, LintRule};
use crate::handler::{Handler, Traverse};
use crate::{tags, Program};
use deno_ast::diagnostics::DiagnosticLevel;
use deno_ast::swc::ast::Id;
use deno_ast::view::{
  BlockStmt, BlockStmtOrExpr, Callee, Expr, JSXAttrName, JSXAttrValue, JSXExpr,
  Node, NodeTrait,
};
use deno_ast::SourceRanged;

#[derive(Debug)]
pub struct RequireUseDeclarations;

const CODE: &str = "uix-require-use-declarations";
const MESSAGE: &str =
  "use() declarations are required in transferable closures";
const HINT: &str =
  "Put all external variables used inside this closure in a use() declaration at the start of the closure";

impl LintRule for RequireUseDeclarations {
  fn tags(&self) -> &'static [tags::Tag] {
    &[tags::RECOMMENDED]
  }

  fn code(&self) -> &'static str {
    CODE
  }

  fn lint_program_with_ast_view(
    &self,
    context: &mut Context,
    program: Program,
  ) {
    RequireUseDeclarationsHandler.traverse(program, context);
  }
}

fn traverse_transferable_function_scope(
  stmt: &BlockStmt,
  context: &mut Context,
) {
  let mut handler = FindVariablesHandler {
    used_vars: HashSet::new(),
  };
  handler.traverse(stmt.as_node(), context);
}

struct RequireUseDeclarationsHandler;

impl Handler for RequireUseDeclarationsHandler {
  fn jsx_attr(
    &mut self,
    attr: &deno_ast::view::JSXAttr,
    context: &mut Context,
  ) {
    match attr.name {
      JSXAttrName::JSXNamespacedName(name) => {
        // :frontend jsx attribute
        if name.name.sym().as_str() == "frontend" {
          if let Some(value) = attr.value {
            match value {
              JSXAttrValue::JSXExprContainer(container) => {
                match container.expr {
                  JSXExpr::Expr(Expr::Fn(fn_expr)) => {
                    if let Some(body) = fn_expr.function.body {
                      traverse_transferable_function_scope(body, context)
                    }
                  }
                  JSXExpr::Expr(Expr::Arrow(fn_expr)) => {
                    match fn_expr.body {
                      BlockStmtOrExpr::Expr(_expr) => { /* todo */ }
                      BlockStmtOrExpr::BlockStmt(stmt) => {
                        traverse_transferable_function_scope(stmt, context)
                      }
                    }
                  }
                  _ => {}
                }
              }
              _ => {}
            }
          }
        }
      }
      _ => {}
    }
  }

  // fn var_decl(&mut self, _n: &deno_ast::view::VarDecl, _ctx: &mut Context) {
  //     _n.kind()
  // }

  fn arrow_expr(
    &mut self,
    expr: &deno_ast::view::ArrowExpr,
    context: &mut Context,
  ) {
    // iterate ancestors and check if the arrow function is callback of a "run" function
    for ancestor in expr.ancestors() {
      match ancestor {
        Node::CallExpr(call_expr) => {
          match call_expr.callee {
            Callee::Expr(callee) => {
              if let Expr::Ident(ident) = callee {
                // check if the jsdoc commment for the variable includes the word "thread"

                if ident.sym().as_str() == "run" {
                  match expr.body {
                    BlockStmtOrExpr::Expr(_expr) => { /* todo */ }
                    BlockStmtOrExpr::BlockStmt(stmt) => {
                      traverse_transferable_function_scope(stmt, context)
                    }
                  }
                }
              }
            }
            _ => {}
          }
        }
        _ => {}
      }
    }
  }
}

struct FindVariablesHandler {
  used_vars: HashSet<Id>,
}

impl Handler for FindVariablesHandler {
  fn call_expr(
    &mut self,
    expr: &deno_ast::view::CallExpr,
    _context: &mut Context,
  ) {
    match expr.callee {
      Callee::Expr(callee) => match callee {
        Expr::Ident(name) => {
          if name.inner.sym.as_str() == "use" {
            for arg in expr.args.iter() {
              match arg.expr {
                Expr::Ident(ident) => {
                  self.used_vars.insert(ident.to_id());
                }
                _ => {}
              }
            }
          }
        }
        _ => {}
      },
      _ => {}
    }
  }

  fn ident(&mut self, id: &deno_ast::view::Ident, context: &mut Context) {
    if context.scope().is_global(&id.to_id()) {
      return;
    }

    if context.scope().var_by_ident(id).is_none() {
      return;
    }

    if self.used_vars.contains(&id.to_id()) {
      return;
    }

    let fixes = vec![];

    context.add_diagnostic_with_severity(
      id.range(),
      CODE,
      MESSAGE,
      Some(HINT.to_string()),
      fixes,
      DiagnosticLevel::Error,
    );
  }
}
