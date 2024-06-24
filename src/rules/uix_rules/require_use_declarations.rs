
use std::collections::HashSet;

// Copyright 2023 unyt.org
use super::super::{Context, LintRule};
use crate::diagnostic::LintDiagnosticSeverity;
use crate::handler::{Handler, Traverse};
use crate::Program;
use deno_ast::swc::ast::Id;
use deno_ast::view::{Expr, JSXAttrName, JSXAttrValue, NodeTrait, Callee, BlockStmt, JSXExpr, BlockStmtOrExpr};
use deno_ast::SourceRanged;

#[derive(Debug)]
pub struct RequireUseDeclarations;

const CODE: &str = "uix-require-use-declarations";
const MESSAGE: &str = "use() declarations are required in transferable closures";
const HINT: &str =
  "Put all external variables used inside this closure in a use() declaration at the start of the closure";

impl LintRule for RequireUseDeclarations {
  fn tags(&self) -> &'static [&'static str] {
    &["recommended"]
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

  #[cfg(feature = "docs")]
  fn docs(&self) -> &'static str {
    include_str!("../../../docs/rules/uix_rules/require_use_declarations.md")
  }
}


fn traverse_transferable_function_scope(stmt: &BlockStmt, context: &mut Context) {
  let mut handler = FindVariablesHandler {used_vars: HashSet::new()};
  handler.traverse(stmt.as_node(), context);
}


struct RequireUseDeclarationsHandler;

impl Handler for RequireUseDeclarationsHandler {


  fn jsx_attr(&mut self, attr: &deno_ast::view::JSXAttr, context: &mut Context) {
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
                    },
                    JSXExpr::Expr(Expr::Arrow(fn_expr)) => {
                      match fn_expr.body {
                        BlockStmtOrExpr::Expr(_expr) => {/* todo */}
                        BlockStmtOrExpr::BlockStmt(stmt) => {
                          traverse_transferable_function_scope(stmt, context)    
                        }
                      }
                    },
                    _ => {}
                  }
                }
                _ => {},
              }
            }

          }
      }
      _ => {}
    }
  }

}


struct FindVariablesHandler {
  used_vars: HashSet<Id>
}


impl Handler for FindVariablesHandler {

  fn call_expr(&mut self, expr: &deno_ast::view::CallExpr, _context: &mut Context) {
    match expr.callee {
      Callee::Expr(callee) => {
        match callee {
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
        }
      }
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

	  let fixes = vec!();

      context.add_diagnostic_with_hint_and_severity(
        id.range(),
        CODE,
        MESSAGE,
        HINT,
		fixes,
      LintDiagnosticSeverity::ERROR
      );
  }

}