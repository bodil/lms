use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{
    punctuated::Punctuated, token, BinOp, Expr, ExprBinary, ExprLit, ExprPath, Ident, IntSuffix,
    Lit, LitInt, Path, PathArguments, PathSegment,
};

use crate::{Context, ExpNode, Fresh, Minus, Mult, Plus};

pub trait Compile<Target> {
    fn compile(&self, context: &Context) -> Target;
}

pub trait ToAST {
    type Output: ToTokens;
    fn to_ast(&self, context: &Context) -> Self::Output;
}

// Everything that implements `Def` must be handled here!
fn expand_sym<A: ToAST<Output = Expr> + 'static>(ctx: &Context, index: usize) -> Expr {
    if let Some(fresh) = ctx.cast_def::<Fresh>(index) {
        return fresh.to_ast(ctx);
    }
    if let Some(plus) = ctx.cast_def::<Plus<A>>(index) {
        return plus.to_ast(ctx);
    }
    if let Some(minus) = ctx.cast_def::<Minus<A>>(index) {
        return minus.to_ast(ctx);
    }
    if let Some(mult) = ctx.cast_def::<Mult<A>>(index) {
        return mult.to_ast(ctx);
    }
    unimplemented!("unhandled AST node type {:?}", ctx.def_debug(index))
}

impl<A> Compile<TokenStream> for ExpNode<A>
where
    A: ToAST<Output = Expr> + 'static,
{
    fn compile(&self, ctx: &Context) -> TokenStream {
        match self {
            ExpNode::Const(value) => value.to_ast(ctx).into_token_stream(),
            ExpNode::Sym(index) => expand_sym::<A>(ctx, *index).into_token_stream(),
        }
    }
}

impl<A> ToAST for ExpNode<A>
where
    A: ToAST<Output = Expr> + 'static,
{
    type Output = Expr;
    fn to_ast(&self, ctx: &Context) -> Self::Output {
        match self {
            ExpNode::Const(value) => value.to_ast(ctx),
            ExpNode::Sym(index) => expand_sym::<A>(ctx, *index),
        }
    }
}

impl ToAST for Fresh {
    type Output = Expr;
    fn to_ast(&self, _ctx: &Context) -> Self::Output {
        let mut path = Punctuated::new();
        path.push_value(PathSegment {
            ident: Ident::new(&format!("v{}", self.0), Span::call_site()),
            arguments: PathArguments::None,
        });
        Expr::Path(ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: Path {
                leading_colon: None,
                segments: path,
            },
        })
    }
}

impl<A> ToAST for Plus<A>
where
    A: ToAST<Output = Expr> + 'static,
{
    type Output = Expr;
    fn to_ast(&self, ctx: &Context) -> Self::Output {
        let left = self.left.to_ast(ctx);
        let right = self.right.to_ast(ctx);
        Expr::Binary(ExprBinary {
            attrs: Vec::new(),
            op: BinOp::Add(token::Add {
                spans: [Span::call_site()],
            }),
            left: Box::new(left),
            right: Box::new(right),
        })
    }
}

impl<A> ToAST for Minus<A>
where
    A: ToAST<Output = Expr> + 'static,
{
    type Output = Expr;
    fn to_ast(&self, ctx: &Context) -> Self::Output {
        let left = self.left.to_ast(ctx);
        let right = self.right.to_ast(ctx);
        Expr::Binary(ExprBinary {
            attrs: Vec::new(),
            op: BinOp::Sub(token::Sub {
                spans: [Span::call_site()],
            }),
            left: Box::new(left),
            right: Box::new(right),
        })
    }
}

impl<A> ToAST for Mult<A>
where
    A: ToAST<Output = Expr> + 'static,
{
    type Output = Expr;
    fn to_ast(&self, ctx: &Context) -> Self::Output {
        let left = self.left.to_ast(ctx);
        let right = self.right.to_ast(ctx);
        Expr::Binary(ExprBinary {
            attrs: Vec::new(),
            op: BinOp::Mul(token::Star {
                spans: [Span::call_site()],
            }),
            left: Box::new(left),
            right: Box::new(right),
        })
    }
}

fn lit_to_expr(lit: Lit) -> Expr {
    Expr::Lit(ExprLit {
        attrs: Vec::new(),
        lit,
    })
}

impl ToAST for isize {
    type Output = Expr;
    fn to_ast(&self, _ctx: &Context) -> Self::Output {
        lit_to_expr(Lit::Int(LitInt::new(
            *self as u64,
            IntSuffix::Isize,
            Span::call_site(),
        )))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{Exp, Rep};

    #[test]
    fn compile_some_shit() {
        let mut ctx = Context::default();
        let prog = (Exp::unit(5) * Rep::unit(2)) + Rep::fresh();
        let ast = prog.run(&mut ctx);
        let stream = ast.compile(&ctx);
        println!("{}", stream);
        panic!();
    }
}
