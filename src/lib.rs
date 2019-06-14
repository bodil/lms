use std::any::Any;
use std::fmt::Debug;
use std::ops::{Add, Mul, Sub};

pub mod codegen;

pub struct Context {
    symbol_table: Vec<Box<dyn Any>>,
    fresh: usize,
}

impl Default for Context {
    fn default() -> Self {
        Context {
            symbol_table: Vec::new(),
            fresh: 0,
        }
    }
}

impl Context {
    fn lookup_def<A, D>(&self, d: &D) -> Option<usize>
    where
        D: Def<A> + 'static,
    {
        for (index, def) in self.symbol_table.iter().enumerate() {
            if let Some(def) = (def as &dyn Any).downcast_ref::<D>() {
                if def == d {
                    return Some(index);
                }
            }
        }
        None
    }

    fn insert_def<A, D>(&mut self, d: D) -> usize
    where
        D: Def<A> + 'static,
    {
        let d: Box<dyn Any> = Box::new(d);
        let size = self.symbol_table.len();
        println!("inserted node of type {:?} : {:?}", d.type_id(), d);
        self.symbol_table.insert(size, d);
        size
    }

    fn cast_def<D: 'static>(&self, index: usize) -> Option<&D> {
        let def: &Box<dyn Any> = &self.symbol_table[index];
        println!(
            "casting type {:?} to {:?} : value {:?}",
            def.type_id(),
            std::any::TypeId::of::<D>(),
            (&self.symbol_table[index] as &dyn Debug)
        );
        (&**def).downcast_ref()
    }

    fn def_debug(&self, index: usize) -> &dyn Debug {
        &self.symbol_table[index]
    }
}

pub trait Rep<A> {
    type Node;

    fn fresh() -> Self;
    fn unit(value: A) -> Self;

    fn run(self, context: &mut Context) -> Self::Node;

    fn bind<F>(self, f: F) -> Exp<A>
    where
        F: Fn(ExpNode<A>) -> Exp<A> + 'static,
        A: 'static;

    fn bind2<F, B>(left: Self, right: B, f: F) -> Exp<A>
    where
        B: Rep<A> + 'static,
        F: Fn(ExpNode<A>, B::Node) -> Exp<A> + 'static,
        A: 'static;
}

pub trait NumericOps<A>:
    Rep<A> + Add<Self, Output = Self> + Sub<Self, Output = Self> + Mul<Self, Output = Self> + Sized
{
    fn plus(left: Self, right: Self) -> Self;
    fn minus(left: Self, right: Self) -> Self;
    fn mult(left: Self, right: Self) -> Self;
}

#[derive(PartialEq, Eq, Debug)]
pub enum ExpNode<A> {
    Sym(usize),
    Const(A),
}

pub struct Exp<A>(Box<dyn FnOnce(&mut Context) -> ExpNode<A>>);

impl<A> Exp<A> {
    pub fn new<F>(f: F) -> Self
    where
        F: FnOnce(&mut Context) -> ExpNode<A> + 'static,
    {
        Exp(Box::new(f))
    }
}

impl<A: 'static> From<ExpNode<A>> for Exp<A> {
    fn from(node: ExpNode<A>) -> Self {
        Exp(Box::new(move |_| node))
    }
}

impl<A: 'static> Rep<A> for Exp<A> {
    type Node = ExpNode<A>;

    fn fresh() -> Self {
        Exp::new(move |ctx| {
            ctx.fresh += 1;
            to_atom(Fresh(ctx.fresh)).run(ctx)
        })
    }

    fn unit(value: A) -> Self {
        ExpNode::Const(value).into()
    }

    fn run(self, ctx: &mut Context) -> Self::Node {
        (self.0)(ctx)
    }

    fn bind<F>(self, f: F) -> Exp<A>
    where
        F: Fn(ExpNode<A>) -> Exp<A> + 'static,
        A: 'static,
    {
        Exp::new(move |ctx| f(self.run(ctx)).run(ctx))
    }

    fn bind2<F, B>(left: Self, right: B, f: F) -> Exp<A>
    where
        B: Rep<A> + 'static,
        F: Fn(ExpNode<A>, B::Node) -> Exp<A> + 'static,
        A: 'static,
    {
        Exp::new(move |ctx| f(left.run(ctx), right.run(ctx)).run(ctx))
    }
}

fn to_atom<A, D>(def: D) -> Exp<A>
where
    D: Def<A> + 'static,
{
    Exp::new(move |ctx| {
        if let Some(index) = ctx.lookup_def(&def) {
            ExpNode::Sym(index)
        } else {
            println!("inserting node of type {:?} : {:?}", def.type_id(), def);
            ExpNode::Sym(ctx.insert_def(def))
        }
    })
}

trait Def<A>: Eq + Debug + Any {}

#[derive(PartialEq, Eq, Debug)]
struct Fresh(usize);
impl<A> Def<A> for Fresh {}

#[derive(PartialEq, Eq, Debug)]
struct Plus<A> {
    left: ExpNode<A>,
    right: ExpNode<A>,
}
impl Def<isize> for Plus<isize> {}

#[derive(PartialEq, Eq, Debug)]
struct Minus<A> {
    left: ExpNode<A>,
    right: ExpNode<A>,
}
impl Def<isize> for Minus<isize> {}

#[derive(PartialEq, Eq, Debug)]
struct Mult<A> {
    left: ExpNode<A>,
    right: ExpNode<A>,
}
impl Def<isize> for Mult<isize> {}

impl NumericOps<isize> for Exp<isize> {
    fn plus(left: Self, right: Self) -> Self {
        Exp::bind2(left, right, |left, right| match (left, right) {
            (ExpNode::Const(left), ExpNode::Const(right)) => ExpNode::Const(left + right).into(),
            (left, right) => to_atom(Plus { left, right }),
        })
    }

    fn minus(left: Self, right: Self) -> Self {
        Exp::bind2(left, right, |left, right| match (left, right) {
            (ExpNode::Const(left), ExpNode::Const(right)) => ExpNode::Const(left + right).into(),
            (left, right) => to_atom(Minus { left, right }),
        })
    }

    fn mult(left: Self, right: Self) -> Self {
        Exp::bind2(left, right, |left, right| match (left, right) {
            (ExpNode::Const(left), ExpNode::Const(right)) => ExpNode::Const(left * right).into(),
            (left, right) => to_atom(Mult { left, right }),
        })
    }
}

impl<A> Add<Exp<A>> for Exp<A>
where
    Exp<A>: NumericOps<A>,
{
    type Output = Exp<A>;
    fn add(self, other: Self) -> Self::Output {
        NumericOps::plus(self, other)
    }
}

impl<A> Sub<Exp<A>> for Exp<A>
where
    Exp<A>: NumericOps<A>,
{
    type Output = Exp<A>;
    fn sub(self, other: Self) -> Self::Output {
        NumericOps::minus(self, other)
    }
}

impl<A> Mul<Exp<A>> for Exp<A>
where
    Exp<A>: NumericOps<A>,
{
    type Output = Exp<A>;
    fn mul(self, other: Self) -> Self::Output {
        NumericOps::mult(self, other)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn collapse_constant() {
        let prog = (Exp::unit(5) * Rep::unit(2)) + Rep::unit(1);
        let result = prog.run(&mut Context::default());
        println!("{:?}", result);
        assert_eq!(ExpNode::Const(11), result);
    }

    #[test]
    fn collapse_non_fresh_constant() {
        let mut ctx = Context::default();
        let prog: Exp<isize> = (Exp::unit(5) * Rep::unit(2)) + Rep::fresh();
        let result = prog.run(&mut ctx);
        if let ExpNode::Sym(index) = result {
            let sym: &Plus<isize> = ctx.cast_def(index).unwrap();
            println!("{:?}", sym);
            assert_eq!(
                &Plus {
                    left: ExpNode::Const(10),
                    right: ExpNode::Sym(0)
                },
                sym
            );
        } else {
            panic!("result was not a symbol");
        }
    }
}
