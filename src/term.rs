use std::collections::{HashMap, HashSet};

use crate::{buff::Buff, sexpr::Sexpr};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Var(String),
    All(String, Box<Type>),
    Fun(Box<Type>, Box<Type>),
}

impl Type {
    fn closed_aux(&self, env: &HashSet<String>) -> bool {
        match self {
            Type::Var(x) => env.contains(x),
            Type::All(x, t) => {
                let mut new_env = env.clone();
                new_env.insert(x.clone());
                t.closed_aux(&new_env)
            }
            Type::Fun(t1, t2) => t1.closed_aux(env) && t2.closed_aux(env),
        }
    }

    pub fn closed(&self) -> bool {
        self.closed_aux(&HashSet::from([]))
    }

    fn vars_aux(&self, binders: &HashSet<String>, env: &mut HashSet<String>) {
        match self {
            Type::Var(x) => {
                if !binders.contains(x) {
                    env.insert(x.clone());
                }
            }
            Type::All(x, t) => {
                let mut new_binders = binders.clone();
                new_binders.insert(x.clone());
                t.vars_aux(&new_binders, env);
            }
            Type::Fun(t1, t2) => {
                t1.vars_aux(binders, env);
                t2.vars_aux(binders, env)
            }
        }
    }

    pub fn vars(&self) -> HashSet<String> {
        let mut env = HashSet::from([]);
        let binders = HashSet::from([]);
        self.vars_aux(&binders, &mut env);
        env
    }

    pub fn subst(&self, x: &String, t: Type) -> Self {
        match self {
            Type::Var(y) => {
                if x == y {
                    t
                } else {
                    self.clone()
                }
            }
            Type::All(a, ta) => {
                if x == a {
                    self.clone()
                } else {
                    Type::All(a.clone(), ta.subst(x, t).into())
                }
            }
            Type::Fun(tl, tr) => Type::Fun(tl.subst(x, t.clone()).into(), tr.subst(x, t).into()),
        }
    }
}

pub enum Term {
    /// Type abstraction
    TLam(String, Box<Term>),
    /// Lambda abstraction
    Lam(String, Type, Box<Term>),
    /// Application
    App(Box<Term>, Box<Term>),
    /// Type instanciation
    TApp(Box<Term>, Type),
    /// Variables
    Var(String),
}

impl Term {
    fn typed_aux(&self, tenv: &HashSet<String>, env: &HashMap<String, Type>) -> Option<Type> {
        match self {
            Term::TLam(a, e) => {
                let mut new_tenv = tenv.clone();
                new_tenv.insert(a.clone());
                e.typed_aux(&new_tenv, env)
                    .map(|te| Type::All(a.clone(), te.into()))
            }
            Term::Lam(x, tx, e) => {
                if tx.closed_aux(tenv) {
                    let mut new_env = env.clone();
                    new_env.insert(x.clone(), tx.clone());
                    e.typed_aux(tenv, &new_env)
                        .map(|te| Type::Fun(tx.clone().into(), te.into()))
                } else {
                    None
                }
            }
            Term::App(e1, e2) => {
                if let Some(Type::Fun(t1l, t1r)) = e1.typed_aux(tenv, env) {
                    let t2 = e2.typed_aux(tenv, env)?;
                    if *t1l == t2 {
                        Some(*t1r)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Term::TApp(e, t) => {
                if let Some(Type::All(a, te)) = e.typed_aux(tenv, env) {
                    Some(te.subst(&a, t.clone()))
                } else {
                    None
                }
            }
            Term::Var(x) => env.get(x).cloned(),
        }
    }

    pub fn typed(&self) -> Option<Type> {
        let tenv = HashSet::from([]);
        let env = HashMap::from([]);
        self.typed_aux(&tenv, &env)
    }
}

impl Term {}

impl TryFrom<Sexpr> for Term {
    type Error = ();

    fn try_from(value: Sexpr) -> Result<Self, Self::Error> {
        match value {
            Sexpr::Sym(x) => Ok(Self::Var(x)),
            Sexpr::List(l) => {
                let mut buff = Buff::new(l);
                let head = buff.next().ok_or(())?;
                if let Some(sym) = head.clone().get_symb_opt() {
                    if sym == *"lam" {
                        let args = buff.expect_convert(Sexpr::get_list_opt).ok_or(())?;
                        todo!()
                    } else if sym == *"tlam" {
                        todo!()
                    } else {
                        Err(())
                    }
                } else {
                    let tail = buff.next().ok_or(())?;
                    let e1: Term = head.try_into()?;
                    let e2: Term = tail.try_into()?;
                    Ok(Term::App(e1.into(), e2.into()))
                }
            }
            Sexpr::Num(_) => panic!(),
        }
    }
    // fn from(s: Sexpr) -> Option<Self> {
    //     match s {
    //         Sexpr::Sym(x) => Self::Var(x),
    //         Sexpr::List(l) => {
    //             let mut buff = Buff::new(l);
    //             let head = buff.next();
    //             todo!()
    //         }
    //         Sexpr::Num(_) => panic!(),
    //     }
    // }
}

#[cfg(test)]
mod Test {
    use super::*;

    #[test]
    fn test1() {
        let t1 = Type::Var("a".to_string());
        assert!(!t1.closed())
    }

    #[test]
    fn test2() {
        use Type::*;
        let t1 = All("a".to_string(), Var("a".to_string()).into());
        assert!(t1.closed())
    }

    #[test]
    fn test3() {
        use Type::*;
        let t1 = Fun(
            All("a".to_string(), Var("a".to_string()).into()).into(),
            Var("a".to_string()).into(),
        );
        assert!(!t1.closed())
    }

    #[test]
    fn test4() {
        let e1 = Term::TLam(
            "a".into(),
            Term::Lam(
                "x".into(),
                Type::Var("a".into()),
                Term::Var("x".into()).into(),
            )
            .into(),
        );
        let t1 = Type::All(
            "a".into(),
            Type::Fun(Type::Var("a".into()).into(), Type::Var("a".into()).into()).into(),
        );
        assert_eq!(e1.typed(), Some(t1))
    }

    #[test]
    fn test5() {
        let e1 = Term::Lam(
            "x".into(),
            Type::Var("a".into()),
            Term::Var("x".into()).into(),
        );
        let e2 = Term::Var("x".into());
        let app = Term::App(e1.into(), e2.into());
        let tapp = Type::Var("a".into());
        let tenv = HashSet::from(["a".into()]);
        let env = HashMap::from([("x".into(), Type::Var("a".into()))]);
        assert_eq!(app.typed_aux(&tenv, &env), Some(tapp))
    }

    #[test]
    fn test6() {
        let ta = Type::Var("a".into());
        let tb = Type::Var("b".into());
        let tf = Type::Fun(ta.clone().into(), tb.clone().into());
        let f = Term::Var("f".into());
        let x = Term::Var("x".into());
        let app = Term::App(f.into(), x.into());
        let tenv = HashSet::from(["a".into(), "b".into()]);
        let env = HashMap::from([("x".into(), ta), ("f".into(), tf)]);
        assert_eq!(app.typed_aux(&tenv, &env), Some(tb))
    }
}
