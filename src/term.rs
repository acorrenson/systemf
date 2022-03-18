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

mod type_conv {
    use super::*;

    pub fn get_type(s: Sexpr) -> Option<Type> {
        match s {
            Sexpr::Sym(x) => Some(Type::Var(x)),
            Sexpr::List(l) => get_all(&l).or_else(|| get_fun(&l)),
            Sexpr::Num(_) => None,
        }
    }

    pub fn get_all(l: &Vec<Sexpr>) -> Option<Type> {
        match &l[..] {
            [Sexpr::Sym(sym), Sexpr::List(vars), body] if sym == "all" => {
                let mut buff = Buff::new(vars.clone());
                let vars = buff.convert_list(Sexpr::get_symb_opt)?;
                buff.expect_end()?;
                let body = get_type(body.clone())?;
                Some(
                    vars.into_iter()
                        .rev()
                        .fold(body, |acc, x| Type::All(x, acc.into())),
                )
            }
            _ => None,
        }
    }

    pub fn get_fun(l: &Vec<Sexpr>) -> Option<Type> {
        let mut buff = Buff::new(l.clone());
        let types = buff.convert_list(get_type)?;
        buff.expect_end()?;
        if types.len() > 0 {
            let head = &types[0];
            let tail = &types[1..];
            Some(tail.into_iter().rfold(head.clone(), |acc, x| {
                Type::Fun(x.clone().into(), acc.into())
            }))
        } else {
            None
        }
    }
}

impl TryFrom<Sexpr> for Type {
    type Error = ();

    fn try_from(value: Sexpr) -> Result<Self, Self::Error> {
        type_conv::get_type(value).ok_or(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub fn typed_aux(&self, tenv: &HashSet<String>, env: &HashMap<String, Type>) -> Option<Type> {
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

mod term_conv {
    use super::*;

    pub fn get_term(s: Sexpr) -> Option<Term> {
        match s {
            Sexpr::Sym(x) => Some(Term::Var(x)),
            Sexpr::List(l) => get_lam(&l).or_else(|| get_tapp(&l)).or_else(|| get_app(&l)),
            Sexpr::Num(_) => None,
        }
    }

    pub fn get_lam(l: &Vec<Sexpr>) -> Option<Term> {
        match &l[..] {
            [Sexpr::Sym(sym), Sexpr::List(args), body] => {
                if sym == "lam" {
                    let args = get_args(args)?;
                    let body = get_term(body.clone())?;
                    Some(
                        args.into_iter()
                            .rev()
                            .fold(body, |acc, (x, tx)| Term::Lam(x, tx, acc.into())),
                    )
                } else if sym == "tlam" {
                    let mut buff = Buff::new(args.clone());
                    let args = buff.convert_list(Sexpr::get_symb_opt)?;
                    let body = get_term(body.clone())?;
                    Some(
                        args.into_iter()
                            .rev()
                            .fold(body, |acc, ty| Term::TLam(ty, acc.into())),
                    )
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn get_tapp(l: &Vec<Sexpr>) -> Option<Term> {
        match &l[..] {
            [Sexpr::Sym(sym), term, ty] if sym == "inst" => {
                let term = get_term(term.clone())?;
                let ty = type_conv::get_type(ty.clone())?;
                Some(Term::TApp(term.into(), ty.into()))
            }
            _ => None,
        }
    }

    fn get_app(l: &Vec<Sexpr>) -> Option<Term> {
        let mut buff = Buff::new(l.clone());
        let terms = buff.convert_list(get_term)?;
        terms
            .into_iter()
            .reduce(|acc, x| Term::App(acc.into(), x.into()))
    }

    fn get_arg(s: Sexpr) -> Option<(String, Type)> {
        let l = s.get_list_opt()?;
        let mut buff = Buff::new(l);
        let var = buff.expect_convert(Sexpr::get_symb_opt)?;
        let ty = buff.expect_convert(type_conv::get_type)?;
        buff.expect_end()?;
        Some((var, ty))
    }

    fn get_args(l: &Vec<Sexpr>) -> Option<Vec<(String, Type)>> {
        let mut buff = Buff::new(l.clone());
        let args = buff.convert_list(get_arg)?;
        buff.expect_end()?;
        Some(args)
    }
}

impl TryFrom<Sexpr> for Term {
    type Error = ();

    fn try_from(value: Sexpr) -> Result<Self, Self::Error> {
        term_conv::get_term(value).ok_or(())
    }
}

#[cfg(test)]
mod test {
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

    #[test]
    fn test7() {
        let inp: Sexpr = "((lam ((x A) (y B)) (x y)) x)".parse().unwrap();
        println!("{:?}", Term::try_from(inp));
    }

    #[test]
    fn test8() {
        let inp: Sexpr = "(lam ((x A)) x)".parse().unwrap();
        let out = Term::try_from(inp);
        let res = Term::Lam(
            "x".into(),
            Type::Var("A".into()),
            Term::Var("x".into()).into(),
        );
        assert_eq!(out, Ok(res));
    }

    #[test]
    fn test9() {
        let inp: Sexpr = "(tlam (A) (lam ((x A)) x))".parse().unwrap();
        let out = Term::try_from(inp);
        let res = Term::TLam(
            "A".into(),
            Term::Lam(
                "x".into(),
                Type::Var("A".into()),
                Term::Var("x".into()).into(),
            )
            .into(),
        );
        assert_eq!(out, Ok(res));
    }

    #[test]
    fn test10() {
        let inp: Sexpr = "(inst (tlam (A) x) B)".parse().unwrap();
        let out = Term::try_from(inp);
        let res = Term::TApp(
            Term::TLam("A".into(), Term::Var("x".into()).into()).into(),
            Type::Var("B".into()),
        );
        assert_eq!(out, Ok(res));
    }
}
