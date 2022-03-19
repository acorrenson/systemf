use std::collections::{HashMap, HashSet};

use crate::{
    sexpr::Sexpr,
    term::{Term, Type},
};

pub enum Command {
    Let(String, Term),
    Def(String, Type),
    Check(Term),
    Eval(Term),
    Context,
}

mod command_conv {
    use super::Command;
    use crate::sexpr::Sexpr;

    pub fn get_command(s: Sexpr) -> Option<Command> {
        let l = s.get_list_opt()?;
        match &l[..] {
            [Sexpr::Sym(cmd), Sexpr::Sym(name), body] => {
                if cmd == "Let" {
                    body.clone()
                        .try_into()
                        .ok()
                        .map(|term| Command::Let(name.clone(), term))
                } else if cmd == "Def" {
                    body.clone()
                        .try_into()
                        .ok()
                        .map(|ty| Command::Def(name.clone(), ty))
                } else {
                    None
                }
            }
            [Sexpr::Sym(cmd), body] => {
                if cmd == "Eval" {
                    body.clone().try_into().ok().map(|term| Command::Eval(term))
                } else if cmd == "Check" {
                    body.clone()
                        .try_into()
                        .ok()
                        .map(|term| Command::Check(term))
                } else {
                    None
                }
            }
            [Sexpr::Sym(cmd)] if cmd == "Context" => Some(Command::Context),
            _ => None,
        }
    }
}

impl TryFrom<Sexpr> for Command {
    type Error = ();

    fn try_from(value: Sexpr) -> Result<Self, Self::Error> {
        command_conv::get_command(value).ok_or(())
    }
}

pub struct Env {
    types: HashMap<String, Type>,
    bindings: HashMap<String, Term>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            types: HashMap::from([]),
            bindings: HashMap::from([]),
        }
    }

    pub fn type_vars(&self) -> HashSet<String> {
        self.types.iter().map(|(x, _)| x.clone()).collect()
    }

    pub fn check(&self, t: &Term) -> Option<Type> {
        t.expand_types(&HashSet::from([]), &self.types)
            .typed_aux(&self.type_vars(), &HashMap::from([]))
    }
}

impl Command {
    pub fn exec(self, env: &mut Env) {
        match self {
            Command::Let(x, e) => {
                if let Some(t) = env.check(&e) {
                    env.types.insert(x.clone(), t);
                    env.bindings.insert(x, e);
                } else {
                    println!("Declaration failed: term is ill-typed");
                }
            }
            Command::Def(x, t) => {
                let set = env
                    .types
                    .iter()
                    .map(|(x, _)| x.clone())
                    .collect::<HashSet<String>>();
                if t.closed_aux(&set) {
                    env.types.insert(x, t);
                } else {
                    println!("ill-formed type");
                }
            }
            Command::Check(e) => {
                if let Some(t) = e.typed_aux(&HashSet::from([]), &env.types) {
                    println!("Type check succeeded");
                    println!("_ : {:?}", t)
                } else {
                    println!("Type check failed");
                }
            }
            Command::Eval(_) => todo!(),
            Command::Context => {
                println!("{:?}", env.types);
                println!("{:?}", env.bindings);
            }
        }
    }
}
