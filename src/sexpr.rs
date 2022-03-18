use std::{fmt::Display, str::FromStr};

use crate::buff::Buff;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Sexpr {
    Sym(String),
    Num(u32),
    List(Vec<Sexpr>),
}

impl Sexpr {
    pub fn is_symb(&self) -> bool {
        matches!(self, Sexpr::Sym(_))
    }

    pub fn is_num(&self) -> bool {
        matches!(self, Sexpr::Num(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Sexpr::List(_))
    }

    pub fn get_symb(self) -> String {
        if let Sexpr::Sym(s) = self {
            s
        } else {
            panic!()
        }
    }

    pub fn get_symb_opt(self) -> Option<String> {
        if let Sexpr::Sym(s) = self {
            Some(s)
        } else {
            None
        }
    }

    pub fn get_num(self) -> u32 {
        if let Sexpr::Num(s) = self {
            s
        } else {
            panic!()
        }
    }

    pub fn get_num_opt(self) -> Option<u32> {
        if let Sexpr::Num(n) = self {
            Some(n)
        } else {
            None
        }
    }

    pub fn get_list(self) -> Vec<Self> {
        if let Sexpr::List(l) = self {
            l
        } else {
            panic!()
        }
    }

    pub fn get_list_opt(self) -> Option<Vec<Sexpr>> {
        if let Sexpr::List(s) = self {
            Some(s)
        } else {
            None
        }
    }

    pub fn get_singleton_opt(self) -> Option<Sexpr> {
        let l = self.get_list_opt()?;
        if l.len() == 1 {
            Some(l[0].clone())
        } else {
            None
        }
    }

    fn parse_list(buff: &mut Buff<char>) -> Option<Vec<Self>> {
        buff.expect_list(Self::parse)
    }

    fn parse(buff: &mut Buff<char>) -> Option<Self> {
        buff.trim();
        match buff.top()? {
            '0'..='9' => buff.expect_u32().map(Sexpr::Num),
            '(' => {
                buff.pop();
                let list = Self::parse_list(buff)?;
                buff.trim();
                buff.expect(')')?;
                Some(Sexpr::List(list))
            }
            _ => buff.expect_symb().map(Sexpr::Sym),
        }
    }
}

impl FromStr for Sexpr {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Sexpr::parse(&mut Buff::new(s.chars().collect()))
            .ok_or_else(|| "Sexpr: parsing error".to_string())
    }
}

impl Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sexpr::Sym(x) => write!(f, "{}", x),
            Sexpr::Num(n) => write!(f, "{}", n),
            Sexpr::List(l) => {
                let ls = l
                    .iter()
                    .map(|s| format!("{}", s))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "({})", ls)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{sexpr::Sexpr, sexpr::Sexpr::*};

    #[test]
    fn test_1() {
        assert_eq!(
            "(a b c d)".parse::<Sexpr>().unwrap(),
            Sexpr::List(vec![
                Sym("a".to_string()),
                Sym("b".to_string()),
                Sym("c".to_string()),
                Sym("d".to_string())
            ])
        );
    }

    #[test]
    fn test_2() {
        assert_eq!(
            "(1 2 3 4)".parse::<Sexpr>().unwrap(),
            Sexpr::List(vec![Num(1), Num(2), Num(3), Num(4),])
        );
    }

    #[test]
    fn test_3() {
        assert_eq!(
            "(1 a 2 b)".parse::<Sexpr>().unwrap(),
            Sexpr::List(vec![
                Num(1),
                Sym("a".to_string()),
                Num(2),
                Sym("b".to_string())
            ])
        );
    }

    #[test]
    fn test_4() {
        assert!("( )".parse::<Sexpr>().is_err());
    }

    #[test]
    fn test_5() {
        assert!("(!)".parse::<Sexpr>().is_err());
    }

    #[test]
    fn test_6() {
        assert!("(a!)".parse::<Sexpr>().is_err());
    }

    #[test]
    fn test_7() {
        assert!("(1!)".parse::<Sexpr>().is_err());
    }

    #[test]
    fn test_8() {
        assert_eq!(
            "(a1 a2 a3 a4)".parse::<Sexpr>().unwrap(),
            Sexpr::List(vec![
                Sym("a1".to_string()),
                Sym("a2".to_string()),
                Sym("a3".to_string()),
                Sym("a4".to_string()),
            ])
        );
    }

    #[test]
    fn test_9() {
        assert_eq!(
            "(a1 (a2 a3 a4))".parse::<Sexpr>().unwrap(),
            Sexpr::List(vec![
                Sym("a1".to_string()),
                List(vec![
                    Sym("a2".to_string()),
                    Sym("a3".to_string()),
                    Sym("a4".to_string()),
                ])
            ])
        );
    }

    #[test]
    fn test_10() {
        assert_eq!(List(vec![Num(1)]).get_singleton_opt(), Some(Num(1)))
    }
}
