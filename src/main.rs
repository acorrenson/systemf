use std::{env, fs};

use systemf::{sexpr::Sexpr, term::Term};

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = fs::read_to_string(args[1].clone())
        .map_err(|err| format!("{}", err))
        .expect(format!("error while reading file \"{}\"", args[1]).as_str());
    let prog = input.parse::<Sexpr>().expect("error while parsing sexp");
    let prog: Term = prog.try_into().expect("error while parsing sexp");
    if let Some(ty) = prog.typed() {
        println!("checked at type {:?}", ty);
    } else {
        println!("type check failed");
    }
}
