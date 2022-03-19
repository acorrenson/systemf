use clap::Parser;
use std::{
    fs,
    io::{self, Write},
};
use systemf::{
    buff::Buff,
    sexpr::Sexpr,
    toplevel::{Command, Env},
};

#[derive(Parser)]
struct Cli {
    #[clap(short = 'i', long = "interactive")]
    repl: bool,
    #[clap(parse(from_os_str))]
    path: Option<std::path::PathBuf>,
}

fn main() {
    let cli = Cli::parse();
    let mut env = Env::new();
    if cli.repl {
        loop {
            print!("> ");
            io::stdout().flush().expect("IO error");
            let mut buff = String::new();
            io::stdin().read_line(&mut buff).expect("IO error");
            let cmd = buff
                .parse::<Sexpr>()
                .map_err(|err| format!("Invalid sexpr: {}", err))
                .and_then(|x| Command::try_from(x).map_err(|_| "Invalid Command".into()));
            match cmd {
                Ok(cmd) => cmd.exec(&mut env),
                Err(msg) => println!("{}", msg),
            }
        }
    } else {
        match cli.path {
            Some(p) => {
                let input = fs::read_to_string(&p).expect("Error while reading input file");
                let mut buff = Buff::new(input.chars().collect());
                let prog = Sexpr::parse_list(&mut buff).expect("Invalid sexpr");
                for line in prog {
                    let cmd: Command = line.try_into().expect("Invalid command");
                    cmd.exec(&mut env);
                }
            }
            None => {
                panic!("No input file provided")
            }
        }
    }
}
