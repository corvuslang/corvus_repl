extern crate corvus_core;
extern crate corvus_standalone;
extern crate rustyline;

use std::cell::RefCell;
use std::rc::Rc;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use rustyline::completion::Completer;

use corvus_standalone::Value;
use corvus_core::{parse, Eval, INamespace, InferredEnv, Namespace, ParseRule, Scope,
                  SharedNamespace, Syntax, Type};

fn main() {
    let mut reader: Editor<Rc<REPL>> = Editor::new();

    let hist_path = std::env::home_dir().map(|path| path.join(".corvus_history"));

    let save_history = |hist: &mut ::rustyline::history::History| {
        if let Some(ref path) = hist_path {
            hist.save(path)
                .unwrap_or_else(|e| panic!("cannot save {:?}: {}", path, e));
        }
    };

    if let Some(ref path) = hist_path {
        let hist = reader.get_history();
        let _ = hist.load(&path);
        save_history(hist);
    }

    let repl: Rc<REPL> = Rc::new(REPL::new());

    reader.set_completer(Some(repl.clone()));

    println!("this is Corvus (press Ctrl-D to exit)");
    println!("");

    loop {
        let mut line = match reader.readline("🐦 ") {
            Ok(line) => line,
            Err(ReadlineError::Eof) => break,
            Err(error) => panic!("{:?}", error),
        };

        if line.trim().is_empty() {
            continue;
        } else {
            let hist = reader.get_history();
            hist.add(&line);
            save_history(hist);
        }

        if !line.starts_with(":") {
            line = format!(":eval {}", line);
        }

        let (cmd, rest) = match line.trim().find(|ch: char| ch.is_whitespace()) {
            Some(pos) => (&line[..pos], line[pos..].trim_left()),
            None => (line.as_str(), ""),
        };

        match cmd {
            ":eval" => {
                repl.eval(rest, true)
                    .map(|(val, ty)| println!("{} ~ {}", val, ty))
                    .unwrap_or_else(|e| println!("{}", e));
            }
            ":type" => match repl.type_of(rest) {
                Ok((stx, ty, globals)) => {
                    println!("{} :: {}", stx, ty);
                    if globals.len() > 0 {
                        println!("\nInferred globals:");
                        for (ref name, ref ty) in globals {
                            println!("{} :: {}", name, ty);
                        }
                    }
                }
                Err(message) => println!("{}", message),
            },
            ":func" => match repl.ns.borrow().get_signature(rest) {
                None => println!("`{}` is not defined", rest),
                Some(sig) => println!("{}", sig),
            },
            ":vars" => {
                let types = repl.as_ref().types.borrow().flatten();
                for (name, val) in repl.as_ref().values.borrow().flatten() {
                    println!("{} = {} :: {}", name, val, types.get(&name).unwrap());
                }
            }
            _ => println!("unknown command: {}", cmd),
        }
    }
}

pub struct REPL {
    ns: SharedNamespace<Value>,
    values: RefCell<Scope<Value>>,
    types: RefCell<Scope<Type>>,
}

impl REPL {
    pub fn new() -> REPL {
        REPL {
            ns: Namespace::new_with_prelude().unwrap().into_shared(),
            values: RefCell::new(Scope::new()),
            types: RefCell::new(Scope::new()),
        }
    }

    pub fn parse(&self, input: &str) -> Result<Syntax, String> {
        parse(&*self.ns.borrow(), ParseRule::term, input).map_err(|e| format!("{}", e))
    }

    pub fn type_of(&self, input: &str) -> Result<(Syntax, Type, InferredEnv), String> {
        self.parse(input).and_then(|stx| {
            use std::iter::empty;
            match corvus_core::type_of(&*self.ns.borrow(), empty(), &stx) {
                Ok((ty, _inferred_env)) => Ok((stx.clone(), ty, _inferred_env)),
                Err(type_errors) => Err(type_errors),
            }
        })
    }

    pub fn eval(&self, input: &str, _safe: bool) -> Result<(Value, Type), String> {
        self.type_of(input).and_then(|(stx, ty, _inferred_env)| {
            let mut values = self.values.borrow_mut();
            stx.eval(&self.ns, &mut values)
                .map(move |val| (val, ty))
                .map_err(|err| format!("runtime error: {}", err))
        })
    }
}

impl Completer for REPL {
    fn complete(&self, line: &str, pos: usize) -> ::rustyline::Result<(usize, Vec<String>)> {
        use std::collections::BTreeSet;
        use std::iter::FromIterator;
        use rustyline::completion::extract_word;

        let break_chars = BTreeSet::from_iter(vec![' ', '\t', '[', '{'].into_iter());
        let (start, word) = extract_word(&line, pos, &break_chars);

        if start == 0 && word.starts_with(":") {
            let matches = vec![":eval", ":type", ":vars", ":func"]
                .into_iter()
                .filter_map(|cmd| {
                    if cmd.starts_with(word) {
                        Some(String::from(cmd))
                    } else {
                        None
                    }
                });
            return Ok((start, matches.collect()));
        }

        let buf: Vec<char> = line.chars().collect();
        let mut is_arg_name = true;
        let mut cursor = start;
        // scan backwards
        while cursor > 0 {
            cursor -= 1;
            match buf[cursor] {
                ':' | '>' | '[' | '{' => {
                    is_arg_name = false;
                    break;
                }
                ' ' | '\t' | '\r' | '\n' => {
                    continue;
                }
                _ => {
                    break;
                }
            }
        }

        if !is_arg_name || cursor == 0 {
            // at beginning of input or following a colon, we complete function names
            let ns = &*self.ns.borrow();
            let completions: Vec<String> = ns.iter()
                .filter_map(|(func_name, _)| {
                    if func_name.starts_with(word) {
                        Some(func_name.clone())
                    } else {
                        None
                    }
                })
                .collect();

            return Ok((cursor, completions));
        }

        // TODO - complete argument names for the current function
        //
        // there was a bunch of shitty code here that didn't work to find the name of the current function
        // it doesn't work because (I think) I need to integrate the actual parser here.
        Ok((pos, Vec::new()))
    }
}
