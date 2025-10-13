mod tokenizer;
mod ast;
mod rpp_vm;

use crate::ast::generate_embedded_ast;
use crate::tokenizer::{is_function_ident, tokenize};


static LARGE_TOKEN_TESTERS: &[&str] = &[
    "=", "==", "===", "====", "=====", "======", "=======", "========",
    "t +=", "+= t!", "world!="
];

static FUNCTION_NAME_TESTS: &[&str] = &[
    "function", "fn", "fctn", "func", "ft", "flt", "funct", "fun", "f",
    "functionality", "functional", "functio", "functi", "functiooooon",
    "fnction", "fntion", "fntn", "fnt", "fntooon", "fntoooon",
    "fctnality", "fctnallity", "fctio", "fctiooooon", "fctnion",
    "funcn", "funcnt", "funcntion", "funcnoon", "funcnooon",
    "ftn", "ftnoon", "ftnooon", "ftnality", "ftnallity", "ftnion",
];

fn main() {
    /*
    // unit tests? What are those? A specific file for unit tests? What's a file?
    (from debugging, but of course I commented it out as regression testing is for losers. I'm not putting millions on the line so it's fine)
    for test in LARGE_TOKEN_TESTERS {
        println!("Testing: '{}'", test);
        for i in 0..test.len() {
            let result = tokenizer::part_of_large_token(test, i);
            println!("  Char Index {}: {}\n -- {}", i, result, &test[..]);
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    
    std::thread::sleep(std::time::Duration::from_millis(500));
    
    for test in FUNCTION_NAME_TESTS {
        println!("Testing: '{}'", test);
        let result = is_function_ident(test);
        println!("  Result: {}\n -- {}", result, &test[..]);
    }
    std::thread::sleep(std::time::Duration::from_millis(500));*/
    
    // for the interpreter a clonable environment is necessary to handle future reference resolving
    
    // look how clean this looks! Just please don't actually go into the functions.... unless it seg faults (then just run, it's not worth it)
    // don't use any of that as an actual example of how to program, but rather maybe how not to do certain things
    let text = std::fs::read_to_string("./scripts/test.rpp").expect("Failed to read example.txt");
    let text = text.lines().collect();
    let (tokens, indents) = tokenize(text);
    let node = generate_embedded_ast(tokens, indents);
    rpp_vm::run(node);
    
    //
}
