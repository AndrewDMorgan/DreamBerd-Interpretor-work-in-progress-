mod tokenizer;

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
    /*for test in LARGE_TOKEN_TESTERS {
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
    
    let text = std::fs::read_to_string("./scripts/test.rpp").expect("Failed to read example.txt");
    let text = text.lines().collect();
    
    tokenize(text);
}
