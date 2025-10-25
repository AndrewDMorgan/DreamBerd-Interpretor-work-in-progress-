mod tokenizer;
mod ast;
mod rpp_vm;

use crate::ast::generate_embedded_ast;
use crate::tokenizer::{tokenize};

// todo! figure out why in the world on assignment tokens/ast ops the debug flag is not carrying over

fn main() {

    // for the interpreter a clonable environment is necessary to handle future reference resolving
    
    // look how clean this looks! Just please don't actually go into the functions.... unless it seg faults (then just run, it's not worth it)
    // don't use any of that as an actual example of how to program, but rather maybe how not to do certain things
    let text = std::fs::read_to_string("./scripts/test.rpp").expect("Failed to read input file");
    // injecting it into the source code (super simply way, definitely won't cause issues later)
    let std_text = std::fs::read_to_string("./scripts/std.rpp").expect("Failed to read std.rpp (Rust++ Standard Library)");
    let text = vec![text.lines().collect::<Vec<&str>>(), std_text.lines().collect::<Vec<&str>>()].concat();
    let combined_for_debug = text.join("\n");
    std::fs::write("./logs/debug_build.rpp", combined_for_debug).expect("Failed to write to ./logs/debug_build.rpp");
    
    let (tokens, indents) = tokenize(text);
    let node = generate_embedded_ast(tokens, indents);
    rpp_vm::run(node);
}
