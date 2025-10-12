use crate::tokenizer;

// I could have default implemented.... or I could resort to null pointers..... hmmmm..... Jk, I'm not a masochist
#[derive(Default, Debug)]
pub struct AstNode<'a> {
    children_scopes: Vec<AstNode<'a>>,
    operations: Vec<AstOperation<'a>>,
}

#[derive(Debug)]
pub enum AstOperation<'a> {
    ScopeChange (usize),  // the local index to the new scope for the node
    Expr        (Expression<'a>),  // a mathematical expression
    Assignment  (Variable<'a>, Option<Expression<'a>>),  // variable name and expression for resolution of the value at runtime
    None,
}

// Made this before realizing there's a super unsafe implementation in the standard library
// Sorry, I mean *perfectly safe
#[derive(Debug)]
pub enum Union<L, R> {
    Left  (L),
    Right (R),
}

#[derive(Debug)]
pub struct Expression<'a> {
    left_op: Union<ExpressionOp<'a>, Value<'a>>,
    right_op: Union<ExpressionOp<'a>, Value<'a>>,
}

#[derive(Debug)]
pub enum ExpressionOp<'a> {
    Add (Value<'a>, Value<'a>),
    Sub (Value<'a>, Value<'a>),
    Mul (Value<'a>, Value<'a>),
    Div (Value<'a>, Value<'a>),
    Pow (Value<'a>, Value<'a>),
    Mod (Value<'a>, Value<'a>),
}

#[derive(Debug)]
pub enum Value<'a> {
    ConstInt (i64),
    ConstFloat (f64),
    Variable (Variable<'a>),
    FuncCall (&'a str, Vec<Value<'a>>),  // function name and argument inputs
}

#[derive(Debug)]
pub enum Variable<'a> {
    Var       (&'a str),
    Array     (&'a str, Box<Variable<'a>>),
    Member    (&'a str, Box<Variable<'a>>),  // base reference following by member name (creates basically a linked list if multiple members are embedded)
}

struct AstNodeFileWrapper<'a, 'b> {
    pub node: AstNode<'b>,
    pub exporting: Vec<Vec<usize>>,  // the indexes of the scopes in order to get to the exported function
    pub file_name: &'a str,
}

#[derive(Clone, Copy)]
struct PtrSync<T> (T);
unsafe impl<T> Sync for PtrSync<T> {}  // does this do anything? Nope. The cpu syncs everything, so what's the big deal?
unsafe impl<T> Send for PtrSync<T> {}  // just send it. If I'm shipping a package I don't *need* a package or envelope, just slap on a label and send it
impl<T> PtrSync<T> {
    fn new(t: T) -> Self { PtrSync(t) }  // Why can't you just do PtrSync(t)? idk
    fn lock(&self) -> &T { &self.0 }  // Don't ask why it was called this, idk (used to wrap around mutex, but dropped the mutex, and now a lock that doesn't lock, perfect)
}

// generates an ast with all files embedded based on exports allowing for a natural control flow in emulation/interpretation
pub fn generate_embedded_ast<'a, 'b>(tokens: Vec<Vec<(tokenizer::Token<'a>, &'a str, usize, usize)>>, indented: Vec<usize>) -> AstNode<'b> {
    let start_time = std::time::Instant::now();
    
    let root = AstNode::default();
    let (mut file_tokens, mut file_indents) = break_into_files(tokens, indented);
    println!("File tokens: {:?}", file_tokens);
    println!("File indents: {:?}", file_indents);

    let mut files = vec![];
    let mut handles = vec![];
    for file_index in 0..file_tokens.len() {
        let file_size = file_tokens[file_index].len();
        // pushing the ownership into the files vector to ensure it lives long enough and isn't dropped after the loop
        let file = AstNodeFileWrapper {
            node: AstNode::default(),
            exporting: vec![],
            file_name: "unnamed",
        };
        files.push(file);
        // creating the super safe pointers to pass into the threads....
        let token_ptr = PtrSync::new(unsafe {
            // what a great idea
            // non-static static data... just the way it was intended
            // anyway, rust-analyzer was working way to well before, and now it keeps crashing!
            
            // when in doubt, or when the compiler keeps complaining, just make it static!
            // why can't we just call all data static? Just Dreamberd it and treat all lifetimes as infinity, what could ever go wrong?
            std::mem::transmute::<&mut Vec<Vec<(tokenizer::Token, &str, usize, usize)>>, &'static mut Vec<Vec<(tokenizer::Token, &str, usize, usize)>>>(&mut file_tokens[file_index]).as_mut_ptr()
        });
        let indent_ptr = PtrSync::new(unsafe {
            // what a great idea
            // non-static static data... just the way it was intended
            // anyway, rust-analyzer was working way to well before, and now it keeps crashing!
            
            // *edit: fourth RustRover crash in, maybe I should change this? Nah, jk, I'll never fix it (if it ain't broke don't fix it, and it's still holding on by the duct tape and glue)
            std::mem::transmute::<&mut Vec<usize>, &'static mut Vec<usize>>(&mut file_indents[file_index]).as_mut_ptr()
        });
        
        // Is this safe? What kind of question is that? Are you OSHA or something, cause I don't know anyone called OSHA
        // the only unsafety is replacing files[file_index]... with file_index as *mut... which doesn't always work too great
        let file_ptr = PtrSync::new( {
            // should I convert file to a pointer? Or should I convert a usize to a pointer to AstNodeFileWrapper?
            // The usize seems reasonable, right? The memory should line up just perfectly. It's not like the former is smaller than the ladder...
            &mut files[file_index] as *mut AstNodeFileWrapper
        });
        let handle = std::thread::spawn(move || {
            // do things here...
            let mut union = Union::Right(*file_ptr.lock());
            generate_scoped_ast(token_ptr, indent_ptr, &mut union, 0, file_size);
        });
        handles.push(handle);
    }
    for handle in handles {
        match handle.join() {
            Ok(_) => {},
            // abort on errors? Nah, how about just leave the file null instead?
            Err(e) => {
                println!("Thread panicked while generating the AST (ast.rs): {:?}", e);
                std::thread::sleep(std::time::Duration::from_millis(250));
            },
        }
    }
    
    let duration = start_time.elapsed();
    for file in files {
        println!("Generated file: {}\n > {:?}", file.file_name, file.node);
    }
    
    println!("AST Generation Time: {:?}", duration);
    
    root
}

fn generate_scoped_ast<'a, 'b>(token_ptr: PtrSync<*mut Vec<(tokenizer::Token<'a>, &'a str, usize, usize)>>,
                               indent_ptr: PtrSync<*mut usize>,
                               mut node_ptr: &'_ mut Union<AstNode<'b>, *mut AstNodeFileWrapper<'b, 'b>>,
                               mut i: usize,
                               file_size: usize
) -> usize
    where 'a: 'b  // 'a is the root scope calling this function and the tokenizer ones. 'b is the scope for the entry function that handles
                  // ast generation (1 deeper than main). '_ is just the lifetime for the current instance (some references are only passed forwards
                  // here, and not returned as references, but rather have their ownership returned if at all)
{
    // reading the file header name
    match node_ptr {
        Union::Left(_) => {},
        Union::Right(n) => unsafe { &mut **n }.file_name = {
            match unsafe { &*token_ptr.lock().add(0) }[0].0 {
                tokenizer::Token::FileHeader(name) => name,
                _ => ""
            }
        },
    }
    let current_indent = {
        if i + 1 >= file_size { 0 }
        // grabbing the next line's indentation as any increase on scope will begin on the line before the scope change (such as the function/class def or conditional branch)
        else { unsafe { *indent_ptr.lock().add(i + 1) } }
    };
    while i < file_size {
        // Unsafe is the new safe! Copyright 2025 by Rust++ Foundation, all rights reserved.
        // Life is inherently unsafe, so why not accept all that life is?
        // Everything will find a way to segfault at some point, so why not make sure it happens sooner, and causes more damage?
        let tokens = unsafe { &mut *token_ptr.lock().add(i) };
        
        // if scope change, do thingy
        if i + 1 < file_size && current_indent != unsafe { *indent_ptr.lock().add(i + 1) } {
            if current_indent < unsafe { *indent_ptr.lock().add(i + 1) } {
                // create a new scope, calling this function recursively with that index, and waiting for a return
                let scoped_node = AstNode::default();
                let mut union = Union::Left(scoped_node);
                i = generate_scoped_ast(token_ptr, indent_ptr, &mut union, i, file_size);
                let scoped_node = match union {
                    Union::Left(n) => n,
                    Union::Right(_n) => AstNode::default(),  // this should never be possible anyway
                };
                // adding the new scope
                match &mut node_ptr {
                    Union::Left(n) => n.children_scopes.push(scoped_node),
                    Union::Right(n) => unsafe { &mut **n }.node.children_scopes.push(scoped_node),
                }
                continue;
            } else {
                // the end of whatever scope this is
                return i;
            }
        }
        // else: call function to generate ast node and append it
        else {
            let ast_line_node = get_ast_line(tokens);
            match &mut node_ptr {
                Union::Left(n) => n.operations.push(ast_line_node),
                Union::Right(n) => unsafe {
                    (&mut **n).node.operations.push(ast_line_node)
                },
            }
        }
        i += 1;
    } i
}

// given a single line of tokens, generate the given operation those tokens represent
// any scope data should already be handled, so this should purely worry about the vector of tokens
fn get_ast_line<'a, 'b>(tokens: &'a mut Vec<(tokenizer::Token<'a>, &'a str, usize, usize)>) -> AstOperation<'b>
    where 'a: 'b  // 'a is the root scope calling this function and the tokenizer ones. 'b is the scope for the entry function that handles ast generation (1 deeper than main)
{
    match &tokens[0] {
        // How long did they say a line should be at most? It was 150% of a full 4k screen, right?
        (tokenizer::Token::Assign(is_const_1, is_const_2, optional_const, lifetime, name, expr, priority), text, start, size) => {
            return AstOperation::Assignment(get_variable(*name), match expr {
                Some(expr) => get_expression(expr),
                None => None
            });
        }
        _ => {}
    }
    AstOperation::None
}

fn get_variable(name: &str) -> Variable<'_> {
    Variable::Var(name)
}

fn get_expression<'b>(expr: &Vec<(tokenizer::Token<'_>, &'_ str, usize, usize)>) -> Option<Expression<'b>> {
    None
}

fn break_into_files<'a>(mut tokens: Vec<Vec<(tokenizer::Token<'a>, &'a str, usize, usize)>>,
                        mut indented: Vec<usize>
) -> (Vec<Vec<Vec<(tokenizer::Token<'a>, &'a str, usize, usize)>>>, Vec<Vec<usize>>) {
    let mut broken_tokens = vec![];
    let mut broken_indents = vec![];
    
    let mut current_tokens = vec![];
    let mut current_indents = vec![];
    while !tokens.is_empty() {
        let token = tokens.remove(0);
        let indent = indented.remove(0);
        if !token.is_empty() && matches!(token[0].0, tokenizer::Token::FileHeader(..)) {
            if !current_indents.is_empty() {
                broken_indents.push(current_indents);
                broken_tokens.push(current_tokens);
            }
            current_tokens = vec![];
            current_indents = vec![];
        }
        if token.is_empty() { continue; }  // clearing blank spaces
        current_indents.push(indent);
        current_tokens.push(token);
    }
    
    if !current_indents.is_empty() {
        broken_indents.push(current_indents);
        broken_tokens.push(current_tokens);
    }
    
    (broken_tokens, broken_indents)
}

