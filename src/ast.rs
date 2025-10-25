use crate::tokenizer::{self, Token};
use std::fmt::Display;

// I could have default implemented.... or I could resort to null pointers..... hmmmm..... Jk, I'm not a masochist
#[derive(Default, Debug, Clone, PartialEq)]
pub struct AstNode<'a> {
    pub children_scopes: Vec<AstNode<'a>>,
    pub operations: Vec<(AstOperation<'a>, Option<bool>, usize)>,  // the option bool is for if the line is a debug line, the usize is the line number
    pub ordering: Vec<Union<usize, usize>>,  // left -> index to next ast op, right -> index to next child
    pub base_line_index: usize,
    pub name: String,
    pub context: Option<Box<AstOperation<'a>>>,
}

impl<'a> AstNode<'a> {
    pub fn get_child(&self, index: usize) -> Option<&AstNode<'a>> {
        self.children_scopes.get(index)
    }
    
    pub fn get_child_mut(&mut self, index: usize) -> Option<&mut AstNode<'a>> {
        self.children_scopes.get_mut(index)
    }
    
    pub fn get_child_recursive(&self, index: &[usize]) -> Option<&AstNode<'a>> {
        if index.is_empty() { return Some(self); }
        match self.children_scopes.get(index.len()) {
            Some(child) => child.get_child_recursive(&index[1..]),
            None => None
        }
    }
    
    pub fn get_child_recursive_mut(&mut self, index: &[usize]) -> Option<&mut AstNode<'a>> {
        if index.is_empty() { return Some(self); }
        match self.children_scopes.get_mut(index.len()) {
            Some(child) => child.get_child_recursive_mut(&index[1..]),
            None => None
        }
    }
    
    pub fn get_text(&self, depth: usize, output: &mut Vec<String>) -> Vec<String> {
        let mut text = vec![];
        for (index, ordered) in self.ordering.iter().enumerate() {
            match ordered {
                Union::Left(op_index) => {
                    let debug = match self.operations[*op_index].1 {
                        Some(true) => "]",
                        _ => ">"
                    };
                    output.push(format!("{}{}{:?}", match self.ordering.get(index + 1) {
                        Some(Union::Right(new_scope)) => format!("{}{:4}:", debug, self.children_scopes[*new_scope].base_line_index + 1),
                        _ => match ordered {
                            Union::Left(op_index) => format!("{}{:4}:", debug, self.operations[*op_index].2 + 1),
                            _ => String::from("     :"),
                        },
                    }, "   ".repeat(depth), &self.operations[*op_index].0));
                    text.push(format!("{}{}{}", match self.ordering.get(index + 1) {
                        Some(Union::Right(new_scope)) => format!("{}{:4}:", debug, self.children_scopes[*new_scope].base_line_index + 1),
                        _ => match ordered {
                            Union::Left(op_index) => format!("{}{:4}:", debug, self.operations[*op_index].2 + 1),
                            _ => String::from("     :"),
                        },
                    }, "   ".repeat(depth), self.operations[*op_index].0.get_text()));
                },
                Union::Right(child_index) => {
                    text.append(&mut self.children_scopes[*child_index].get_text(depth + 1, output));
                }
            }
        } text
    }
}

impl Display for AstNode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = self.get_text(1, &mut vec![]);
        
        write!(f, "{}", text.join("\n"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstOperation<'a> {
    ScopeChange (usize),  // the local index to the new scope for the node
    Expr        (Expression<'a>),  // a mathematical expression
    Assignment  (Variable<'a>, Option<Union<Expression<'a>, Value<'a>>>, bool),  // variable name and expression for resolution of the value at runtime, if it's a reassignment or allocation
    None,
    Function    (&'a str, Vec<usize>, Vec<&'a str>),  // name, index, parameters
    // todo! remember in the vm to make sure any called functions are of the same scope or lower when defined (same for classes)
    FunctionCall(&'a str, Vec<usize>, Vec<&'a str>),  // name, index, parameters
    Closure     (AstNode<'a>),  // the closure node (will require at least opening brackets to identify) todo! add closures
    Return      (Option<Union<Expression<'a>, Value<'a>>>),
    ScopeDrop   (usize),  // the number of scopes to drop down   todo!
    NoOp        (Vec<(Token<'a>, &'a str, usize, usize)>, usize),
    Context     (Box<AstOperation<'a>>),  // context for the scope (not to be run)
}

impl AstOperation<'_> {
    fn get_var_text(variable: &Variable) -> String {
        match &variable.var_type {
            VariableType::Var(name) => format!("{}", name),
            VariableType::Array(name,..) => format!("{}", name),
            VariableType::Member(name, child) => format!("{}.{}", name, Self::get_var_text(&*child)),
        }
    }
    
    pub fn get_text(&self) -> String {
        match self {
            AstOperation::NoOp(text,..) => {
                text[0].1[text[0].2..text[0].2+text[0].3].to_string()
            },
            AstOperation::Function(name, _index, parameters) => {
                format!("Function '{}' with parameter(s) '{}'", name, parameters.join(", "))
            },
            AstOperation::Assignment(var, optional_expression_or_const, is_reassignment) => {
                let var_text = Self::get_var_text(var);
                match optional_expression_or_const {
                    Some(Union::Left(expr)) => {format!("Set '{}' to expr {{ {:?} }}", Self::get_var_text(var), expr)},  // todo! (improve formatting for expressions)
                    Some(Union::Right(constant)) => {
                        format!("Set '{}' to {}    ({} pointer to a {} value{}{})", var_text, match constant {
                            Value::Variable(var) => format!("variable '{}'", Self::get_var_text(var)),
                            Value::ConstInt(value) => format!("int '{}'", value),
                            Value::ConstFloat(value) => format!("float '{}'", value),
                            Value::ConstStr(value) => format!("string '{}'", value),
                            Value::FuncCall(..) => format!("'Function call'"),  // todo!
                        }, match var.reassignable.unwrap_or(false) {
                            true => "reassignable",
                            false => "constant",
                        }, match var.mutable.unwrap_or(false) {
                            true => "mutable",
                            false => "constant",
                        }, match is_reassignment {
                            true => "; reassigning",
                            false => "",
                        }, match var.global.unwrap_or(&None) {
                            Some(true) => "; global scope--takes ownership of construct",
                            _ => ""
                        })
                    },
                    _ => {
                        format!("Alloc '{}'", var_text)
                    }
                }
            },
            _ => format!("{:?}", self),
        }
    }
}

// Made this before realizing there's a super unsafe implementation in the standard library
// Sorry, I mean *perfectly safe
#[derive(Debug, Clone, PartialEq)]
pub enum Union<L, R> {
    Left  (L),
    Right (R),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'a> {
    left_op: Union<Box<Expression<'a>>, Value<'a>>,
    operator: ExpressionOp,
    right_op: Union<Box<Expression<'a>>, Value<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    ConstInt (i64),
    ConstFloat (f64),
    Variable (Variable<'a>),
    FuncCall (&'a str, Vec<usize>, Vec<Value<'a>>),  // function name, index, and argument inputs
    ConstStr (&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable<'a> {
    pub var_type: VariableType<'a>,
    pub reassignable: Option<bool>,  // None means the parent contains the info (a reassignment/mutation)
    pub mutable:Option< bool>,  // None means the parent contains the info (a reassignment/mutation)
    // these can be lifetime 'a as 'a comes from main and these parameters come from tokens which are passed from main
    pub global: Option<&'a Option<bool>>,  // None means the parent contains the info (a reassignment/mutation)
    pub lifetime: Option<&'a tokenizer::Lifetime>,  // None means the parent contains the info (a reassignment/mutation)
    pub priority: Option<isize>,  // None means the parent contains the info (a reassignment/mutation)
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableType<'a> {
    Var       (&'a str),
    Array     (&'a str, Box<Variable<'a>>),
    Member    (&'a str, Box<Variable<'a>>),  // base reference following by member name (creates basically a linked list if multiple members are embedded)
}

#[derive(Clone, Debug, Default)]
pub struct AstNodeFileWrapper<'a> {
    pub node: AstNode<'a>,
    pub exporting: Vec<(Vec<usize>, &'a str)>,  // the indexes of the scopes in order to get to the exported function (the final index is the index of the operations instead of tree nodes)
    pub importing: Vec<(&'a str, Option<&'a str>, &'a str, Vec<usize>)>,  // function name, optional alias, file name, index
    pub file_name: &'a str,
}

#[derive(Clone, Copy)]
struct PtrSync<T> (T);
unsafe impl<T> Sync for PtrSync<T> {}  // does this do anything? Nope. The cpu syncs everything, so what's the big deal?
unsafe impl<T> Send for PtrSync<T> {}  // just send it. If I'm shipping a package I don't *need* a package or envelope, just slap on a label and send it
impl<T> PtrSync<T> {
    fn new(t: T) -> Self { PtrSync(t) }  // Why can't you just do PtrSync(t)? idk
    fn lock(self) -> T { self.0 }  // Don't ask why it was called this, idk (used to wrap around mutex, but dropped the mutex, and now a lock that doesn't lock, perfect)
}

// generates an ast with all files embedded based on exports allowing for a natural control flow in emulation/interpretation
pub fn generate_embedded_ast<'a>(tokens: Vec<(Vec<(Token<'a>, &'a str, usize, usize)>, usize)>,
                                 indented: Vec<usize>
) -> Vec<AstNodeFileWrapper<'a>> {
    let start_time = std::time::Instant::now();
    
    let (mut file_tokens, mut file_indents) = break_into_files(tokens, indented);
    println!("File tokens: {:?}", file_tokens);
    println!("File indents: {:?}", file_indents);
    
    let mut files = vec![Box::new(AstNodeFileWrapper::default()); file_tokens.len()];
    let mut handles = vec![];
    for file_index in 0..file_tokens.len() {
        let file_size = file_tokens[file_index].len();
        // pushing the ownership into the files vector to ensure it lives long enough and isn't dropped after the loop
        // placed in a box to doubly verify that when the vector gets reallocated it doesn't move in memory
        let mut file = Box::new(AstNodeFileWrapper {
            node: AstNode::default(),
            exporting: vec![],
            file_name: "unnamed",
            importing: vec![],
        });
        file.node.base_line_index = file_tokens[file_index][0].1;
        file.node.name = match file_tokens[file_index][0].0[0].0 {
            Token::FileHeader(name) => name.to_string(),
            _ => "file root name".to_string(),
        };
        files[file_index] = file;
        // creating the super safe pointers to pass into the threads....
        let token_ptr = PtrSync::new(unsafe {
            // what a great idea
            // non-static static data... just the way it was intended
            // anyway, rust-analyzer was working way to well before, and now it keeps crashing!
            
            // when in doubt, or when the compiler keeps complaining, just make it static!
            // why can't we just call all data static? Just Dreamberd it and treat all lifetimes as infinity, what could ever go wrong?
            std::mem::transmute::<&mut Vec<(Vec<(Token, &str, usize, usize)>, usize)>, &'static mut Vec<(Vec<(Token, &str, usize, usize)>, usize)>>(&mut file_tokens[file_index]).as_mut_ptr()
        });
        let indent_ptr = PtrSync::new(unsafe {
            // what a great idea
            // non-static static data... just the way it was intended
            // anyway, rust-analyzer was working way to well before, and now it keeps crashing!
            
            // *edit: fourth RustRover crash in, maybe I should change this? Nah, jk, I'll never fix it (if it ain't broke don't fix it, and it's still holding on by the duct tape and glue)
            std::mem::transmute::<&mut Vec<usize>, &'static mut Vec<usize>>(&mut file_indents[file_index]).as_ptr()
        });
        
        // Is this safe? What kind of question is that? Are you OSHA or something, cause I don't know anyone called OSHA
        // the only unsafety is replacing files[file_index]... with file_index as *mut... which doesn't always work too great
        let file_ptr = PtrSync::new( {
            // should I convert file to a pointer? Or should I convert a usize to a pointer to AstNodeFileWrapper?
            // The usize seems reasonable, right? The memory should line up just perfectly. It's not like the former is smaller than the ladder...
            &mut *files[file_index] as *mut AstNodeFileWrapper
        });
        let handle = std::thread::spawn(move || {
            // do things here...
            let mut union = Union::Right(file_ptr.lock());
            generate_scoped_ast(token_ptr, indent_ptr, &mut union, 0, file_size, vec![file_index]);
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
    
    for file in &files {
        // identifying all functions
        // todo! identify functions (including imports) and extract any calls into the correct ast syntax node, rather than a blank no-op operation
        let mut functions = vec![];  // a per-file function table
        for imported in &file.importing {
            //FunctionCall (&'b str, Vec<usize>, Vec<&'b str>),  // name, index, parameters
            // todo! trace the function down to it's origin (even through multiple import/exports) until the base index and necessary parameters can be found
            let index: Vec<usize> = vec![];
            let parameters: Vec<&str> = vec![];
            functions.push((imported.1.unwrap_or(imported.0), index, parameters));
        }
    }
    
    let duration = start_time.elapsed();
    for file in &files {
        println!("Generated file: {}\n > {:?}", &file.file_name, &file.node);
    }
    
    println!("AST Generation Time: {:?}", duration);
    
    let mut final_files = vec![];
    for file in files {
        final_files.push(*file);
    }
    
    let mut output = vec![];
    for (index, file) in final_files.iter().enumerate() {
        output.push(format!("{:?}", file));
        //if file.file_name == "main.rpp" || index == final_files.len() - 2 {
        let text = file.node.get_text(1, &mut output);
        println!("Ast Text Formated (file '{}'):\n{}", file.file_name, text.join("\n"));
    }
    std::fs::write("./logs/debug_ast.txt", output.join("\n")).expect("Failed to write to ./logs/debug_ast.txt");
    
    final_files
}

fn handle_imports_and_exports<'a>(
    file: &mut AstNodeFileWrapper<'a>,
    token_ptr: *const (Vec<(Token<'a>, &'a str, usize, usize)>, usize), file_size: usize)
{
    for i in 0..file_size {
        match &unsafe { &(*token_ptr.add(i)).0 }[0] {
            (Token::Import(name, optional_alias, file_add), _text, _start, _size) => {
                // todo! add the actual index to make jump between things much easier (the index needs to link directly to the original function even if it's through a chain of imports)
                file.importing.push((*name, *optional_alias, *file_add, vec![]));
            },
            (Token::Export(name, file_add), _text, _start, _size) => {
                // todo! actually paste the real index possibly change the type to reflect it correctly
                file.exporting.push((vec![], *file_add))
            },
            _ => {}
        }
    }
}

fn generate_scoped_ast<'a>(
    token_ptr: PtrSync<*mut (Vec<(Token<'a>, &'a str, usize, usize)>, usize)>,
    indent_ptr: PtrSync<*const usize>,
    mut node_ptr: &'_ mut Union<AstNode<'a>, *mut AstNodeFileWrapper<'a>>,
    mut i: usize,
    file_size: usize,
    index: Vec<usize>,
) -> usize {
    // check if it's the file wrapper, if so finding all imports and exports
    let current_indent = {
        if i + 1 >= file_size { 0 }
        // grabbing the next line's indentation as any increase on scope will begin on the line before the scope change (such as the function/class def or conditional branch)
        else { unsafe { *indent_ptr.lock().add(i + 1) } }
    };
    let line_number = unsafe { &*token_ptr.lock().add(i) }.1;
    let start_index = i;
    match &mut node_ptr {
        Union::Left(node) => {
            // grabbing the context
            let (ast_line_node, debug,..) = unsafe { get_ast_line(&mut (*token_ptr.lock().add(i)).0, &index, 0, (*token_ptr.lock().add(i)).1) };
            node.context = Some(Box::new(ast_line_node.to_owned()));
            let ast_line_node = AstOperation::Context(Box::new(ast_line_node));
            
            match &mut node_ptr {
                Union::Left(n) => {
                    n.ordering.push(Union::Left(n.operations.len()));
                    n.operations.push((ast_line_node, debug, line_number));
                },
                Union::Right(n) => unsafe {
                    (&mut **n).node.ordering.push(Union::Left((&mut **n).node.operations.len()));
                    (&mut **n).node.operations.push((ast_line_node, debug, line_number));
                },
            }
            i += 1;
        },
        Union::Right(ptr) => {
            let file = unsafe { &mut **ptr };
            handle_imports_and_exports(file, token_ptr.lock(), file_size);
        }
    }
    
    // reading the file header name
    match node_ptr {
        Union::Left(_) => {},
        Union::Right(n) => unsafe { &mut **n }.file_name = {
            match unsafe { &(*token_ptr.lock().add(0)).0 }[0].0 {
                Token::FileHeader(name) => name,
                _ => "Unnamed--Error"
            }
        },
    }
    while i < file_size {
        // Unsafe is the new safe! Copyright 2025 by Rust++ Foundation, all rights reserved.
        // Life is inherently unsafe, so why not accept all that life is?
        // Everything will find a way to segfault at some point, so why not make sure it happens sooner, and causes more damage?
        let mut tokens = unsafe { &mut *token_ptr.lock().add(i) };
        let line_number = unsafe { &*token_ptr.lock().add(i) }.1;
        
        let ast_line_node = get_ast_line(&mut tokens.0, &index, i - start_index, unsafe {
            { &*token_ptr.lock().add(i) }.1
        });
        match &mut node_ptr {
            Union::Left(n) => {
                n.ordering.push(Union::Left(n.operations.len()));
                n.operations.push(ast_line_node);
            },
            Union::Right(n) => unsafe {
                (&mut **n).node.ordering.push(Union::Left((&mut **n).node.operations.len()));
                (&mut **n).node.operations.push(ast_line_node);
            },
        }
        let tokens = unsafe { &*token_ptr.lock().add(i) };
        
        // if scope change, do thingy
        if i + 1 < file_size && current_indent != unsafe { *indent_ptr.lock().add(i + 1) } {
            if current_indent < unsafe { *indent_ptr.lock().add(i + 1) } {
                // create a new scope, calling this function recursively with that index, and waiting for a return
                let mut scoped_node = AstNode::default();
                scoped_node.base_line_index = tokens.1;
                scoped_node.name = match tokens.0.get(1) {
                    Some(v) => v.1,
                    // many operations are already condensed into a single expression token
                    None => match &tokens.0[0].0 {
                        Token::Function(name,..) => name,
                        Token::Class(name) => name,
                        _ => "Unnamed",
                    }
                }.to_string();  // todo!
                let mut union = Union::Left(scoped_node);
                i = generate_scoped_ast(token_ptr, indent_ptr, &mut union, i, file_size, {
                    let mut index = index.clone();
                    index.push(match &mut node_ptr {
                        Union::Left(n) => n.children_scopes.len(),
                        Union::Right(n) => unsafe { &mut **n }.node.children_scopes.len(),
                    });
                    index
                });
                let scoped_node = match union {
                    Union::Left(n) => n,
                    Union::Right(_n) => AstNode::default(),  // this should never be possible anyway
                };
                // adding the new scope
                match &mut node_ptr {
                    Union::Left(n) => {
                        if !matches!(tokens.0[0].0, Token::Function(..)) {
                            n.operations.push((AstOperation::ScopeChange(n.children_scopes.len()), None, line_number));
                        }
                        n.ordering.push(Union::Right(n.children_scopes.len()));
                        n.children_scopes.push(scoped_node);
                    },
                    Union::Right(n) => {
                        let n = unsafe { &mut **n };
                        if !matches!(tokens.0[0].0, Token::Function(..)) {
                            n.node.operations.push((AstOperation::ScopeChange(n.node.children_scopes.len()), None, line_number));
                        }
                        n.node.ordering.push(Union::Right(n.node.children_scopes.len()));
                        n.node.children_scopes.push(scoped_node);
                    }
                }
                continue;
            } else {
                // the end of whatever scope this is
                return i + 1;
            }
        }
        // else: call function to generate ast node and append it
        else {
        }
        i += 1;
    } i
}

// given a single line of tokens, generate the given operation those tokens represent
// any scope data should already be handled, so this should purely worry about the vector of tokens
fn get_ast_line<'a>(
    tokens: &'a Vec<(Token<'a>, &'a str, usize, usize)>,
    index: &Vec<usize>,
    i: usize,
    line_number: usize
) -> (AstOperation<'a>, Option<bool>, usize) {
    let mut debug = tokens.iter().any(|(token,..)| {
        token == &Token::Debug()
    });
    let mut result = match &tokens[0] {
        // How long did they say a line should be at most? It was 150% of a full 4k screen, right?
        (Token::Assign(is_const_1, is_const_2, optional_const, lifetime, name, expr, priority), text, start, size) => {
            if let Some(expr) = expr {
                let mut quotes = 0;
                let mut direction = 1;
                let mut quoted = false;
                debug = expr.iter().any(|(token,..)| {
                    if matches!(token, Token::SingleQ() | Token::DoubleQ()) {
                        quotes += direction;
                        quoted = true;
                    } else if quotes > 0 {
                        direction = -1;
                    }
                    token == &Token::Debug() && (quotes <= 0 || (direction == 1 && quotes == 0) || !quoted)
                });
            }
            
            Some((AstOperation::Assignment(get_variable(*name, *is_const_1, *is_const_2, optional_const, lifetime, *priority), match expr {
                Some(expr) => get_expression(expr),
                None => None
            }, false), Some(debug), line_number))
        },
        (Token::Function(name, parameters), _text, _start, _size) => {
            Some((AstOperation::Function(name, {
                let mut index = index.clone();
                index.push(i);
                index
            }, parameters.clone()), Some(debug), line_number))
        },
        // TODO! add more operations here
        (Token::String(..),..) => { Some((AstOperation::NoOp(tokens.to_owned(), line_number), Some(debug), line_number)) },
        _ => { None }
    };
    match tokens.get(1) {
        Some((Token::Math(op, _sig_left, _sig_right, reassignment),..)) if *reassignment => {
            // todo! handle expressions
            result.replace((AstOperation::Assignment(
                Variable {
                    var_type: VariableType::Var(&tokens[0].1[tokens[0].2..tokens[0].3]),
                    reassignable: None,
                    mutable: None,
                    global: None,
                    lifetime: None,
                    priority: None,
                },
                Some(Union::Left(Expression {
                    left_op: Union::Right(Value::Variable(
                        Variable {
                            var_type: VariableType::Var(&tokens[0].1[tokens[0].2..tokens[0].3]),
                            reassignable: None,
                            mutable: None,
                            global: None,
                            lifetime: None,
                            priority: None,
                    })),
                    operator: ExpressionOp::Add,
                    // handle actual expressions here todo!
                    right_op: Union::Right(
                        match tokens.get(2) {
                            Some((Token::Int(value),..)) => Value::ConstInt(*value as i64),
                            Some((Token::Float(value),..)) => Value::ConstFloat(*value),
                            Some((Token::String(value),..)) => Value::ConstStr(*value),
                            _ => Value::ConstStr("unimplemented"),  // todo!
                    }),
                })),
                true
            ), Some(debug), line_number));
        },
        _ => {}
    }
    result.unwrap_or((AstOperation::None, Some(debug), line_number))
}

fn get_variable<'a>(name: &'a str,
                        reassignable: bool,
                        mutable: bool,
                        global: &'a Option<bool>,
                        lifetime: &'a tokenizer::Lifetime,
                        priority: isize,
) -> Variable<'a> {
    // this will need to not be this at some point
    // add members, arrays, and members or members of...
    Variable { var_type: VariableType::Var(name), reassignable: Some(reassignable), mutable: Some(mutable), global: Some(global), lifetime: Some(lifetime), priority: Some(priority) }  // TODO!
}

fn get_expression<'a>(expr: &'a Vec<(Token<'_>, &'_ str, usize, usize)>) -> Option<Union<Expression<'a>, Value<'a>>> {
    // ideally, the tokenizer already simplified everything such that the type of expression is the first token; additional details may follow or be combined
    // todo!!! make sure the simple constants don't take expressions or constants !! / ? as strings
    let mut quoted = false;
    let end = expr.iter().position(|(token,..)| {
        match token {
            // todo! track this correctly....
            Token::DoubleQ() | Token::SingleQ() => {
                quoted = true;
                false
            },
            Token::Debug() | Token::Priority(..) if !quoted => true,
            _ => false
        }
    }).unwrap_or(expr.len()).saturating_sub(1);
    let expr = &expr[..=end];
    let is_expr = expr.iter().any(|(token,..)| {
        match token {
            // would indicate a larger expression
            Token::Boolean(..) | Token::Math(..) | Token::Compare(..) => true,
            _ => false,
        }
    });
    match &expr[0].0 {
        // checking if it's a simple constant
        Token::Int(int) if !is_expr => {
            Some(Union::Right(Value::ConstInt(*int as i64)))
        },
        Token::Float(float) if !is_expr => {
            Some(Union::Right(Value::ConstFloat(*float)))
        },
        Token::String(string) if !is_expr => {
            Some(Union::Right(Value::ConstStr(&expr[0].1[expr[0].2..])))
        },
        // todo! this somehow got borken
        Token::SingleQ() | Token::DoubleQ() => {
            let mut text = &expr[0].1[expr[0].2..];  // trimming the final element(s) if necessary
            // searching for the starting size
            while ["!", "?", "¡"].contains(&&text[text.len() - 1..]) {
                text = &text[..text.len() - 1];
            }
            let mut num_quotes = 0;
            for c in text.chars() {
                match c {
                    '\'' => num_quotes += 1,
                    '"' => num_quotes += 2,
                    _ => break,
                }
                text = &text[1..];
            }
            // removing as many from the end as possible
            let mut end = text.len();
            while num_quotes > 0 {
                if text.ends_with('\'') {
                    //text = &text[..text.len() - 1];
                    num_quotes -= 1;
                    end -= 1;
                } else if text.ends_with('"') {
                    //text = &text[..text.len() - 1];
                    num_quotes -= 2;
                    end -= 1;
                } else { break; }
            }
            Some(Union::Right(Value::ConstStr(&text[..end])))
        }
        // todo! if await is used, wrap that expression all in the await signifying that:
        //    * instead of looking into the future, store the call and conclude it later once finalized
        //    * such as when doing ```await next variable + 6``` where, the next value of variable is waited for until the expression and subsequently line is completed
        // TODO! add more expression types (such as actual expressions and not just constants)
        _exp => {
            // assuming it's a string    todo! this is borken rn
            Some(Union::Right(Value::ConstStr({
                let end = &expr[end];
                &expr[0].1[expr[0].2..]
                //&expr[0].1[expr[0].2..]
            })))
        }
    }
}

fn break_into_files<'a>(mut tokens: Vec<(Vec<(Token<'a>, &'a str, usize, usize)>, usize)>,
                        mut indented: Vec<usize>
) -> (Vec<Vec<(Vec<(Token<'a>, &'a str, usize, usize)>, usize)>>, Vec<Vec<usize>>) {
    let mut broken_tokens = vec![];
    let mut broken_indents = vec![];
    
    let mut current_tokens = vec![];
    let mut current_indents = vec![];
    while !tokens.is_empty() {
        let token = tokens.remove(0);
        let indent = indented.remove(0);
        if !token.0.is_empty() && matches!(token.0[0].0, Token::FileHeader(..)) {
            if !current_indents.is_empty() {
                broken_indents.push(current_indents);
                broken_tokens.push(current_tokens);
            }
            current_tokens = vec![];
            current_indents = vec![];
        }
        if token.0.is_empty() { continue; }  // clearing blank spaces
        current_indents.push(indent);
        current_tokens.push(token);
    }
    
    if !current_indents.is_empty() {
        broken_indents.push(current_indents);
        broken_tokens.push(current_tokens);
    }
    
    (broken_tokens, broken_indents)
}

