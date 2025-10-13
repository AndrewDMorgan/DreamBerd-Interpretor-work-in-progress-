use std::collections::HashMap;

// or.... I could just say segfault like c and be lazy and everyone should love it, right?
// if C can get away with it, why not me? It's not laziness, it's just simple plagiarism
// (for my own sanity, I'll avoid that.... for now)
#[derive(Debug)]
enum RuntimeErrorType {
    LifetimeAccessViolation,
    TypeError,
    VariableNotFound,
    FunctionNotFound,
    DeletedValueAccess,
    InvalidOperation,
}

#[derive(Debug)]
struct RuntimeError {
    message: String,
    location_name: String,
    error_type: RuntimeErrorType,
}

pub fn run(node: crate::ast::AstNode) {
    // actually run the vm..... (sounds like a lot of work)
    let mut env = VmEnvironment {
        variables: HashMap::new(),
        line_index: vec![(0, 0)],
    };
}

// stores the actual data for the vm allowing for state saving, cloning, loading, etc. for the complex future resolution system
#[derive(Clone)]
struct VmEnvironment<'a> {
    // the usize represents the priority
    variables: HashMap<&'a str, (Vec<(Value<'a>, Lifetime, Mutability, usize)>)>,
    
    // a path to the current scope in the ast, along with the actual operation line index for each corresponding scope
    line_index: Vec<(usize, usize)>,
}

impl<'a> VmEnvironment<'a> {
    // runs the vm until completion or error
    fn run(&mut self) -> Result<(), RuntimeError> {
        Ok(())
    }
}

#[derive(Clone)]
struct Mutability {
    mutable: bool,
    reassignable: bool,
    global_control: Option<bool>,  // once a value is assigned, nothing can ever touch it again is my understanding (except through this variable; dropping it means the value is deleted forever)
}

#[derive(Clone)]
enum Value<'a> {
    Str (&'a str),
    Int (i64),
    Float (f64),
    Bool (Bool),
    List (Vec<Value<'a>>),
    Instance (&'a str, HashMap<&'a str, Value<'a>>), // class name, internal variables/data/state
    Null,
    
    // todo! should this be here, or pre-resolved in the ast? probably pre-resolved? Unless there can be function pointers? In that case is this just a pointer?
    Function (&'a str, Vec<&'a str>, Vec<usize>), // name, parameters, index for path to body in base ast node
}

#[derive(Clone)]
struct Lifetime {
    start_time: std::time::Instant,
    start_line_counter: usize,
    alive_duration: Option<std::time::Duration>, // None means forever
    alive_lines: Option<usize>, // None means forever
    scope: Vec<usize>,
}

#[derive(Clone)]
enum Bool {
    True,
    False,
    Maybe
}

