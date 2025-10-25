use crate::ast::{AstNode, AstNodeFileWrapper, AstOperation, Union};
use std::collections::HashMap;
use crate::tokenizer::{Token};

static mut DEBUG: bool = false;

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
    Unknown,  // super helpful error, ikr
}

#[derive(Debug)]
struct RuntimeError {
    message: String,
    location_name: String,
    error_type: RuntimeErrorType,
    stack_trace: Vec<String>,
}

fn search_for_functions<'a>(
    functions: &'_ mut HashMap<&'a str, (Vec<usize>, &'a str)>,
    node: &AstNode<'a>,
    scope: Vec<usize>
) {
    // continuing the depth
    for (i, child_node) in node.children_scopes.iter().enumerate() {
        search_for_functions(functions, child_node, {
            let mut scope = scope.clone();
            scope.push(i);
            scope
        })
    }
    
    // searching through the current operations
    for (i, (operation, _debug, _line)) in node.operations.iter().enumerate() {
        if i == 0 { continue; }  // skipping the scope definition operation
        // searching for a function
        match operation {
            AstOperation::Function(name, index, parameters) => {
                println!("\nName: {}\nindex: {:?}\nparams: {:?}\n", name, index, parameters);
            },
            _ => {}
        }
    }
}

fn generate_functions<'a>(node: &'_ Vec<AstNodeFileWrapper<'a>>) -> HashMap<&'a str, (Vec<usize>, &'a str)> {
    let mut functions = HashMap::new();
    // gathering all functions and correct providing paths to make look-ups far easier and more efficent
    for (file_index, file) in node.iter().enumerate() {
        search_for_functions(&mut functions, &file.node, vec![file_index])
    }
    functions
}

fn locate_index_of_main(node: &Vec<AstNodeFileWrapper>) -> usize {
    for (i, file) in node.iter().enumerate() {
        if file.file_name == "main.rpp" {
            return i;
        }
    } node.len() - 2  // the fallback (last file)
}

pub fn run(node: Vec<AstNodeFileWrapper>) {
    println!("\n\n\n\n\n Running the VM!!! \n\n\n\n\nReceived: {:?}\n\n\n\n\n", node);
    
    // actually run the vm..... (sounds like a lot of work)
    let functions = generate_functions(&node);
    let mut env = VmEnvironment {
        variables: {
            let mut hash = HashMap::new();
            // environmental variables
            
            // the std output
            hash.insert("ENV_VAR_LOG_WRITE", vec![(Value::Null, Lifetime {
                start_time: std::time::Instant::now(),
                start_line_counter: 0,
                alive_duration: None,
                alive_lines: None,
                scope: vec![]
            }, Mutability {
                reassignable: false,
                mutable: true,
                global_control: None,
            }, isize::MAX)]);
            hash
        },
        line_index: vec![locate_index_of_main(&node), 0],
        stack_calls: vec![],
        line_indexes_reserved: vec![0],
        node,
        removed_constructs: vec![],
        line_iteration: 0,
        function_idents: functions,
    };
    
    // running the vm ig (any state cloning can happen internally?)
    match env.run() {
        Ok(()) => {
            println!("Exited with status code 0 (success!)");
        },
        Err(e) => {
            // in the vm, any future propagated checks will instead return null rather than crashing allowing better future resolution without constant crashes from time traveling paradoxes
            println!("Exited with Error Status: {:?} at {} -- {}\nStack Trace:\n{}", e.error_type, e.location_name, e.message, e.stack_trace.join("\n"));
        }
    }
}

// stores the actual data for the vm allowing for state saving, cloning, loading, etc. for the complex future resolution system
#[derive(Clone)]
struct VmEnvironment<'a> {
    // the isize represents the priority
    variables: HashMap<&'a str, Vec<(Value<'a>, Lifetime, Mutability, isize)>>,
    
    // a path to the current scope in the ast, along with the actual operation line index for each corresponding scope
    line_index: Vec<usize>,  // the path is absolute, the stack is not (the stack is all visited functions in order, the index is the literal index to the current node)
    stack_calls: Vec<(&'a str, usize)>,  // the stack can be used to resort the correct index when jumping between files
    line_indexes_reserved: Vec<usize>,
    
    node: Vec<AstNodeFileWrapper<'a>>,
    
    removed_constructs: Vec<Union<Value<'a>, AstOperation<'a>>>,
    
    line_iteration: usize,
    
    function_idents: HashMap<&'a str, (Vec<usize>, &'a str)>,
}

impl<'a> VmEnvironment<'a> {
    // runs until a value is found
    fn run_and_search(&mut self, reference: &'a str) -> Result<(Value<'a>, Lifetime), RuntimeError> {
        Ok(self.run_vm(Some(reference))?.ok_or_else(|| RuntimeError {
            message: format!("Variable '{}' not found after execution", reference),
            location_name: "VmEnvironment::run_and_search".to_string(),
            error_type: RuntimeErrorType::VariableNotFound,
            stack_trace: self.get_stack_trace(),
        })?)
    }
    
    fn search_for_raw_node<'b>(node: &'b mut AstNode<'a>, mut index: Vec<usize>) -> Option<&'b mut AstNode<'a>> {
        if index.len() == 1 {
            return Some(node);
        }
        let new_index = index.remove(0);
        Self::search_for_raw_node(match node.children_scopes.get_mut(new_index) {
            Some(n) => n,
            None => return None,
        }, index)
    }
    
    fn search_for_node<'b>(node: &'b mut AstNode<'a>, mut index: Vec<usize>) -> Option<&'b mut (AstOperation<'a>, Option<bool>, usize)> {
        if index.len() == 1 {
            return Some(node.operations.get_mut(index[0])?);
        }
        let new_index = index.remove(0);
        Self::search_for_node( node.children_scopes.get_mut(new_index)?, index)
    }
    
    fn run_vm(&mut self, reference: Option<&'a str>) -> Result<Option<(Value<'a>, Lifetime)>, RuntimeError> {
        let mut add_to_stack = true;
        loop {
            let mut index = self.line_index.clone();
            index.remove(0);  // the base file isn't necessary here?
            // super safe bypass of the ownership model; the memory will be unchanged and live long enough though, so only so unsafe
            let node = unsafe {
                &mut *(
                    match Self::search_for_node(&mut self.node[self.line_index[0]].node, index) {
                        Some(n) => n,
                        None => {
                            // out of range (either pop an index or end if at the root
                            let line_index = self.line_indexes_reserved.pop().unwrap();
                            self.line_index.pop();
                            let i = self.line_index.len() - 1;
                            self.line_index[i] = line_index;
                            self.stack_calls.pop();
                            // purely pointing to a file isn't enough; it needs the file, the scopes node
                            if self.line_index.len() == 1 { break; }
                            continue;
                        }
                    } as *mut (AstOperation<'a>, Option<bool>, usize)
                )
            };
            
            if add_to_stack {
                let index = self.line_index[1..self.line_index.len()].to_vec();
                let node = Self::search_for_raw_node(&mut self.node[self.line_index[0]].node, index).unwrap();
                self.stack_calls.push((node.name, node.base_line_index));
            }
            add_to_stack = false;
            
            // updating the debug state
            if node.1 == Some(true) {
                unsafe { DEBUG = true; }
                println!("\nCall stack:\n{:?}\nOperation:\n{:?}\n", self.stack_calls, node);
                std::thread::sleep(std::time::Duration::from_millis(10));
            }
            else { unsafe { DEBUG = false; } }
            let node = &node.0;  // mutability really wasn't needed, but always good to be safe, and the debug status isn't needed attached to it
            
            // todo! increment the counter properly while accounting for scopes and functions (by pointer, closure, whatever)
            match node {
                AstOperation::ScopeChange(index) => {
                    let i = self.line_index.len() - 1;
                    self.line_indexes_reserved.push(self.line_index[i] + 1);
                    self.line_index[i] = *index;
                    self.line_index.push(1);  // the 1 is to skip the definition of the scope which appears a second time
                    add_to_stack = true;
                },
                _ => {
                    let index = self.line_index.len() - 1;
                    self.line_index[index] += 1;
                }
            }
            self.line_iteration += 1;
            
            // output the standard output if it's not null
            print!("{}", match self.variables.get("ENV_VAR_LOG_WRITE") {
                Some(vec) => match &vec[0].0 {
                    Value::Bool(bool) => match bool {
                        Bool::False => String::from("false"),
                        Bool::True => String::from("true"),
                        Bool::Maybe => String::from("maybe"),
                    },
                    Value::Int(number) => format!("{}", number),
                    Value::Float(number) => format!("{}", number),
                    Value::Str(string) => format!("{}", string),
                    // todo! add support for other data types
                    _ => continue
                },
                _ => continue
            });
            
            // todo! run until the end has been reached, or until the reference was found if it is Some
            // temp todo! do something idk what
        } Ok(None)
    }
    
    // runs the vm until completion or error
    fn run(&mut self) -> Result<(), RuntimeError> {
        match self.run_vm(None) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
    
    // resolves a reference to a non-awaited future value (either negative lifetimes or the next keyword)
    fn resolve_future(&mut self, variable_reference: &'a str, lifetime: Option<Lifetime>, mutability: Option<Mutability>) -> Result<Value<'a>, RuntimeError> {
        let mut env = self.clone();
        // updating the value to ensure it has some value to run with (null by default)
        let variable = env.variables.get_mut(variable_reference).unwrap();
        if variable.is_empty() {
            variable.push((Value::Null, lifetime.unwrap_or(Lifetime {
                start_time: std::time::Instant::now(),
                start_line_counter: self.line_iteration,
                alive_duration: None,
                alive_lines: None,
                scope: self.line_index.iter().enumerate().map(|(i, a)| *a * (i < self.line_index.len()) as usize).collect(),
            }), mutability.unwrap_or(Mutability {
                mutable: false,
                reassignable: false,
                global_control: None,
            }), 0));
        }
        let result = env.run_and_search(variable_reference);
        
        match result {
            Ok((v, l)) => {
                if self.valid_lifetime(&l, -1.) { Ok(v) }
                else { match unsafe { DEBUG } {
                        // debug lines are nice and actually give errors instead of silent nulls
                        true => Err(RuntimeError {
                            message: format!("Variable '{}' accessed outside of its lifetime", variable_reference),
                            location_name: "VmEnvironment::resolve_future".to_string(),
                            error_type: RuntimeErrorType::LifetimeAccessViolation,
                            stack_trace: self.get_stack_trace(),
                        }),
                        false => Ok(Value::Null)
                    }
                }
            },
            Err(err) => match unsafe { DEBUG } {
                true => Err(RuntimeError {
                    message: format!("\n{}\n -- {:?}", get_random_future_error_text(), err),
                    location_name: "Somewhere in the future".to_string(),
                    error_type: RuntimeErrorType::Unknown,
                    stack_trace: self.get_stack_trace(),
                }),
                false => Ok(Value::Null)
            }
        }
    }
    
    fn get_stack_trace(&self) -> Vec<String> {
        /*let mut trace = vec![];
        for scope in &self.stack_calls {
            trace.push(format!("{} on line {}\n", scope.0, scope.1 + 1));
        } trace*/
        self.stack_calls
            .iter()
            .map(|scope| format!(" * {} with a scope starting around line {}\n", scope.0, scope.1 + 1))
            .collect()
    }
    
    // ensures the lifetime is currently valid
    fn valid_lifetime(&self, lifetime: &Lifetime, time_scalar: f64) -> bool {
        if let Some(alive_for) = lifetime.alive_duration {
            if lifetime.start_time.elapsed().as_secs_f64() > alive_for*time_scalar {
                return false;
            }
        }
        if let Some(alive_for) = lifetime.alive_lines {
            if (self.line_iteration as isize - lifetime.start_line_counter as isize) > alive_for {
                if alive_for < 0 && (self.line_iteration as isize - lifetime.start_line_counter as isize) > 0 {
                    return false;
                }
            }
        }
        // checking if the scope is also valid
        for (i, a) in self.line_index.iter().enumerate() {
            if i >= lifetime.scope.len() { break; } // no need to check further
            if *a != lifetime.scope[i] { return false; }
        } true
    }
    
    // gets the highest priority valid reference for a variable
    fn get_prioritized_reference(&self, reference: &'a str) -> Result<usize, RuntimeError> {
        let mut best_priority: Option<isize> = None;
        let mut best_index = None;
        let mut best: Option<&Value<'a>> = None;
        for (index, variable_ref) in self.variables.get(reference).unwrap().iter().enumerate() {
            if best.is_none() || variable_ref.3 > best_priority.unwrap_or(isize::MIN) {
                if !self.valid_lifetime(&variable_ref.1, 1.) { continue; } // skip invalid lifetimes
                best = Some(&variable_ref.0);
                best_index = Some(index);
                best_priority = Some(variable_ref.3);
            }
        }
        best_index.ok_or(RuntimeError {
            message: format!("Variable '{}' not found", reference),
            location_name: "VmEnvironment::get_prioritized_reference".to_string(),
            error_type: RuntimeErrorType::VariableNotFound,
            stack_trace: self.get_stack_trace(),
        })
    }
    
    // checks if a value is globally locked already
    fn is_globally_locked(&self, value: Value<'a>) -> bool {
        false  // todo!
    }
    
    // checks if the value is overloaded
    fn get_overloaded(&self, value_or_property: &'a Union<Value<'a>, AstOperation<'a>>) -> Result<&'a Union<Value<'a>, AstOperation<'a>>, RuntimeError> {
        if self.removed_constructs.contains(&value_or_property) { return Err(RuntimeError {
            message: "Attempted to access a deleted value or property".to_string(),
            location_name: "VmEnvironment::get_overloaded".to_string(),
            error_type: RuntimeErrorType::DeletedValueAccess,
            stack_trace: self.get_stack_trace(),
        }) }
        Ok(&*value_or_property)  // todo!
    }
}

#[derive(Clone, PartialEq)]
struct Mutability {
    mutable: bool,
    reassignable: bool,
    global_control: Option<bool>,  // once a value is assigned, nothing can ever touch it again is my understanding (except through this variable; dropping it means the value is deleted forever)
}

#[derive(Clone, PartialEq)]
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
    
    // this would require launching a second call to the main run function within this file to run (or some other thing, idk; maybe it's all handled within the vm)
    Closure (AstNode<'a>), // name, parameters, index for path to body in base ast node, captured environment
}

#[derive(Clone, PartialEq)]
struct Lifetime {
    start_time: std::time::Instant,
    start_line_counter: usize,
    alive_duration: Option<f64>, // None means forever, float is float in seconds
    alive_lines: Option<isize>, // None means forever
    scope: Vec<usize>,
}

#[derive(Clone, PartialEq)]
enum Bool {
    True,
    False,
    Maybe
}

// Note: Error messages are randomly selected using the spacetime flux.
// Do not attempt to reproduce results. Future selves may vary.

// This should spice up the errors some and ensure nobody ever knows what is going wrong (just like c and c++ programming!!)
fn get_random_future_error_text() -> &'static str {
    let examples = [
        "Temporal hiccup detected: your future self is on vacation",
        "Segmentation paradox at t+∞: self not found, but probably existing somewhere else",
        "Existential error: cannot resolve ‘you’ because tomorrow hasn’t RSVP’d",
        "FutureValueException: value not yet instantiated. Quantum flux interference suspected",
        "Oops… the future whispered 'null' and disappeared",
        "Time loop corrupted: value eaten by recursive owl",
        "Warning: you are attempting to pet a cat that doesn’t exist yet",
        "Error 0xDEADFUTURE: self not yet allocated in spacetime",
        "Flux violation: attempted to dereference tomorrow’s breakfast",
        "Future object missing, but blame is on past-you",
        "Value evaporated during hyper-threaded paradox",
        "Caution: accessing a timeline not approved by HR",
        "Unresolved_future: maybe you, maybe not, probably null",
        "ChronoFail: variable is socially distancing from present",
        "Recursive existential panic: attempted to access your future regrets",
        "Memory leak in spacetime: your future self refuses to answer",
        "UB detected: future-you is busy napping",
        "ValueError: the future has been postponed indefinitely",
        "Warning: timeline mismatch, proceed at your own risk",
        "Temporal overflow: cannot fit future-you in current line",
        "Future not found, possibly abducted by aliens",
        "Quantum ambiguity: variable both exists and does not exist",
        "Attempted to call yourself before first coffee",
        "Recursive doom: your future self filed a restraining order",
        "Paradox exception: value collapses into Schrödinger’s sandwich",
        // back to the future references?
        "Great Scott! Tried to access tomorrow’s value at 88 mph, flux capacitor not engaged",
        "Temporal displacement error: Marty forgot to set the time circuits, value missing",
        "Doc says: ‘Your future self hasn’t invented this yet, wait till 1955’",
    ];
    
    // super random and quality, ik, ik
    // seems random-ish enough
    let now = std::time::Instant::now();
    // plenty of randomizing (sleep calls and time calls require system api calls that can taken hundreds of
    //     ns so this should add up quickly to some nice error utilizing the timing errors built into the
    //     Mac-Os operating system)
    std::thread::sleep(std::time::Duration::from_micros(2));  // this should randomize it plenty more!
    std::thread::sleep(std::time::Duration::from_micros(2));  // this should randomize it plenty more!
    std::thread::sleep(std::time::Duration::from_micros(2));  // this should randomize it plenty more!
    std::thread::sleep(std::time::Duration::from_micros(2));  // this should randomize it plenty more!
    std::thread::sleep(std::time::Duration::from_micros(2));  // this should randomize it plenty more!
    let index = now.elapsed().as_nanos() % examples.len() as u128;
    examples[index as usize]
}

