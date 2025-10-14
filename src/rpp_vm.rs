use crate::ast::{AstNode, AstOperation, Union};
use std::collections::HashMap;

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
}

pub fn run(node: AstNode) {
    println!("\n\n\n\n\n Running the VM!!! \n\n\n\n\n");
    
    // actually run the vm..... (sounds like a lot of work)
    let mut env = VmEnvironment {
        variables: HashMap::new(),
        line_index: vec![(0, 0)],
        node: &node,
        removed_constructs: vec![],
        line_iteration: 0,
    };
    
    // running the vm ig (any state cloning can happen internally?)
    match env.run() {
        Ok(()) => {
            println!("Exited with status code 0 (success!)");
        },
        Err(e) => {
            // in the vm, any future propagated checks will instead return null rather than crashing allowing better future resolution without constant crashes from time traveling paradoxes
            println!("Exited with Error Status: {:?} at {} -- {}", e.error_type, e.location_name, e.message);
        }
    }
}

// stores the actual data for the vm allowing for state saving, cloning, loading, etc. for the complex future resolution system
#[derive(Clone)]
struct VmEnvironment<'a> {
    // the isize represents the priority
    variables: HashMap<&'a str, Vec<(Value<'a>, Lifetime, Mutability, isize)>>,
    
    // a path to the current scope in the ast, along with the actual operation line index for each corresponding scope
    line_index: Vec<(usize, usize)>,
    
    node: &'a AstNode<'a>,
    
    removed_constructs: Vec<Union<Value<'a>, AstOperation<'a>>>,
    
    line_iteration: usize,
}

impl<'a> VmEnvironment<'a> {
    // runs until a value is found
    fn run_and_search(&self, reference: &'a str) -> Result<(Value<'a>, Lifetime), RuntimeError> {
        self.run_vm(Some(reference))
    }
    
    fn run_vm(&self, reference: Option<&str>) -> Result<(Value<'a>, Lifetime), RuntimeError> {
        Err(RuntimeError {
            message: "Not implemented yet".to_string(),
            location_name: "VmEnvironment::run_and_search".to_string(),
            error_type: RuntimeErrorType::LifetimeAccessViolation,
        })  // todo!
        // TODO! whenever a line is processed, check if it's a debug line, and, if so, change DEBUG to true, otherwise set it to false
    }
    
    // runs the vm until completion or error
    fn run(&mut self) -> Result<(), RuntimeError> {
        match self.run_vm(None) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
    
    // resolves a reference to a non-awaited future value (either negative lifetimes or the next keyword)
    fn resolve_future(&self, variable_reference: &'a str, lifetime: Option<Lifetime>, mutability: Option<Mutability>) -> Result<Value<'a>, RuntimeError> {
        let mut env = self.clone();
        // updating the value to ensure it has some value to run with (null by default)
        let variable = env.variables.get_mut(variable_reference).unwrap();
        if variable.is_empty() {
            variable.push((Value::Null, lifetime.unwrap_or(Lifetime {
                start_time: std::time::Instant::now(),
                start_line_counter: self.line_iteration,
                alive_duration: None,
                alive_lines: None,
                scope: self.line_index.iter().map(|(a, _)| *a).collect(),
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
                        }),
                        false => Ok(Value::Null)
                    }
                }
            },
            Err(err) => match unsafe { DEBUG } {
                true => Err(RuntimeError {
                    message: format!("\n{} -- {:?}", get_random_future_error_text(), err),
                    location_name: "Somewhere in the future".to_string(),
                    error_type: RuntimeErrorType::Unknown,
                }),
                false => Ok(Value::Null)
            }
        }
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
        for (i, (a, _)) in self.line_index.iter().enumerate() {
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
    std::thread::sleep(std::time::Duration::from_millis(1));
    let index = now.elapsed().as_nanos() % examples.len() as u128;
    examples[index as usize]
}

