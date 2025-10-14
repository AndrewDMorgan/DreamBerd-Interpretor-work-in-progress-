
#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Priority (isize),     // the priority ! and count of them
    String   (&'a str),   // any non-identified token (everything is strings therefor everything has to compile)
    Function (&'a str, Vec<&'a str>),   // identified function names
    Compare  (CompareOp, usize),     // the level of comparison (0 for = (just type check), 1 for == (type and whole number)), 2 for === (decimal), 3 for ==== (exact) )
    File     (),          // file identifier (no name as combining happens with the ast generation)
    Export   (&'a str, &'a str),  // function name, file name
    Reverse  (),
    Lifetime (Lifetime),  // a lifetime specifier
    Previous (),
    Next     (),
    Boolean  (Boolean),
    Class    (&'a str),
    New      (),
    Int      (isize),
    Float    (f64),
    Math     (MathOp, usize, usize),  // significance to left/right (number of spaces; less == higher importance)
    FileHeader (&'a str), // the name of the file
    Return   (),
    Delete   (),
    When     (Vec<(Token<'a>, &'a str, usize, usize)>),  // the expression tokens for mutation pattern
    Await    (),
    Async    (),
    Comma    (),
    To       (),
    As       (),
    From     (),
    If       (Vec<(Token<'a>, &'a str, usize, usize)>),  // the expression tokens
    Else     (),
    Debug    (),
    Elif     (Vec<(Token<'a>, &'a str, usize, usize)>),  // the expression tokens
    Import   (&'a str, Option<&'a str>, &'a str),  // function name, optional alias, file name
    Assign   (bool, bool, Option<bool>, Lifetime, &'a str, Option<Vec<(Token<'a>, &'a str, usize, usize)>>, isize),  // is const, is const, is const, name, right side
}

#[derive(Debug, PartialEq, Eq)]
pub enum MathOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Power,
    Inc,
    Dec,
    And,
    Or,
    Xor,
    Not,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompareOp {
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Boolean {
    True,
    False,
    Maybe,  // You can never be too sure
}

#[derive(Debug, PartialEq, Eq, Default)]
pub enum Lifetime {
    #[default] Infinity,  // This means all data leaks.... jk, I don't know anyone called memory leak so it'll be fine
    NegativeInfinity,  // This means all data leaks.... jk, I don't know anyone called memory leak so it'll be fine
    Seconds(isize),
    Lines(isize),
}

pub fn tokenize(text: Vec<&'_ str>) -> (Vec<Vec<(Token<'_>, &'_ str, usize, usize)>>, Vec<usize>) {
    let start = std::time::Instant::now();
    let (text, tokens, indentation) = break_tokens(text);
    let el = start.elapsed();
    println!("Text: {:?}", text);
    println!("\nTokens: {:?}", tokens);
    println!("\nIndents: {:?}\n in {:?}", indentation, el);
    (tokens, indentation)
}

struct PtrSync<T> (T);
unsafe impl<T> Sync for PtrSync<T> {}  // does this do anything? Nope
unsafe impl<T> Send for PtrSync<T> {}
impl<T> PtrSync<T> {
    fn new(t: T) -> Self { PtrSync(t) }  // Why can't you just do PtrSync(t)? idk
    fn lock(&self) -> &T { &self.0 }  // Don't ask why it was called this, idk (used to wrap around mutex, but dropped the mutex, and now a lock that doesn't lock, perfect)
}

static NUM_THREADS: usize = 8;  // more threads is always better, right? What could go wrong

fn break_tokens(text: Vec<&'_ str>) -> (Vec<Vec<&'_ str>>, Vec<Vec<(Token<'_>, &'_ str, usize, usize)>>, Vec<usize>) {
    // why am I using a box? Idk, hopefully it'll ensure the memory address holds up better (it doesn't, this is called denial about bad code)
    let mut indentation = Box::new(vec![]);
    let mut output = Box::new(vec![]);
    let mut tokens = Box::new(vec![]);
    for line in text {
        output.push(vec![line]);
        tokens.push(vec![]);  // woops..... kinda need to actually allocate the array and not just index into it
        indentation.push(0);
    }
    
    // gather n slices
    let avg_count = output.len() / NUM_THREADS;
    let mut slices_index = vec![];
    if avg_count > 0 {
        for i in 0..NUM_THREADS - 1 {
            slices_index.push((i * avg_count, avg_count));
        }
    }
    // is this remotely even? no. But nor is whoever was in office yesterday relative to today (2025 snl. if u know u know)
    if output.len() >= (NUM_THREADS-1)*avg_count || true {
        slices_index.push(((NUM_THREADS-1)*avg_count, output.len() % NUM_THREADS + 1));
    }
    
    // getting the indent direction
    // in other words, finding if the indents are negative (less indent means higher scope) or positive (more indent means higher scope, like python)
    let mut indent_direction = 0;
    get_indent_direction(&mut indent_direction, &output);
    println!("Direction: {}", indent_direction);
    
    // creating the threads to handle the data
    let mut handles = vec![];
    for slice_index in slices_index {  // consuming to allow it to work
        // should live long enough and shouldn't move
        let slice_ptr = PtrSync::new(unsafe {
            // what a great idea
            // unsafe cell or whatever it's called? Never heard of them
            // will this crash? Who's crash? Never heard of that person either
            std::mem::transmute::<&mut Box<Vec<Vec<&str>>>, &'static mut Box<Vec<Vec<&str>>>>(&mut output).as_mut_ptr()
        });
        let tokens_ptr = PtrSync::new(unsafe {
            // what a great idea
            // non-static static data... just the way it was intended
            // anyway, rust-analyzer was working way to well before, and now it keeps crashing!
            std::mem::transmute::<&mut Box<Vec<Vec<(Token, &str, usize, usize)>>>, &'static mut Box<Vec<Vec<(Token, &str, usize, usize)>>>>(&mut tokens).as_mut_ptr()
        });
        let indent_ptr = PtrSync::new(unsafe {
            // what a great idea
            // non-static static data... just the way it was intended
            // anyway, rust-analyzer was working way to well before, and now it keeps crashing!
            std::mem::transmute::<&mut Box<Vec<usize>>, &'static mut Box<Vec<usize>>>(&mut indentation).as_mut_ptr()
        });
        let length = output.len();
        // making sure to add threading because:
        //    1. why not
        //    2. it was running 10x too fast before, so this should fix that by slowing it down
        //    3. it was too easy to read before, so this should make it harder
        //    4. it was far too safe before, so this should ensure lots of undefined behavior and crashes
        handles.push(std::thread::spawn(move || {
            for i in 0..slice_index.1 {
                if slice_index.0 + i >= length { break; }
                // Unsafe is the new safe, copyright 2025©
                let line = unsafe {&mut *(&mut slice_ptr.lock()).add(slice_index.0 + i) };
                let tokens = unsafe {&mut *(&mut tokens_ptr.lock()).add(slice_index.0 + i) };
                let indentation = unsafe {&mut *(&mut indent_ptr.lock()).add(slice_index.0 + i) };
                let whole = &line[0][..];
                split_line(line);
                into_tokens(line, whole, tokens, indentation, indent_direction);
                combine_tokens(tokens);
        } })); }
    for handle in handles {
        match handle.join() {
            Ok(_) => {},
            // should it crash? Of course not. Then it wouldn't compile every time. Can't have bugs when it's hardcoded out (if it compiles it'll run every time!™)
            Err(e) => {
                println!("Thread error: {:?}", e);
                std::thread::sleep(std::time::Duration::from_millis(250));
            }
    } } (*output, *tokens, *indentation)
}

pub fn is_function_ident(token: &str, tokens: &Vec<&str>) -> bool {
    if token.len() > "function".len() || token.is_empty() { return false; }
    // checking if there are any assignment symbols indicating a non-function declaration
    if tokens.iter().any(|x| ["=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "var", "const"].contains(x)) {
        return false;
    }
    
    let mut function = "function";
    for i in 0..token.len() {
        let index = match function.find(&token[i..next_valid_index(i+1, token)]) {
            Some(i) => i,
            None => return false,
        };
        function = &function[index+1..];
    } true
}

fn combine_tokens<'a>(tokens: &mut Vec<(Token<'a>, &'a str, usize, usize)>) {
    let mut index = 0;
    while index < tokens.len() {
        if matches!(tokens[index], (Token::File(), _, _, _)) {
            // consuming the next two (or three if available) tokens
            let text;
            let name;
            if let Some(token) = tokens.get(index + 4) && token.0 == Token::File() {
                text = &tokens[index].1[tokens[index].2..token.2 + token.3];
                name = &tokens[index].1[tokens[index + 1].2..tokens[index + 3].2 + tokens[index + 3].3];
                // use a for loop instead of copy pasting? Nah, for loops are a relic of the past. No real programmer would ever use them (fyi. ignor the lines above. While isn't actually a loop, it's just a conspiracy from the deep state)
                tokens.remove(index + 1);  // name
                tokens.remove(index + 1);  // .
                tokens.remove(index + 1);  // extension
                tokens.remove(index + 1);  // end file header
                //let removed = tokens.remove(index + 2);
            } else {
                match tokens.get(index + 3) {
                    Some(_) => {
                        text = &tokens[index].1[tokens[index].2..tokens[index + 3].2 + tokens[index + 3].3];
                        name = &tokens[index].1[tokens[index + 1].2..tokens[index + 3].2 + tokens[index + 3].3];
                        tokens.remove(index + 1);  // name
                        tokens.remove(index + 1);  // .
                        tokens.remove(index + 1);  // extension
                    },
                    None => {
                        text = "Unnamed";
                        name = "Unnamed";
                    }
                }
            }
            let start_byte = tokens[index].2;
            tokens[index] = (Token::FileHeader(name), text, start_byte, text.len());
        }
        else if index > 0 && matches!((&tokens[index - 1], &tokens[index]), (&(Token::Math(MathOp::Subtract,..),..), (Token::Lifetime(_),..))) {
            index -= 1;
            tokens.remove(index);
            match &mut tokens[index] {
                (Token::Lifetime(lifetime),..) => {
                    *lifetime = match lifetime {
                        Lifetime::Infinity => {
                            Lifetime::NegativeInfinity
                        },
                        Lifetime::NegativeInfinity => {
                            Lifetime::Infinity
                        },
                        Lifetime::Seconds(seconds) => {
                            Lifetime::Seconds(-*seconds)
                        },
                        Lifetime::Lines(lines) => {
                            Lifetime::Lines(-*lines)
                        },
                    };
                },
                _ => {}
            }
        }
        // cleaning up lifetime parameters
        if matches!(tokens[index].0, Token::Lifetime(..)) {
            tokens.remove(index - 1);
            index -= 1;
            tokens.remove(index + 1);
        }
        
        // parse class names
        // parse function names + parameters
        if matches!(tokens[index], (Token::Function(..),..)) && index + 1 < tokens.len() {
            // getting the name and parameters (if any)
            // name is the next parameter, and following that should be any parameters in order
            let name = &tokens[index].1[tokens[index + 1].2..tokens[index + 1].2 + tokens[index + 1].3];
            tokens.remove(index + 1);
            
            let curr_index = index + 1;
            let mut parameters = vec![];
            while curr_index < tokens.len() {
                let token = tokens.remove(curr_index);
                if matches!(token, (Token::String(..),..)) {
                    match token.0 {
                        Token::String(string) => {
                            parameters.push(string);
                        }
                        _ => {}
                    }
                }
            }
            let token = &mut tokens[index];
            match &mut token.0 {
                Token::Function(func_name, params) => {
                    *func_name = name;
                    params.append(&mut parameters);
                },
                _ => {}
            }
        }
        if matches!(tokens[index], (Token::Export(..),..)) && index + 5 < tokens.len() {
            // export function_name to file.name
            let new_tokens = tokens.split_off(index + 1);
            let function_name = &new_tokens[0].1[new_tokens[0].2..new_tokens[0].2 + new_tokens[0].3];
            let file_name = &new_tokens[0].1[new_tokens[2].2..new_tokens[4].2 + new_tokens[4].3];
            match &mut tokens[index].0 {
                Token::Export(function, file) => {
                    *function = function_name;
                    *file = file_name;
                },
                _ => {}
            }
        }
        if matches!(tokens[index], (Token::Import(..),..)) {
            match tokens.len() {
                6 | 7 => {
                    // import function_name from file.name
                    let new_tokens = tokens.split_off(index + 1);
                    let function_name = &new_tokens[0].1[new_tokens[0].2..new_tokens[0].2 + new_tokens[0].3];
                    let file_name = &new_tokens[index].1[new_tokens[2].2..new_tokens[4].2 + new_tokens[4].3];
                    match &mut tokens[index].0 {
                        Token::Import(function, alias, file) => {
                            *function = function_name;
                            *file = file_name;
                            *alias = None;
                        },
                        _ => {}
                    }
                },
                8 | 9 => {
                    // import function_name as new_function_name from file.name
                    let new_tokens = tokens.split_off(index + 1);
                    let function_name = &new_tokens[0].1[new_tokens[0].2..new_tokens[0].2 + new_tokens[0].3];
                    let new_function_name = &new_tokens[2].1[new_tokens[2].2..new_tokens[2].2 + new_tokens[2].3];
                    let file_name = &new_tokens[0].1[new_tokens[4].2..new_tokens[6].2 + new_tokens[6].3];
                    match &mut tokens[index].0 {
                        Token::Import(function, alias, file) => {
                            *function = function_name;
                            *file = file_name;
                            *alias = Some(new_function_name);
                        },
                        _ => {}
                    }
                },
                _ => {},
            }
        }
        index += 1;
    }
    
    // second pass (requires already combined tokens)
    let mut index = 0;
    while index < tokens.len() {
        if index == 0 && matches!(tokens[index].0, Token::String("const" | "var")) {
            // getting the next two or three and the name and taking the right hand
            let is_const = (tokens[index].0 == Token::String("const"), tokens[index + 1].0 == Token::String("const"));
            let optional;
            if matches!(tokens[index + 2].0, Token::String("const" | "var")) {
                optional = Some(tokens[index + 2].0 == Token::String("const"));
                tokens.remove(index + 1);  // removing this one
            } else { optional = None }
            tokens.remove(index + 1);  // second const/var
            // the other one will be replaced with the assign token
            
            // checking for the name
            let name = &tokens[index].1[tokens[index + 1].2..next_valid_index(tokens[index + 1].2 + tokens[index + 1].3, tokens[index].1)];
            tokens.remove(index + 1);
            
            let priority = {
                if matches!(tokens[tokens.len() - 1].0, Token::Priority(..)) {
                    match tokens.remove(tokens.len() - 1).0 {
                        Token::Priority(priority) => priority,
                        _ => 0
                    }
                } else { 0 }
            };
            
            let lifetime = {
                if index + 1 < tokens.len() && matches!(tokens[index + 1].0, Token::Lifetime(..)) {
                    match tokens.remove(index + 1).0 {
                        Token::Lifetime(lifetime) => lifetime,
                        _ => Lifetime::default()
                    }
                } else {
                    Lifetime::default()
                }
            };
            
            let new_token;
            if index + 1 < tokens.len() && tokens[index + 1].0 == Token::Compare(CompareOp::Equal, 0) {
                tokens.remove(index + 1);
                let mut right = vec![];
                while index + 1 < tokens.len() {
                    right.push(tokens.remove(index + 1));
                }
                new_token = Token::Assign(is_const.0, is_const.1, optional, lifetime, name, Some(right), priority);
            } else {
                new_token = Token::Assign(is_const.0, is_const.1, optional, lifetime, name, None, priority);
            }
            tokens[index].0 = new_token;
        }
        else if index == 0 && matches!(tokens[index].0, Token::Class(..)) {
            let name = &tokens[index].1[tokens[index + 1].2..tokens[index + 1].2 + tokens[index + 1].3];
            tokens.remove(index + 1);
            match &mut tokens[index].0 {
                Token::Class(class_name) => {
                    *class_name = name;
                },
                _ => {}
            }
        }
        else if index == 0 && matches!(tokens[index].0, Token::If(..)) {
            let expression_tokens = tokens.split_off(index + 1);
            match &mut tokens[index].0 {
                Token::If(expr) => {
                    *expr = expression_tokens;  // this may need pruning/cleaning up? todo! figure that out
                },
                _ => {}
            }
        }
        else if index == 0 && matches!(tokens[index].0, Token::Elif(..)) {
            let expression_tokens = tokens.split_off(index + 1);
            match &mut tokens[index].0 {
                Token::Elif(expr) => {
                    *expr = expression_tokens;  // this may need pruning/cleaning up? todo! figure that out
                },
                _ => {}
            }
        }
        else if index == 0 && matches!(tokens[index].0, Token::When(..)) {
            let expression_tokens = tokens.split_off(index + 1);
            match &mut tokens[index].0 {
                Token::When(expr) => {
                    *expr = expression_tokens;  // this may need pruning/cleaning up? todo! figure that out
                },
                _ => {}
            }
        }
        
        index += 1;
    }
}

fn get_indent_direction(indent_direction: &mut isize, output: &Box<Vec<Vec<&str>>>) {
    // the trick here is... just look away. Out of sight, out of mind, right?
    // if you don't think about it than it can't hurt you
    // they say if there's no enemy inside the enemy outside can't hurt you. Well, there is definitely an enemy inside, so just don't have one also on the outside
    // *this is what laziness looks like at it's finest. And, I can use the exuse that it's just for a meme language so the bad code is a feature (meme) and not me being bad
    let mut lines = *output.clone();  // cloning is free, just like any store. You just have to bring a gun, and it won't cost anything
    for line in lines.iter_mut() {
        split_line(line);  // I put so much work into the function, so why call it on just one pass and not two?
    }
    let output = lines;
    
    for line in output.iter() {
        let mut line_start = true;
        if !line.iter().any(move |x| {
            let result = PRUNE.contains(x) && line_start;
            line_start = line_start && BLANK_CHARS.contains(x);
            result
        }) {
            if *indent_direction == 0 {
                // checking if there's a syntax indicating an indent (if not, then an indent means negative, otherwise positive)
                let is_indented = is_function_ident(line[0], line) ||
                    ["class", "when", "if", "elif", "else", "{"].contains(&line[0]);
                if is_indented {
                    *indent_direction = 1;
                    println!("Normal");
                    return;
                }
            }
        }
        
        for token in line {
            match *token {
                t if !BLANK_CHARS.contains(&t) => {
                    break;
                },
                " " => {
                    // checking if there's a syntax indicating an indent (if not, then an indent means negative, otherwise positive)
                    let mut line_start_2 = true;
                    let indent_count = line.iter().filter(|x| {
                        let result = BLANK_CHARS.contains(x);
                        line_start_2 = line_start_2 && BLANK_CHARS.contains(x);
                        result && line_start_2
                    }).count();
                    *indent_direction = -(indent_count as isize);
                    println!("Negative");
                    return;
                },
                _ => {}
            }
        }
    }
}

// never nesters? Never heard of never nesters. So never would I never indent. So never would I never nest.
// Never would I never indent nested never nesters.
// This never would never not be called a nested function, right?
fn into_tokens<'a>(line: &mut Vec<&'a str>,
                   text: &'a str,
                   tokens: &mut Vec<(Token<'a>, &'a str, usize, usize)>,
                   indentation: &mut usize,
                   indent_direction: isize,
) {
    // making sure to update unindented lines from negative indent direction
    let mut line_start = true;
    if !line.iter().any(move |x| {
        let result = PRUNE.contains(x) && line_start;
        line_start = line_start && BLANK_CHARS.contains(x);
        result
    }) {
        if indent_direction < 0 {
            *indentation = (-indent_direction) as usize;
        }
    }
    
    let mut byte = 0;
    let mut line_start = true;
    for (index, token) in line.iter().enumerate() {
        byte += token.len();
        let chosen_token = match *token {
            "!" | "¡" => {
                let mut priority = 0;
                for c in &line[index..] {
                    if *c == "!" { priority += 1; }
                    else if *c == "¡" { priority -= 1; }
                    else { break; }
                }
                tokens.push((Token::Priority(priority), text, byte - token.len(), token.len()));
                break;
            },
            "//" => { break; },
            t if is_function_ident(t, &*line) && line_start => {
                Token::Function(*token, vec![])
            },
            t if PRUNE.contains(&t) => {
                if line_start {
                    if t == " " {
                        if indent_direction > 0 {
                            *indentation += indent_direction as usize;
                        } else if indent_direction < 0 {
                            if *indentation == 0{
                                *indentation = ((-indent_direction) as usize).saturating_sub(1);
                            } else {
                                *indentation = indentation.saturating_sub(1);
                            }
                        }
                    }
                }
                continue;
            },
            
            "delete" => { Token::Delete() },
            
            // is there a simple pattern to follow? Yes, but I'm friends with command + c and not with reasoning and logic
            "=" => { Token::Compare(CompareOp::Equal, 0) },
            "==" => { Token::Compare(CompareOp::Equal, 1) },
            "===" => { Token::Compare(CompareOp::Equal, 2) },
            "====" => { Token::Compare(CompareOp::Equal, 3) },
            ";=" => { Token::Compare(CompareOp::NotEqual, 0) },
            ";==" => { Token::Compare(CompareOp::NotEqual, 1) },
            ";===" => { Token::Compare(CompareOp::NotEqual, 2) },
            ";====" => { Token::Compare(CompareOp::NotEqual, 3) },
            
            ">" => { Token::Compare(CompareOp::Greater, 0) },
            ">>" => { Token::Compare(CompareOp::Greater, 1) },
            ">>>" => { Token::Compare(CompareOp::Greater, 2) },
            ">>>>" => { Token::Compare(CompareOp::Greater, 3) },
            ">=" => { Token::Compare(CompareOp::GreaterEqual, 0) },
            ">>=" => { Token::Compare(CompareOp::GreaterEqual, 1) },
            ">>>=" => { Token::Compare(CompareOp::GreaterEqual, 2) },
            ">>>>=" => { Token::Compare(CompareOp::GreaterEqual, 3) },
            
            "<" => { Token::Compare(CompareOp::Less, 0) },
            "<<" => { Token::Compare(CompareOp::Less, 1) },
            "<<<" => { Token::Compare(CompareOp::Less, 2) },
            "<<<<" => { Token::Compare(CompareOp::Less, 3) },
            "<=" => { Token::Compare(CompareOp::LessEqual, 0) },
            "<<=" => { Token::Compare(CompareOp::LessEqual, 1) },
            "<<<=" => { Token::Compare(CompareOp::LessEqual, 2) },
            "<<<<=" => { Token::Compare(CompareOp::LessEqual, 3) },
            
            "true" => { Token::Boolean(Boolean::True) },
            "false" => { Token::Boolean(Boolean::False) },
            "maybe" => { Token::Boolean(Boolean::Maybe) },  // maybe so, maybe not. Maybe it's maybe, maybe it's none. Maybe maybe. So maybe is the maybe maybe
            
            "+" => { Token::Math(MathOp::Add, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "-" => { Token::Math(MathOp::Subtract, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "*" => { Token::Math(MathOp::Multiply, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "/" => { Token::Math(MathOp::Divide, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "%" => { Token::Math(MathOp::Modulus, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "^" => { Token::Math(MathOp::Power, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "+=" => { Token::Math(MathOp::Add, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "-=" => { Token::Math(MathOp::Subtract, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "*=" => { Token::Math(MathOp::Multiply, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "/=" => { Token::Math(MathOp::Divide, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "%=" => { Token::Math(MathOp::Modulus, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "&&" => { Token::Math(MathOp::And, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "||" => { Token::Math(MathOp::Or, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "^^" => { Token::Math(MathOp::Xor, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "++" => { Token::Math(MathOp::Inc, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            "--" => { Token::Math(MathOp::Dec, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            ";" => { Token::Math(MathOp::Not, get_significance(-1, &line, index), get_significance(1, &line, index)) },
            
            "new" => { Token::New() },
            "class" => { Token::Class("") },
            "=======" => {
                *indentation = 0;  // new file, new indentation
                Token::File()
            },
            "export" => { Token::Export("", "") },
            "import" => { Token::Import("", None, "") },
            "from" => { Token::From() },
            "to" => { Token::To() },
            "as" => { Token::As() },
            "reverse" => { Token::Reverse() },
            "previous" => { Token::Previous() },
            "next" => { Token::Next() },
            "return" => { Token::Return() },
            
            "when" => { Token::When(vec![]) },
            "await" => { Token::Await() },
            "async" => { Token::Async() },
            "," => { Token::Comma() },
            
            "if" => { Token::If(vec![]) },
            "elif" => { Token::Elif(vec![]) },
            "else" => { Token::Else() },
            
            "?" => { Token::Debug() }
            
            // Are there enough nested if else's? No, never
            t if t.parse::<isize>().is_ok() || t[0..next_valid_index(t.len()-1, t)].parse::<isize>().is_ok() => {
                if t.ends_with("s") {
                    Token::Lifetime(Lifetime::Seconds(
                        unsafe { t[0..t.len()-1].parse::<isize>().unwrap_unchecked() }
                    ))
                } else if (line[index.saturating_sub(1)] == "<" ||
                            (!tokens.is_empty() && matches!(tokens[tokens.len() - 1].0, Token::Math(MathOp::Subtract,..)))) &&
                           line.get(index + 1) == Some(&">")
                {
                    Token::Lifetime(Lifetime::Lines(
                        unsafe { t.parse::<isize>().unwrap_unchecked() }
                    ))
                } else {
                    unsafe { Token::Int(t.parse::<isize>().unwrap_unchecked()) }
                }
            },
            t if t.parse::<f64>().is_ok() => {
                // need to sprinkle in some unsafety to ensure things would flow smoothly
                // you can always be too safe
                unsafe { Token::Float(t.parse::<f64>().unwrap_unchecked()) }
            },
            
            // you might be asking where '{' and '}' are.... well it's based on white space like python so they aren't needed even though it's part of the language (makes sense, right?)
            
            // Everything is valid. Why should code be invalid? That just sounds like crashes. It's as simple as just saying "don't crash," duh.
            _ => {
                Token::String(*token)
            }
        };
        line_start = line_start && BLANK_CHARS.contains(token);
        tokens.push((chosen_token, text, byte - token.len(), token.len()));
} }

fn get_significance(direction: isize, line: &Vec<&str>, mut index: usize) -> usize {
    let mut sig = 0;
    while index > 0 && index < line.len() {
        index = (index as isize + direction) as usize;
        let char = line[index];
        if char == " " { sig += 1; }
        else { break; }
    } sig
}

fn next_valid_index(mut index: usize, text: &str) -> usize {
    while index < text.len() && !text.is_char_boundary(index) {
        index += 1;
    } index
}

fn last_valid_index(index: usize, text: &str) -> usize {
    let mut index = index;
    while index > 0 && !text.is_char_boundary(index) {
        index -= 1;
    } index
}

pub fn part_of_large_token(text: &str, char_index: usize) -> bool {
    if text.len() == char_index + 1 {
        if text.len() == 1 { return false; }
        for breaker in BREAKS {
            if breaker.len() == 1 { continue; }
            if breaker.contains(&text) {
                return true;
            }
        } return false;
    }
    if BREAKS.contains(&&text[0..next_valid_index(1, text)]) {
        for breaker in BREAKS {
            if breaker.len() == 1 || breaker.len() - 1 <= char_index { continue; }
            if breaker.contains(&&text[0..next_valid_index(usize::min(breaker.len(), text.len()), &text)]) {
                return true;
    } } }
    for breaker in BREAKS {
        if breaker.len() == 1 { continue; }
        if breaker.contains(&&text[char_index..]) {
            return true;
    } } false
}

fn split_line(line: &mut Vec<&str>) {
    let mut i = 0;
    let mut char_index = 0;
    loop {
        //println!("\n\n\n{:?}", &line);
        //std::thread::sleep(std::time::Duration::from_millis(200));
        if char_index >= line[i].len() {
            char_index = 0;
            i += 1;
            if i >= line.len() { break; }
        }
        
        if &line[i][char_index..next_valid_index(char_index + 1, line[i])] == "¡" {
            let next = next_valid_index(char_index + 1, line[i]);
            let text_seg = line.remove(i);
            //println!("Tripled from single breaker:\n * {}\n * {}\n * {}", &text_seg[char_index+1..], &text_seg[char_index..char_index+1], &text_seg[..char_index]);
            line.insert(i, &text_seg[next..]);  // the end bit
            line.insert(i, &text_seg[char_index..next]);  // the end bit
            line.insert(i, &text_seg[..char_index]);  // the start
            i += 2;  // the previous token is completed
            char_index = 0;  // restart the check for this new token
        }
        else if BREAKS.contains(&&line[i][0..char_index]) && !part_of_large_token(line[i], last_valid_index(char_index.saturating_sub(1), line[i])) {
            let text_seg = line.remove(i);
            //println!("Whole breaker:\n * {}\n * {}", &text_seg[..char_index], &text_seg[char_index..]);
            line.insert(i, &text_seg[char_index..]);
            line.insert(i, &text_seg[..char_index]);
            i += 1;  // the previous token is completed
            char_index = 0;  // restart the check for this new token
            continue;
        }
        else if BREAKS.contains(&&line[i][char_index..next_valid_index(char_index+1, line[i])]) {
            // making sure it's not part of a larger token, if so breaking it up
            if part_of_large_token(&line[i], last_valid_index(char_index.saturating_sub(1), line[i])) {
                if char_index == 0 || BREAKS.contains(&&line[i][0..char_index]) {
                    char_index += next_valid_index(char_index + 1, line[i]) - char_index;
                    continue;
                }
                let text_seg = line.remove(i);
                //println!("Large token start:\n * {}\n * {}", &text_seg[char_index..], &text_seg[..char_index]);
                // adding the previous bit allowing the token to be looked at in its entirety
                line.insert(i, &text_seg[char_index..]);
                line.insert(i, &text_seg[..char_index]);
                i += 1;  // the previous token is completed
                char_index = 0;  // restart the check for this new token
                continue;
            }
            
            if part_of_large_token(&line[i][char_index..], 0) {
                let text_seg = line.remove(i);
                //println!("Large token start 2:\n * {}\n * {}", &text_seg[char_index..], &text_seg[..char_index]);
                // adding the previous bit allowing the token to be looked at in its entirety
                line.insert(i, &text_seg[char_index..]);
                line.insert(i, &text_seg[..char_index]);
                i += 1;  // the previous token is completed
                char_index = 0;  // restart the check for this new token
                continue;
            }
            
            if char_index == 0 {
                let text_seg = line.remove(i);
                //println!("Breaker at 0 index:\n * {}\n * {}", &text_seg[0..1], &text_seg[1..]);
                line.insert(i, &text_seg[1..]);
                line.insert(i, &text_seg[0..1]);  // the token
                i += 1;
                char_index = 0;
                continue;
            }
            
            let text_seg = line.remove(i);
            //println!("Tripled from single breaker:\n * {}\n * {}\n * {}", &text_seg[char_index+1..], &text_seg[char_index..char_index+1], &text_seg[..char_index]);
            line.insert(i, &text_seg[char_index+1..]);  // the end bit
            line.insert(i, &text_seg[char_index..char_index+1]);  // the end bit
            line.insert(i, &text_seg[..char_index]);  // the start
            i += 2;  // the previous token is completed
            char_index = 0;  // restart the check for this new token
        }
        char_index += next_valid_index(char_index + 1, line[i]) - char_index;//line[i][].bytes().count();
    }
}

static PRUNE: &[&str] = &[
    "", " ", "\n", "\t", "(", ")", "{", "}"
];

static BLANK_CHARS: &[&str] = &[
    " ",
];

static BREAKS: &[&str] = &[
    " ", "\n", "\t", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "-", "=", "+",
    "[", "]", "{", "}", ";", ":", "'", "\"", ",", "<", ".", ">", "/", "?", "\\", "|",
    "==", "===", "====", "=====", "======", "=======", "+=", "-=", "*=", "/=", "%=", "&&", "||", "++", "--",
    ">", ">>", ">>>", ">>>>", ">=", ">>=", ">>>=", ">>>>=",
    "<", "<<", "<<<", "<<<<", "<=", "<<=", "<<<=", "<<<<=",
    "//", ";=", ";==", ";===", ";====", "^^", "¡"
];
