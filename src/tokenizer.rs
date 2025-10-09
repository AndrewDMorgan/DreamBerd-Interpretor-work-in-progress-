
pub enum Token {
    Priority (usize),  // the priority ! and count of them
    //
}

pub fn tokenize(text: Vec<&str>) -> Vec<Vec<(Token, &str)>> {
    let start = std::time::Instant::now();
    let text = break_tokens(text);
    let el = start.elapsed().as_micros();
    println!("{:?} in {}", text, el);
    let tokens = vec![];
    
    tokens
}

struct PtrSync<T> (T);
unsafe impl<T> Sync for PtrSync<T> {}
unsafe impl<T> Send for PtrSync<T> {}
impl<T> PtrSync<T> {
    fn new(t: T) -> Self { PtrSync(t) }
    fn lock(&self) -> &T { &self.0 }  // Don't ask why it was called this, idk
}

static NUM_THREADS: usize = 8;  // more threads is always better, right? What could go wrong

fn break_tokens(text: Vec<&str>) -> Vec<Vec<&str>> {
    let mut output = Box::new(vec![]);
    for line in text { output.push(vec![line]); }
    let mut tokens = Box::new(vec![]);
    
    // gather n slices
    let avg_count = output.len() / NUM_THREADS;
    let mut slices_index = vec![];
    for i in 0..NUM_THREADS - 1 {
        let slice = &output[i*avg_count..(i+1)*avg_count];
        slices_index.push((i*avg_count, avg_count));
    }
    let slice = &output[(NUM_THREADS-1)*avg_count..];
    slices_index.push(((NUM_THREADS-1)*avg_count, output.len() - (NUM_THREADS-1)*avg_count));
    
    // creating the threads to handle the data
    let mut handles = vec![];
    for slice_index in slices_index {  // consuming to allow it to work
        // should live long enough and shouldn't move
        let slice_ptr = PtrSync::new(unsafe {
            // what a great idea
            std::mem::transmute::<&mut Box<Vec<Vec<&str>>>, &'static mut Box<Vec<Vec<&str>>>>(&mut output).as_mut_ptr()
        });
        let tokens_ptr = PtrSync::new(unsafe {
            // what a great idea
            std::mem::transmute::<&mut Box<Vec<Vec<Token>>>, &'static mut Box<Vec<Vec<Token>>>>(&mut tokens).as_mut_ptr()
        });
        let length = output.len();
        // making sure to add threading because:
        //    1. why not
        //    2. it was running 10x too fast before, so this should fix that by slowing it down
        //    3. it was too easy to read before, so this should make it harder
        handles.push(std::thread::spawn(move || {
            for i in 0..slice_index.1 {
                if slice_index.0 + i >= length { break; }
                let line = unsafe {&mut *(&mut slice_ptr.lock()).add(slice_index.0 + i) };
                let tokens = unsafe {&mut *(&mut tokens_ptr.lock()).add(slice_index.0 + i) };
                split_line(line);
                into_tokens(line, tokens);
            }
        }));
    }
    for handle in handles {
        match handle.join() {
            Ok(_) => {},
            Err(e) => { println!("Thread error: {:?}", e); }
        }
    } *output
}

fn into_tokens(line: &mut Vec<&str>, tokens: &mut Vec<Token>) {
    //
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
    if BREAKS.contains(&&text[0..1]) {
        for breaker in BREAKS {
            if breaker.len() == 1 || breaker.len() - 1 <= char_index { continue; }
            if breaker.contains(&&text[0..usize::min(breaker.len(), text.len() - 1)]) {
                return true;
            }
        }
    }
    for breaker in BREAKS {
        if breaker.len() == 1 { continue; }
        if breaker.contains(&&text[char_index..usize::min(char_index + text.len(), text.len() - 1)]) {
            return true;
        }
    } false
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
        
        if BREAKS.contains(&&line[i][0..char_index]) && !part_of_large_token(line[i], char_index.saturating_sub(1)) {
            let text_seg = line.remove(i);
            //println!("Whole breaker:\n * {}\n * {}", &text_seg[..char_index], &text_seg[char_index..]);
            line.insert(i, &text_seg[char_index..]);
            line.insert(i, &text_seg[..char_index]);
            i += 1;  // the previous token is completed
            char_index = 0;  // restart the check for this new token
            continue;
        }
        else if BREAKS.contains(&&line[i][char_index..char_index+1]) {
            // making sure it's not part of a larger token, if so breaking it up
            if part_of_large_token(&line[i], char_index.saturating_sub(1)) {
                if char_index == 0 || BREAKS.contains(&&line[i][0..char_index]) {
                    char_index += 1;
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
            //char_index -= 1;  // the previous token is completed
            line.insert(i, &text_seg[char_index+1..]);  // the end bit
            line.insert(i, &text_seg[char_index..char_index+1]);  // the end bit
            line.insert(i, &text_seg[..char_index]);  // the start
            i += 2;  // the previous token is completed
            char_index = 0;  // restart the check for this new token
        }
        char_index += 1;
    }
}

static BREAKS: &[&str] = &[
    " ", "\n", "\t", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "-", "_", "=", "+",
    "[", "]", "{", "}", ";", ":", "'", "\"", ",", "<", ".", ">", "/", "?", "\\", "|",
    "==", "===", "====", "=====", "======", "=======", "+=", "-=", "*=", "/=", "%=", "&&", "||", "++", "--",
    "//", "!="
];
