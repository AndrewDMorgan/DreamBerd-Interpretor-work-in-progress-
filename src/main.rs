mod tokenizer;

use crate::tokenizer::tokenize;


static LARGE_TOKEN_TESTERS: &[&str] = &[
    "=", "==", "===", "====", "=====", "======", "=======", "========",
    "t +=", "+= t!", "world!="
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
    
    std::thread::sleep(std::time::Duration::from_millis(500));*/
    
    let text = vec![
        " ==  Hello,fdf world!= This is a test=== d = == === ==== ===== ====== =======",
    ];
    tokenize(text);
}
