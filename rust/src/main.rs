use crc32fast;
use std::thread;
use std::process;
use rand;

const THREAD_COUNT: u32 = 8;
const STRING: &str = "Data's true form sought\nCRC-32 ensures this haiku's heart\n";

fn format_hash(hash: u32) -> String {
    format!("{:#010x}", hash)
}

// checks if the crc32 hash of X + Y is equal to Y
fn is_crcuine(text: &str, hash: u32) -> bool {
    let new_text = format!("{}{}", text, format_hash(hash));
    let hash2 = crc32fast::hash(new_text.as_bytes());

    return hash == hash2;
}

// scans the range [start, end] for a crcuine hash, prints it and exits the program if it finds one
fn check_range(text: &str, start: u32, end: u32) -> u32 {
    for i in start..=end {
        if is_crcuine(text, i) {
            println!("Found hash: {:#010x}", i);
            process::exit(0);
        }
    }
    return 0;
}

fn split_range(start: u32, end: u32, total: u32, index: u32) -> (u32, u32) {
    let range = end - start;
    let split = range / total;
    let start = start + split * index;

    if index == total - 1 {
        return (start, end);
    } else {
        return (start, start + split - 1);
    }
}

fn main() {
    let threads = 1..=THREAD_COUNT;

    let handles: Vec<thread::JoinHandle<()>> = threads
        .map(|i|
            thread::spawn(move || {                
                let (start, end) = split_range(0, 0xffffffff, THREAD_COUNT, i-1);
                println!("Starting thread {}, checking range [{}, {}]", i, start, end);
                check_range(STRING, start, end);
            })
        ).collect();
    
    for handle in handles {
        handle.join().unwrap();
    }
}
