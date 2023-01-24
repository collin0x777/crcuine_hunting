use crc32fast;

fn main() {
    let result = crc32fast::hash(b"Hello, world!\n");

    

    println!("{:#08x}", result)
}
