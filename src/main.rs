mod parser;
mod interpreter;

fn main() {
    let scanned = parser::scan("($n ($f $x ($u ^x) ($g ($h ^f g h)) n $k ($x ^x) k)) $pred");
    println!("Scanned: {scanned:?}");
}
