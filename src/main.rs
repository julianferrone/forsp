mod lexer;

fn main() {
    let scanned = lexer::scan("($n ($f $x ($u ^x) ($g ($h ^f g h)) n $k ($x ^x) k)) $pred");
    println!("Scanned: {scanned:?}");
}
