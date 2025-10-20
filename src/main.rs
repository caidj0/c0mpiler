use std::fs;

use c0mpiler::utils::test;

fn main() {
    let test_str =
        fs::read_to_string("RCompiler-Testcases/semantic-1/src/basic23/basic23.rx").unwrap();

    match test::run(&test_str) {
        Ok(_) => println!("Semantic check passed!"),
        Err(e) => println!("{e}"),
    }
}
