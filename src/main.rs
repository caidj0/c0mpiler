use std::fs;

use c0mpiler::utils::test;

fn main() {
    let test_str = fs::read_to_string("RCompiler-Testcases/semantic-2/misc59/misc59.rx").unwrap();

    let _ = test::run(&test_str).map_err(|e| println!("{e}"));
}
