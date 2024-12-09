use cel_interpreter::{Context, Program};
use rstest::rstest;
use std::fs;
use std::path::PathBuf;

fn check_script(script: &str, expected: &str) {
    let program = Program::compile(script).unwrap();
    print!("{:?} => ", program);
    let context = Context::default();
    let result = program.execute(&context);
    print!("{:?} => ", result);
    assert_eq!(result.unwrap().to_string(), expected.to_string());
}

#[rstest]
fn simple_expressions(#[files("tests/scripts/simple/**/*.cel")] path: PathBuf) {
    let content = fs::read_to_string(path).expect("Unable to read file");
    // then there must be a script
    assert!(!content.is_empty());
    // separate the input and the expected output
    let parts: Vec<&str> = content.split("---").collect();
    // with a script and an answer
    let script = parts[0];
    let answer = parts[1];
    check_script(script, answer.trim());
}
