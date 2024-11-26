use cel_interpreter::{Context, Program};
use rstest::rstest;

fn check_script(script: &str, expected: &str) {
    let program = Program::compile(script).unwrap();
    let context = Context::default();
    let result = program.execute(&context);
    assert_eq!(result.unwrap().to_string(), expected.to_string());
}

#[rstest]
#[case("1 + 1", "2")]
fn atoms(#[case] script: &str, #[case] expected: &str) {
    check_script(script, expected);
}
