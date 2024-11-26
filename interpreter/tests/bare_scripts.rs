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
#[case("1 - 1", "0")]
#[case("2024-11-26 - 1d", "2024-11-25T00:00:00.000Z")]
fn atoms(#[case] script: &str, #[case] expected: &str) {
    check_script(script, expected);
}
