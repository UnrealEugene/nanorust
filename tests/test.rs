use nanorust::ir::InterpretEnv;


#[test]
fn test() {
    let ir = nanorust::ir::example();
    let mut env = InterpretEnv::new();
    println!("Result: {:?}", env.interpret(&ir))
}