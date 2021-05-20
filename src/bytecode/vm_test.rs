    use super::*;

    fn run(source: &str) -> Vm {
        let mut vm = Vm::new();
        let result = vm.interpreter(source);
        assert!(result.is_ok(), "error result: {:?}", result);
        println!("stack: {:?}", vm.stack);
        vm
    }

    fn assert_eq(source: &str, value: Value) {
        let mut vm = run(source);
        assert!(matches!(vm.stack.pop(), Some(value)))
    }

    #[test]
    fn test() {
        // assert_eq("1+1 -2*3", Value::Number(-4.0));
        let nums = vec![1, 2, 3, 1];
        let s = &nums[1..nums.len()];
        let result = s.iter().fold((0, 0), |(prev, prevv), item| {
            (std::cmp::max(prevv + item, prev), prev)
        });
        println!("{:?}", result);
    }
    #[test]
    fn add_str() {
        let mut vm = run("var a = \"abc\" + \"efg\";");
        // let val = vm.stack.pop().unwrap();
        // println!("{:?}", val)
    }