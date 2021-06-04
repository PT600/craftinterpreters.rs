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
    let code = r##"
            var a = "abc" + "efg";
            print a;
        "##;
    let mut vm = run(code);
    // let val = vm.stack.pop().unwrap();
    // println!("{:?}", val)
}

#[test]
fn function() {
    let code = r#"
            var a = 1;
            fun inc(){
                a = a + 1;
            }
            inc();
            print a;
        "#;
    let vm = run(code);
}

#[test]
fn test_if() {
    run(r#"
            var a = 1;
            if(a > 0){
                a = a + 2;
                print "aaaaa: " + a;
            }
            if(a == 3){
                a = a + 2;
                print "bbbb: " + a;
            }
        "#);
}

#[test]
fn function_call_function() {
    run(r#"
            fun double(i){
                print "start double: " + i;
                var result = i * 2;
                print "in double: " + result;
                return result;
            }
            fun foo(i){
                print "start foo: " + i;
                var result = double(i) * 3;
                print "in foo: " + result;
                return result;
            }
            var a = 10;
            var b = foo(a);
            print b;
        "#);
}
#[test]
fn function_factorial() {
    run(r#"
            var a = 9;
            fun factorial(i){
                if(i==1) return 1;
                var result = i * factorial(i-1);
                print "call foo " + i + ", result";
                return result;
            }
            a = factorial(a);
            print a;
        "#);
}
#[test]
fn function_recursive() {
    run(r#"
            var a = 10;
            fun fib(i){
                print "call fib " + i;
                if(i==0) return 1;
                if(i==1) return 1;
                var result = fib(i-1) + fib(i-2);
                return result;
            }
            a = fib(a);
            print a;
        "#);
}

#[test]
fn scope_fun() {
    run(r#"
    fun outer() {
        var x = "outside";
        fun inner() {
            fun iii(){
                print x;
            }
            iii();
        }
        inner();
      }
      outer();
    "#);
}

#[test]
fn closed_upvalues() {
    run(r#"
      fun outer() {
        var x = "outside";
        fun inner() {
          print x;
        }
        return inner;
      }
      var closure = outer();
      closure();
      closure();
    "#);
}

#[test]
fn closed_upvalues2() {
    run(r#"
    var getter = nil;
    var setter;
    fun outer() {
        var x = "outside";
        fun innerGetter() {
          return x;
        }
        fun innerSetter(newX){
            x = newX;
        }
        getter = innerGetter;
        setter = innerSetter;
    }
    outer();
    print getter();
    setter("newX");
    print getter();
    "#);
}
