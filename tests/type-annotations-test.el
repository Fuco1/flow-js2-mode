;; -*- lexical-binding: t -*-

(require 'flow-js2-test-helpers)

(describe "Type annotations support"

  (it "should parse a primitive type annotation on a variable"
    (flow-js2-deftest-parse "var a: number = 1;"))

  (it "should parse a primitive type annotation on a function argument"
    (flow-js2-deftest-parse "var fn = function(x: string) {};"))

  (it "should not parse a ternary expression assigned to a variable as an optional type signature"
    (flow-js2-deftest-parse "const foo = {bar: 1};
const a = foo.bar ? \"1\" : null;"))

  )
