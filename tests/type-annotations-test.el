;; -*- lexical-binding: t -*-

(require 'flow-js2-test-helpers)

(describe "Type annotations support"

  (it "should parse a primitive type annotation on a variable"
    (flow-js2-deftest-parse "var a: number = 1;"))

  (it "should parse a primitive type annotation on a function argument"
    (flow-js2-deftest-parse "var fn = function(x: string) {};")))
