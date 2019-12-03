;; -*- lexical-binding: t -*-

(require 'flow-js2-test-helpers)

(describe "Generics support"

  (it "should parse a generic marker after a function keyword"
    (flow-js2-deftest-parse "function f<T>() {};"))

  (it "should parse a generic marker after a function keyword"
    (flow-js2-deftest-parse "<T>() => {};"))
  )
