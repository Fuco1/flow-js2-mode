;; -*- lexical-binding: t -*-

(require 'flow-js2-test-helpers)

(describe "Type export support"

  (it "should export a regular type"
    (flow-js2-deftest-parse "export type Number = number;"))

  (it "should export an opaque type"
    (flow-js2-deftest-parse "export opaque type Number = number;")))
