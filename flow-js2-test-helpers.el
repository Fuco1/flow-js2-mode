(require 'js2-mode)
(require 'buttercup)
(require 'flow-js2-mode)

(defun flow-js2-mode--and-parse ()
  (js2-mode)
  (flow-js2-mode 1)
  (js2-reparse))

(defun flow-js2-test-string-to-ast (s)
  (insert s)
  (flow-js2-mode--and-parse)
  (expect js2-mode-buffer-dirty-p :not :to-be-truthy)
  js2-mode-ast)

(cl-defun flow-js2-test-parse-string (code-string &key syntax-error errors-count
                                                  reference warnings-count)
  (with-temp-buffer
    (let ((ast (flow-js2-test-string-to-ast code-string)))
      (if syntax-error
          (let ((errors (js2-ast-root-errors ast)))
            (expect (length errors) :to-equal (or errors-count 1) )
            (cl-destructuring-bind (_ pos len) (car (last errors))
              (expect syntax-error :to-equal (substring code-string (1- pos) (+ pos len -1)))))
        (expect (js2-ast-root-errors ast) :to-be nil)
        (with-temp-buffer
          (js2-print-tree ast)
          (skip-chars-backward " \t\n")
          (expect (buffer-substring-no-properties (point-min) (point))
                  :to-equal (or reference code-string) ))
        (if (= (or warnings-count 0) 0)
            (expect (js2-ast-root-warnings ast) :to-be nil)
          (expect (length (js2-ast-root-warnings ast)) :to-equal (or warnings-count 0)))))))

(cl-defmacro flow-js2-deftest-parse (code-string &key bind syntax-error errors-count
                                                 reference warnings-count)
  "Parse CODE-STRING.  If SYNTAX-ERROR is nil, print syntax tree
with `js2-print-tree' and assert the result to be equal to
REFERENCE, if present, or the original string.  If SYNTAX-ERROR
is passed, expect syntax error highlighting substring equal to
SYNTAX-ERROR value.  BIND defines bindings to apply them around
the test."
  (declare (indent defun))
  `(let ,(append bind '((js2-basic-offset 2)))
     (flow-js2-test-parse-string ,code-string
                                 :syntax-error ,syntax-error
                                 :errors-count ,errors-count
                                 :warnings-count ,warnings-count
                                 :reference ,reference)))

(provide 'flow-js2-test-helpers)
