(require 'rjsx-mode)
(require 'flow-minor-mode)

(defgroup flow-js2-minor-mode nil
  "Support for flow annotations in JSX files."
  :group 'js2-mode)

;;;###autoload
(define-minor-mode flow-js2-minor-mode
  "Minor mode for editing JS files with flow type annotations."
  :lighter ":FLOW"
  :group 'flow-js2-minor-mode

  ;; Register the primitive types as external identifiers:
  (dolist (kw '("boolean" "number" "string" "null" "void" "any" "mixed"))
    (add-to-list 'js2-additional-externs kw)))

(defun activate-flow-js2-minor-mode ()
  (when (and (flow-minor-tag-present-p)
             ;; (flow-configured-p)
             )
    (flow-js2-minor-mode +1)))

(defvar flow-js2-parsing-typespec-p nil)
(defun flow-js2-create-name-node (orig-fun &rest args)
  (let ((name (apply orig-fun args)))
    (if (and flow-js2-minor-mode
             (or (not flow-js2-parsing-typespec-p)
                 flow-js2-parsing-type-alias-p))
        (apply 'flow-js2-do-create-name-node name args)
      name)))

(advice-add 'js2-create-name-node :around #'flow-js2-create-name-node)

(add-hook 'js2-mode-hook 'activate-flow-js2-minor-mode)

(defun flow-js2-do-create-name-node (name &optional check-activation-p token string)
  (let (generic-type)
    (when (and (or (not flow-js2-parse-object-literal-p)
                   flow-js2-parsing-type-alias-p)
               (or (and (js2-match-token js2-HOOK)
                        (js2-match-token js2-COLON))
                   (js2-match-token js2-COLON)
                   (setq generic-type (js2-match-token js2-LT))))
      (let* ((pos (js2-node-pos name))
             (tt (js2-current-token-type))
             (left name)
             (type-spec (js2-parse-flow-type-spec))
             (len (- (js2-node-end type-spec) pos)))
        (when generic-type
          (js2-match-token js2-GT))
        (js2-set-face pos (js2-node-end name) 'font-lock-variable-name-face 'record)
        (setq name (make-js2-flow-type-annotated-node :pos pos :len len :name name :typespec type-spec))
        (js2-node-add-children name left type-spec))))
  name)


;;; Node types

(cl-defmacro js2-flow-define-node-type ((name (&rest fields) &rest constructor-args)
                                     docstring
                                     (&rest print-function-args) &body print-function-body)
  (declare (indent defun))
  (let ((attributed-sym (intern (format "cl-struct-%s" name)))
        (printer-name (intern (format "print-%s" name))))
    `(progn
       (cl-defstruct (,name (:include js2-node) (:constructor nil)
                            (:constructor ,(intern (format "make-%s" name)) (&key ,@constructor-args)))
         docstring
         ,@fields)
       (put ',attributed-sym 'js2-visitor 'js2-visit-none)
       (put ',attributed-sym 'js2-printer
            (defun ,printer-name (,@print-function-args) ,@print-function-body)))))

;;; Type-annotated variables or other names --- const a: string
(js2-flow-define-node-type (js2-flow-type-annotated-node (name typespec)
                                                     (pos (js2-current-token-beg))
                                                     (len (- js2-ts-cursor
                                                             (js2-current-token-beg)))
                                                     name
                                                     typespec)
  "Represent a name with a flow type annotation. This applies to
variables and function arguments alike." (n i)
  (let* ((tt (js2-node-type n)))
    (js2-print-ast (js2-flow-type-annotated-node-name n) i)
    (insert ": ")
    (js2-print-ast (js2-flow-type-annotated-node-typespec n) 0)))

;;; Combination types --- a | b or a & b
(js2-flow-define-node-type (js2-flow-typespec-combination-node (op left right)
                                                               (pos (js2-current-token-beg))
                                                               (len (- js2-ts-cursor
                                                                       (js2-current-token-beg)))
                                                               op left right)
  "Represent a flow combination (union or intersection) type." (n i)
  (js2-print-ast (js2-flow-typespec-combination-node-left n) 0)
  (insert " ")
  (insert (js2-flow-typespec-combination-node-op n))
  (insert " ")
  (js2-print-ast (js2-flow-typespec-combination-node-right n) 0))

;;; Array type: a[]
(js2-flow-define-node-type (js2-flow-typespec-array-node (typespec)
                                                         (pos (js2-current-token-beg))
                                                         (len (- js2-ts-cursor
                                                                 (js2-current-token-beg)))
                                                         typespec)
  "Represent a flow array type." (n i)
  (js2-print-ast (js2-flow-typespec-maybe-node-typespec n) 0)
  (insert "[]"))

;;; Maybe types: ?a
(js2-flow-define-node-type (js2-flow-typespec-maybe-node (typespec)
                                                         (pos (js2-current-token-beg))
                                                         (len (- js2-ts-cursor
                                                                 (js2-current-token-beg)))
                                                         typespec)
  "Represent a flow maybe type." (n i)
  (insert "?")
  (js2-print-ast (js2-flow-typespec-maybe-node-typespec n) 0))

;;; Type alias definitions --- type Foo = Bar
(js2-flow-define-node-type (js2-flow-type-alias-node (type-name typespec)
                                                     (pos (len (- js2-ts-cursor pos)))
                                                     type-name typespec)
  "Represent a flow type alias definition." (n i)
  (insert "type ")
  (js2-print-ast (js2-flow-type-alias-node-type-name n) 0)
  (insert " = ")
  (js2-print-ast (js2-flow-type-alias-node-typespec n) 0)
  (insert ";\n"))

;;; Type-annotated class properties:
(js2-flow-define-node-type (js2-flow-typed-class-property-node (property value)
                                                               (pos (len (- js2-ts-cursor pos)))
                                                               property value)
  "Represent a type-annotated property with an optional assignment." (n i)
  (js2-print-ast (js2-flow-typed-class-property-node-property n) i)
  (when (js2-flow-typed-class-property-node-value n)
    (insert " = ")
    (js2-print-ast (js2-flow-typed-class-property-node-value n) 0))
  (insert ";"))


;;;; Parsing nodes:
(defun js2-parse-flow-leaf-type-spec ()
  (let ((flow-js2-parsing-typespec-p t)
        (tt (js2-get-token))
        (pos (js2-current-token-beg)))
    (cond ((= tt js2-HOOK)
           (let* ((type-spec (js2-parse-flow-leaf-type-spec))
                  (len (- (js2-node-end type-spec) pos))
                  (maybe (make-js2-flow-typespec-maybe-node :pos pos :len len
                                                            :typespec type-spec)))
             (js2-node-add-children maybe maybe type-spec)
             maybe))
          ((= tt js2-NAME)
           (js2-create-name-node nil))
          ((= tt js2-STRING)
           (make-js2-string-node :type tt))
          ((= tt js2-NUMBER)
           (make-js2-number-node))
          ((or (= tt js2-NULL)
               (= tt js2-TRUE)
               (= tt js2-FALSE))
           (make-js2-keyword-node :type tt))
          ((= tt js2-LB)
           (js2-parse-array-literal pos))
          ((= tt js2-LC)
           (js2-parse-object-literal))
          (t
           (js2-report-error "msg.syntax")
           (make-js2-error-node)))))

(defun js2-parse-flow-type-spec ()
  (let ((type-spec (js2-parse-flow-union-or-intersection-type-spec)))
    (when (js2-match-token js2-LB)
      (let ((array-node (js2-parse-array-literal (js2-current-token-beg))))
        (setq type-spec (make-js2-flow-typespec-array-node
                         :pos (js2-node-pos type-spec)
                         :len (- (js2-node-end array-node) (js2-node-pos type-spec))
                         :typespec type-spec))))
    (js2-set-face (js2-node-pos type-spec) (js2-node-end type-spec) 'font-lock-type-face 'record)
    type-spec))

(defun js2-parse-flow-union-or-intersection-type-spec ()
  (let ((type-spec (js2-parse-flow-leaf-type-spec)))
    (cl-loop while (or (js2-match-token js2-BITOR) (js2-match-token js2-BITAND))
             do (let ((op (if (eq (js2-current-token-type) js2-BITOR) ?| ?&))
                      (left type-spec)
                      (right (js2-parse-flow-leaf-type-spec)))
                  (setq type-spec (make-js2-flow-typespec-combination-node :op op
                                                                           :left type-spec
                                                                           :right right))
                  (js2-node-add-children type-spec left right)))
    type-spec))

;;; A helper to ensure symbol definition lines up correctly:
(defun flow-js2-define-symbol (orig-fun decl-type name &optional node ignore-not-in-block)
  (if (and (not (null node))
           (js2-flow-type-annotated-node-p node))
      (let ((name-node (js2-flow-type-annotated-node-name node)))
        (funcall orig-fun decl-type (js2-name-node-name name-node)
                 name-node
                 ignore-not-in-block))
    (funcall orig-fun decl-type name node ignore-not-in-block)))

(advice-add 'js2-define-symbol :around #'flow-js2-define-symbol)

;;; Parse "type" (it gets interpreted as a name):
(defun flow-js2-parse-name-or-label (orig-fun)
  (if (string-equal (js2-current-token-string) "type")
      (flow-js2-parse-type-alias)
    (funcall orig-fun)))
(advice-add 'js2-parse-name-or-label :around #'flow-js2-parse-name-or-label)

(defvar flow-js2-parsing-type-alias-p nil)
(defun flow-js2-parse-type-alias ()
  "Parse `type Foo = <type-def>` type aliases."
  (let ((pos (js2-current-token-beg)))
    (when (js2-match-token js2-NAME)
      (let ((name (js2-create-name-node nil)))
        (if (not (js2-match-token js2-ASSIGN))
            (progn
              (js2-report-error "msg.syntax")
              (make-js2-error-node))
          (let* ((flow-js2-parsing-type-alias-p t)
                 (typespec (js2-parse-flow-type-spec))
                 (alias (make-js2-flow-type-alias-node :pos pos
                                                       :type-name name :typespec typespec)))
            (js2-node-add-children typespec name alias)
            alias))))))

(defun flow-js2-record-name-node (orig-fun node)
  "Support registering an (optionally flow-typed) name node as a regular name node."
  (if (js2-flow-type-annotated-node-p node)
      (funcall orig-fun (js2-flow-type-annotated-node-name node))
    (funcall orig-fun node)))
(advice-add 'js2-record-name-node :around #'flow-js2-record-name-node)

;;; Parse class property syntax:
(defvar flow-js2-parse-object-literal-p nil)
(defun flow-js2-parse-object-literal (orig-fun)
  (let* ((flow-js2-parse-object-literal-p t)
         (object-literal (funcall orig-fun)))
    (if (js2-match-token js2-COLON)
        (let* ((typespec (js2-parse-flow-type-spec)))
          (js2-node-add-children object-literal typespec)
          object-literal)
      object-literal)))
(advice-add 'js2-parse-object-literal :around #'flow-js2-parse-object-literal)

(defun flow-js2-parse-named-prop (orig-fun tt previous-token &optional class-p)
  (let ((key (js2-parse-prop-name tt))
        (pos (js2-current-token-beg)))
    (cond ((not (null previous-token))
           (funcall orig-fun tt previous-token class-p))
          ((js2-match-token js2-ASSIGN)
           (let* ((assignment (js2-parse-assign-expr))
                  (prop (make-js2-flow-typed-class-property-node
                         :pos pos
                         :property key
                         :value assignment)))
             (js2-node-add-children prop key prop)
             prop))
          (t
           (funcall orig-fun tt previous-token class-p)))))
(advice-add 'js2-parse-named-prop :around #'flow-js2-parse-named-prop)

;;; Parse functions with return type annotations:
(defun flow-js2-parse-function-params (orig-fun function-type fn-node pos)
  (funcall orig-fun function-type fn-node pos)
  (when (js2-match-token js2-COLON)
    (let* ((typespec (js2-parse-flow-type-spec))
           (type-annotation (make-js2-flow-type-annotated-node :pos pos
                                                               :name fn-node
                                                               :typespec typespec)))
      (js2-node-add-children fn-node type-annotation typespec)
      type-annotation)))
(advice-add 'js2-parse-function-params :around #'flow-js2-parse-function-params)

;;; Parse `import type' nodes
(defun flow-js2-parse-import-clause (orig-fun)
  (if (js2-match-contextual-kwd "type")
      (funcall orig-fun)
    (funcall orig-fun)))
(advice-add 'js2-parse-import-clause :around #'flow-js2-parse-import-clause)

;;; Parse `export type' nodes
(defun flow-js2-parse-export (orig-fun)
  (if (js2-match-contextual-kwd "type")
      (flow-js2-parse-type-alias)
    (funcall orig-fun)))
(advice-add 'js2-parse-export :around #'flow-js2-parse-export)

;;; Parse generic marker in function expression
(defun flow-js2-parse-function-expr (orig-fun &optional async-p)
  "Parse generic marker after `function' token in a function expression.

Example:

  function<T>(param: T): T {}

This function parses the <T> immediately after `function'"
  (let ((generic-type (js2-match-token js2-LT)))
    (when generic-type
      (js2-parse-flow-type-spec)
      (while (js2-match-token js2-COMMA)
        (js2-parse-flow-type-spec))
      (js2-match-token js2-GT))
    (funcall orig-fun async-p)))
(advice-add 'js2-parse-function-expr :around #'flow-js2-parse-function-expr)
