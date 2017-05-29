(require 'rjsx-mode)
(require 'flow-mode)

(defgroup flow-js2-minor-mode nil
  "Support for flow annotations in JSX files."
  :group 'js2-mode)

;;;###autoload
(define-minor-mode flow-js2-minor-mode
  "Minor mode for editing JS files with flow type annotations."
  :lighter ":FLOW"
  :group 'flow-js2-minor-mode
  (dolist (kw '("boolean" "number" "string" "null" "void" "any" "mixed"))
    (add-to-list 'js2-additional-externs kw)))

(defun activate-flow-js2-minor-mode ()
  (when (and (flow-tag-present-p)
             ;; (flow-configured-p)
             )
    (flow-js2-minor-mode +1)))


(defvar flow-js2-parsing-typespec-p nil)
(defun flow-js2-create-name-node (orig-fun &rest args)
  (let ((name (apply orig-fun args)))
    (if (and flow-js2-minor-mode
             (not flow-js2-parsing-typespec-p))
        (apply 'flow-js2-do-create-name-node name args)
      name)))

(advice-add 'js2-create-name-node :around #'flow-js2-create-name-node)

(add-hook 'js2-mode-hook 'activate-flow-js2-minor-mode)

(defun flow-js2-do-create-name-node (name &optional check-activation-p token string)
  (when (js2-match-token js2-COLON)
    (let* ((pos (js2-node-pos name))
           (tt (js2-current-token-type))
           (left name)
           (type-spec (js2-parse-flow-type-spec))
           (len (- (js2-node-end type-spec) pos)))
      (setq name (make-js2-flow-typed-name-node :pos pos :len len :name name :typespec type-spec))
      (js2-node-add-children name left type-spec)))
  name)


;;; Node types

;;; Type-annotated variables or other names --- const a: string
(cl-defstruct (js2-flow-typed-name-node
               (:include js2-node)
               (:constructor nil)
               (:constructor make-js2-flow-typed-name-node (&key (pos (js2-current-token-beg))
                                                                 (len (- js2-ts-cursor
                                                                         (js2-current-token-beg)))
                                                                 name
                                                                 typespec)))
  "Represent a name with a flow type annotation. This applies to
  variables and function arguments alike."
  name
  typespec)

(put 'cl-struct-js2-flow-typed-name-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-flow-typed-name-node 'js2-printer 'js2-print-flow-typed-name-node)

(defun js2-print-flow-typed-name-node (n i)
  (let* ((tt (js2-node-type n)))
    (js2-print-ast (js2-flow-typed-name-node-name n) 0)
    (insert ": ")
    (js2-print-ast (js2-flow-typed-name-node-typespec n) 0)))

;;; Combination types --- a | b or a & b
(cl-defstruct (js2-flow-typespec-combination-node
               (:include js2-node)
               (:constructor nil)
               (:constructor make-js2-flow-typespec-combination-node (&key (pos (js2-current-token-beg))
                                                                     (len (- js2-ts-cursor
                                                                             (js2-current-token-beg)))
                                                                     op left right)))
  "Represent a flow combination type."
  op
  left
  right)

(put 'cl-struct-js2-flow-typespec-combination-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-flow-typespec-combination-node 'js2-printer 'js2-print-flow-typespec-combination-node)

(defun js2-print-flow-typespec-combination-node (n i)
  (js2-print-ast (js2-flow-typespec-combination-node-left n) 0)
  (insert " ")
  (insert (js2-flow-typespec-combination-node-op n))
  (insert " ")
  (js2-print-ast (js2-flow-typespec-combination-node-right n) 0))

;;; Maybe types: ?a
(cl-defstruct (js2-flow-typespec-maybe-node
               (:include js2-node)
               (:constructor nil)
               (:constructor make-js2-flow-typespec-maybe-node (&key (pos (js2-current-token-beg))
                                                                     (len (- js2-ts-cursor
                                                                             (js2-current-token-beg)))
                                                                     typespec)))
  "Represent a flow maybe type."
  typespec)

(put 'cl-struct-js2-flow-typespec-maybe-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-flow-typespec-maybe-node 'js2-printer 'js2-print-flow-typespec-maybe-node)

(defun js2-print-flow-typespec-maybe-node (n i)
  (insert "?")
  (js2-print-ast (js2-flow-typespec-maybe-node-typespec n) 0))

;;; Parsing nodes:
(defun js2-parse-flow-leaf-type-spec ()
  (let ((flow-js2-parsing-typespec-p t)
        (tt (js2-get-token)))
    (cond ((= tt js2-HOOK)
           (let* ((pos (js2-current-token-beg))
                  (type-spec (js2-parse-flow-leaf-type-spec))
                  (len (- (js2-node-end type-spec) pos))
                  (maybe (make-js2-flow-typespec-maybe-node :pos pos :len len
                                                            :typespec type-spec)))
             (js2-node-add-children maybe maybe type-spec)
             maybe))
          ((= tt js2-NAME)
           (js2-parse-name tt))
          ((= tt js2-STRING)
           (make-js2-string-node :type tt))
          ((= tt js2-NUMBER)
           (make-js2-number-node))
          ((or (= tt js2-NULL)
               (= tt js2-TRUE)
               (= tt js2-FALSE))
           (make-js2-keyword-node :type tt))
          (t
           (js2-report-error "msg.syntax")
           (make-js2-error-node)))))

(defun js2-parse-flow-type-spec ()
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

;;; A helpers to ensure symbol definition lines up correctly:
(defun flow-js2-define-symbol (orig-fun decl-type name &optional node ignore-not-in-block)
  (if (and (not (null node))
           (js2-flow-typed-name-node-p node))
      (let ((name-node (js2-flow-typed-name-node-name node)))
        (funcall orig-fun decl-type (js2-name-node-name name-node)
                 name-node
                 ignore-not-in-block))
    (funcall orig-fun decl-type name node ignore-not-in-block)))

(advice-add 'js2-define-symbol :around #'flow-js2-define-symbol)
