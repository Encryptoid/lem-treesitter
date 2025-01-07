;; TODO How to call this :lem-treesitter/langs/python
(defpackage :lem-treesitter/python
  (:use :cl)
  (:local-nicknames (:ts :treesitter))
  (:export :*commonlisp*))
(in-package :lem-treesitter/python)

(lem:define-command ts-init-py () ()
  (setq lem-treesitter/parser::*get-attr* #'get-py-attribute))

(defun get-py-attribute (node)
  (alexandria:switch ((ts:node-type node) :test #'equal)
    ;; Keywords
    ("def" 'lem:syntax-keyword-attribute)
    ("class" 'lem:syntax-keyword-attribute)
    ("if" 'lem:syntax-keyword-attribute)
    ("else" 'lem:syntax-keyword-attribute)
    ("return" 'lem:syntax-keyword-attribute)
    ("import" 'lem:syntax-keyword-attribute)
    ("from" 'lem:syntax-keyword-attribute)
    ("as" 'lem:syntax-keyword-attribute)

    ("identifier" (alexandria:switch ((ts:node-type (ts:node-parent node)) :test #'equal)
                    ("function_definition" 'lem:syntax-function-name-attribute)
                    ("class_definition" 'lem:syntax-type-attribute)))

    (")" 'lem:syntax-constant-attribute)
    (")" 'lem:syntax-constant-attribute)
    ("decorator > identifier" lem/buffer/internal:syntax-keyword-attribute)

    ("integer" 'lem:syntax-constant-attribute)
    ("float" 'lem:syntax-constant-attribute)
    ("true" 'lem:syntax-constant-attribute)
    ("false" 'lem:syntax-constant-attribute)
    ("none" 'lem:syntax-constant-attribute)
    ("string" 'lem:syntax-constant-attribute) ;; The word string

    ("string_content" 'lem:syntax-string-attribute)

    ("comment" 'lem:syntax-comment-attribute)

    ("binary_operator" 'syntax-builtin-attribute)
    ("unary_operator" 'syntax-builtin-attribute)

    ("decorator" 'syntax-keyword-attribute)

    ("type" 'syntax-type-attribute)

    (t (if (some #'alpha-char-p (ts:node-type node)) 'lem:syntax-keyword-attribute 'lem:syntax-constant-attribute))
    ))
