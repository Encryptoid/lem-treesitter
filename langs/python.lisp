;; TODO How to call this :lem-treesitter/langs/python
(defpackage :lem-treesitter/python
  (:use :cl)
  (:local-nicknames (:ts :treesitter)
                    (:ts-bind :treesitter/bindings))
  (:export :*commonlisp*))
(in-package :lem-treesitter/python)

;; For now call this, need to introduce generic
(lem:define-command ts-init-py () ()
  (setq lem-treesitter/parser::*get-attr* #'get-py-attribute))

(defun get-py-attribute (node)
  "Get syntax highlighting attribute for a node, considering both base rules and queries"
  (or nil
   (apply-query-highlights node)
      ;(get-base-py-attribute node)
      ))

(defun get-base-py-attribute (node)
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
    ("not" 'lem:syntax-variable-attribute)


    ("identifier" (alexandria:switch ((ts:node-type (ts:node-parent node)) :test #'equal)
                    ("function_definition" 'lem:syntax-function-name-attribute)
                    ("class_definition" 'lem:syntax-type-attribute)
                    ("decorator" 'lem:syntax-type-attribute)
                    ("constant" 'lem:syntax-variable-attribute)))

    ("constant.builtin" 'lem:syntax-string-attribute)
    ("(" 'lem:syntax-variable-attribute)
    (")" 'lem:syntax-variable-attribute)
    (":" 'lem:syntax-variable-attribute)
    ("decorator > identifier" lem:syntax-keyword-attribute)

    ("integer" 'lem:syntax-constant-attribute)
    ("float" 'lem:syntax-constant-attribute)
    ("true" 'lem:syntax-constant-attribute)
    ("false" 'lem:syntax-constant-attribute)
    ("none" 'lem:syntax-constant-attribute)
    ("string" 'lem:syntax-constant-attribute) ;; The word string

    ("string_content" 'lem:syntax-string-attribute)

    ("comment" 'lem:syntax-comment-attribute)

    ("binary_operator" 'lem:syntax-builtin-attribute)
    ("unary_operator" 'lem:syntax-builtin-attribute)

    ("decorator" 'lem:syntax-keyword-attribute)

    ("type" 'lem:syntax-type-attribute)
    ("self" 'lem:syntax-type-attribute)

    ;; (t (if (some #'alpha-char-p (ts:node-type node)) 'syntax-keyword-attribute 'lem:syntax-constant-attribute))
    )

  )


(defvar *python-queries* nil)
(format nil "~a" *python-queries*)
(format nil "~a" *cached-python-query*)

;(load-python-queries)

(defvar *cached-python-query* nil)
(defvar *cached-python-cursor* nil)

(defun load-python-queries ()
  "Load Python syntax highlighting queries from .scm files and cache the query"
  (let ((queries-path "/home/l/code/treesitter/tree-sitter-python/queries/highlights.scm"))
    (when (probe-file queries-path)
      (setf *python-queries*
            (alexandria:read-file-into-string queries-path))
      ;; Create and cache the query and cursor when loading
      (let ((lang (gethash "python" lem-treesitter/core::*treesitters*)))
        (setf *cached-python-query*
              (ts::make-query lang *python-queries*))
        (setf *cached-python-cursor*
              (ts::make-query-cursor))))))


(defun apply-query-highlights (node)
  "Apply highlights based on query matches"
;  (return-from apply-query-highlights)
  (when *python-queries*
    ;; (lem-treesitter/core::logm (format nil "Capture match0: " ))
    (handler-case
        (let* ((query (ts::make-query (ts:node-language node) x))
               (cursor (ts::make-query-cursor)))
          (ts::query-cursor-exec cursor query node)
          (dolist (match-node (ts::query-cursor-nodes cursor))
            (let ((highlight (get-query-highlight match-node)))
              ;; (lem-treesitter/core::logm (format nil "Capture match1: ~A" (ts:node-type match-node)))
              ;; (lem-treesitter/core::logm (format nil "OOOOO: ~A" (ts:node-type match-node)))
              (when highlight
                (return-from apply-query-highlights highlight)))))
      (error (e)
        (format *error-output* "Query error: ~A~%" e)
        nil))))

(defun get-query-highlight (node)
  "Map query captures to Lem highlighting attributes"
  (lem-treesitter/core::logm (format nil "Capture: ~A, Parent: ~a, GP: ~A" (ts:node-type node)
                                     (ts:node-type (ts:node-parent node)) (ts:node-type (ts:node-parent (ts:node-parent node)))))
  (alexandria:when-let ((capture-name (ts:node-type node)))
    (alexandria:switch (capture-name :test #'equal)
      ("identifier" (alexandria:switch ((ts:node-type (ts:node-parent node)) :test #'equal)
                      ;("call" 'lem:syntax-variable-attribute)
                      ("function_definition" 'lem:syntax-function-name-attribute)
                      ("class_definition" 'lem:syntax-type-attribute)
                      ("decorator" 'lem:syntax-type-attribute)
                      ("function.builtin" 'lem:syntax-variable-attribute)
                      ("function" 'lem:syntax-variable-attribute)
                      ;(t 'lem:syntax-function-name-attribute)
                      ))

      ("call" (alexandria:switch ((ts:node-type (ts:node-parent node)) :test #'equal)
                ;("call" 'lem:syntax-variable-attribute)
                ("function" 'lem:syntax-function-name-attribute)
                ("function.builtin" 'lem:syntax-type-attribute)
                ("@function.builtin" 'lem:syntax-type-attribute)
                ("function.builtin" 'lem:syntax-variable-attribute)
                ("function" 'lem:syntax-variable-attribute)
                ;(t 'lem:syntax-function-name-attribute)
                ))

      ("@function.builtin" 'lem:syntax-variable-attribute)
      ("def" 'lem:syntax-keyword-attribute)
      ("class" 'lem:syntax-keyword-attribute)
      ("if" 'lem:syntax-keyword-attribute)
      ("else" 'lem:syntax-keyword-attribute)
      ("return" 'lem:syntax-keyword-attribute)
      ("import" 'lem:syntax-keyword-attribute)
      ("from" 'lem:syntax-keyword-attribute)
      ("as" 'lem:syntax-keyword-attribute)
      ("not" 'lem:syntax-variable-attribute)

      ("(" 'lem:syntax-variable-attribute)
      (")" 'lem:syntax-variable-attribute)
      (":" 'lem:syntax-variable-attribute)
      ("decorator > identifier" lem:syntax-keyword-attribute)

      ("integer" 'lem:syntax-constant-attribute)
      ("float" 'lem:syntax-constant-attribute)
      ("true" 'lem:syntax-constant-attribute)
      ("false" 'lem:syntax-constant-attribute)
      ("none" 'lem:syntax-constant-attribute)
      ("string" 'lem:syntax-constant-attribute) ;; The word string

      ("string_content" 'lem:syntax-string-attribute)

      ("comment" 'lem:syntax-comment-attribute)

      ("binary_operator" 'lem:syntax-builtin-attribute)
      ("unary_operator" 'lem:syntax-builtin-attribute)

      ("decorator" 'lem:syntax-keyword-attribute)

      ("type" 'lem:syntax-type-attribute)
      ("self" 'lem:syntax-type-attribute)
      ("decorator_definition" 'lem:syntax-variable-attribute))))


(defvar x "((identifier) @c)")
(setq x "((identifier) @iden)")

(defun apply-query-highlights (node)
  "Apply highlights based on query matches"
  (lem-treesitter/core::logm
   (format nil "Querying : node ~A" (ts:node-type node)))
  (handler-case
      (let* ((lang-ptr (ts::ts-node-language (ts::pointer node)))
             (_ (lem-treesitter/core::logm (format nil "Got language ptr: ~A" lang-ptr)))
             (lang (make-instance 'treesitter:language
                                  :free #'ts::ts-language-delete
                                  :pointer (ts::ts-language-copy lang-ptr)))
             (_ (lem-treesitter/core::logm "Created language instance"))
             (query (ts::make-query lang-ptr x))
             (_ (lem-treesitter/core::logm (format nil "Created query: ~A" query)))
             (cursor (ts::ts-query-cursor-new)))
        (lem-treesitter/core::logm
         (format nil "Querying cursor: query ~A, node ~A"
                 query (ts:node-type node)))
        ;; Get raw pointers for the exec call
        (ts-bind::ts-query-cursor-exec cursor
                                       (ts::pointer query)  ; Get raw query pointer
                                       (ts::pointer node))  ; Get raw node pointer
        (let ((match (ts::foreign-alloc '(:struct ts-bind::ts-query-match))))
          (loop :while (ts-bind::ts-query-cursor-next-match cursor match)
                :do (let ((pattern-index (ts-bind::ts-query-match-pattern-index match))
                          (capture-count (ts-bind::ts-query-match-capture-count match)))
                      (lem-treesitter/core::logm
                       (format nil "Match found: pattern ~A, captures ~A"
                               pattern-index capture-count))
                      ;; Process each capture in the match
                      (dotimes (i capture-count)
                        (let* ((capture (ts-bind::ts-query-match-capture match i))
                               (cap-node (ts-bind::ts-query-capture-node capture))
                               (index (ts-bind::ts-query-capture-index capture))
                               (name-len-ptr (ts::foreign-alloc :uint32))
                               (capture-name (ts-bind::ts-query-capture-name-for-id (ts::pointer query) index name-len-ptr)))
                          (lem-treesitter/core::logm
                           (format nil "Capture ~A: type=~A capture-name=~A"
                                   index
                                   (ts::ts-node-type cap-node)
                                   capture-name))
                          (ts::foreign-free name-len-ptr)
                          (ts-bind::ts-query-capture-delete capture)))
                      :finally (ts-bind::ts-query-match-delete match)))))
    (error (e)
      (lem-treesitter/core::logm
       (format nil "Query error: ~A~%" e))
      nil)))
