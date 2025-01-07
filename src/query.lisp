(in-package :lem-treesitter/query)

;; File for all the queries
; The q prefix is to not have name clash(eg. defun)
; These are function rather than vars so params can be passed later

;; Just look for defun
(defun q-defun ()
  "(defun) @defun")

;; Get all the nodes in the function (very verbose query)
; (defun q-defun ()
;   "(defun (defun_header
;            keyword: (_) @keyword
;            function_name: (_) @name
;            lambda_list: (_) @params)
;     value: (_) @body) @function.definition")

;; This one won't work out the box for you unless you rebuild the grammar(or use /external/ binary)
(defun q-defparameter-custom ()
  "(defun
  (defun_header
    keyword: (defun_keyword) @keyword
    (#eq? @keyword \"defparameter\")
    function_name: (_) @name))")

(defun q-defparameter ()
  "(list_lit
     value: (sym_lit) @deftype
     (#eq? @deftype \"defparameter\")
     value: (sym_lit) @name)")
