(defpackage :lem-treesitter/core
  (:use :cl)
  (:local-nicknames (:ts :treesitter))
  (:export :*commonlisp*))
(in-package :lem-treesitter/core)

;; Seems you call this once
(cffi:use-foreign-library "/usr/local/lib/libtree-sitter-commonlisp.so")
(cffi:defcfun "tree_sitter_commonlisp" :pointer)

;; Only setup one language for now
(defvar *commonlisp* (make-instance 'ts:language
                                    :free #'ts::ts-language-delete
                                    :pointer (tree-sitter-commonlisp)))

;(asdf:load-asd (probe-file "/home/l/code/treesitter/cl-treesitter/cl-treesitter.asd"))
;(ql:quickload :cl-treesitter)
