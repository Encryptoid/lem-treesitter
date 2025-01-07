(in-package :lem-treesitter/ext/highlight)

;(defgeneric )

(defun init-highlighting (buffer)
  (lem-treesitter/parser::walk-node-with-cursor
   (ts:tree-root-node (lem:buffer-value buffer :ts-tree)) buffer))
