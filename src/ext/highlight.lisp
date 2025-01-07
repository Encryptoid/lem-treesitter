(in-package :lem-treesitter/ext/highlight)

;(defgeneric )

(defun init-highlighting (buffer)
  (lem-treesitter/parser::walk-node-with-cursor
   (ts:tree-root-node (lem:buffer-value buffer :ts-tree)) buffer)
  (lem:add-hook lem-treesitter/buffer::*ts-edit-hook* 'highlight-changed-nodes)
;  (lem:add-hook (lem:variable-value lem-treesitter/buffer::*ts-edit-hook* :buffer) 'highlight-changed-nodes)
  )

;(lem:variable-value lem-treesitter/buffer::*ts-edit-hook* :buffer (lem:current-buffer))

(defun highlight-changed-nodes (buffer nodes)
  "Highlight changed nodes after a tree-sitter edit."
  (dolist (node nodes
    (lem-treesitter/parser::walk-node-with-cursor node buffer)))
