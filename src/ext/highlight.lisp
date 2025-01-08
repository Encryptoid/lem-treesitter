(in-package :lem-treesitter/ext/highlight)

;(defgeneric )

(defun init-highlighting (buffer)
  (let ((tree (lem:buffer-value buffer :ts-tree)))
    (lem-treesitter/core::logm (format nil "Tree for highlighting: ~A" (lem-treesitter/core::print-tree-to-string (ts:tree-root-node tree))))
    (unless tree
      (error "No tree found in buffer for highlighting"))
    (let ((root-node (ts:tree-root-node tree)))
      (lem-treesitter/core::logm (format nil "Root node: ~A" root-node))
      (unless root-node
        (error "Could not get root node from tree"))
      (lem-treesitter/parser::walk-node-with-cursor root-node buffer)))
  (lem:add-hook lem-treesitter/buffer::*ts-edit-hook* 'highlight-changed-nodes))

;(lem:variable-value lem-treesitter/buffer::*ts-edit-hook* :buffer (lem:current-buffer))

(defun highlight-changed-nodes (buffer nodes)
  "Highlight changed nodes after a tree-sitter edit."
  (lem-treesitter/core::logm (format nil "after hooks: ~a" nodes))
  (dolist (node nodes)
    (lem-treesitter/parser::walk-node-with-cursor node buffer)))
