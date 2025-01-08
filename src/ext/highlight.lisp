(in-package :lem-treesitter/ext/highlight)

;(defgeneric handle-query-match)

;(lem:variable-value lem-treesitter/buffer::*ts-edit-hook* :buffer (lem:current-buffer))

(defun init-highlighting (buffer)
  (let ((tree (lem:buffer-value buffer :ts-tree)))
    (lem-treesitter/core::logm (format nil "Tree for highlighting: ~A" (lem-treesitter/core::print-tree-to-string (ts:tree-root-node tree))))
    (unless tree
      (error "No tree found in buffer for highlighting"))
    (let ((root-node (ts:tree-root-node tree)))
      (lem-treesitter/core::logm (format nil "Root node: ~A" root-node))
      (unless root-node
        (error "Could not get root node from tree"))
      (highlight-changed-nodes buffer (list root-node)))) ;; Initial full highlight
  (lem:add-hook lem-treesitter/buffer::*ts-edit-hook* 'highlight-changed-nodes))

(defun highlight-changed-nodes (buffer nodes)
  "Highlight changed nodes after a tree-sitter edit."
  (dolist (node nodes)
    (lem-treesitter/parser::walk-node-with-cursor node buffer)))
