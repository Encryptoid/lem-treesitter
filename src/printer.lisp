(in-package :lem-treesitter)

;; Ai generated, feel free to edit, works for now

(defun print-nodes (nodes &optional source)
  (format t "~%NODES PARSED:: ~a" (length nodes))
  (dolist (node nodes)
    (print-node node source)))

(defun print-node (node &optional source)
  "Pretty print a treesitter node. If source is provided, shows both the
   tree structure and the actual text content."
  (format t "~%Node Type: ~a" (node-type node))
  (format t "~%Tree structure: ~a" (node-string node))
  (when source
    (format t "~%Text content:~%~a" (node-text node source)))
  (format t "~%Start byte: ~a, End byte: ~a"
          (node-start-byte node)
          (node-end-byte node))
  (format t "~%Start point: ~a, End point: ~a"
          (node-start-point node)
          (node-end-point node))
  (format t "~%Named?: ~a" (node-named-p node))
  (format t "~%Children count: ~a" (node-child-count node))
  (format t "~%---~%")
  (values))
