(in-package :lem-treesitter/core)

;; Ai generated, feel free to edit, works for now

(defun logm (string)
  (with-open-file (stream "~/temp/ts.log"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (write-line string stream)))

(defun print-nodes (nodes &optional source)
  (format t "~%NODES PARSED:: ~a" (length nodes))
  (dolist (node nodes)
    (print-node node source)))

(defun print-node (node &optional source)
  "Pretty print a treesitter node. If source is provided, shows both the
   tree structure and the actual text content."
  (format t "~%Node Type: ~a" (ts:node-type node))
  (format t "~%Tree structure: ~a" (ts:node-string node))
  (when source
    (format t "~%Text content:~%~a" (ts:node-text node source)))
  (format t "~%Start byte: ~a, End byte: ~a"
          (ts:node-start-byte node)
          (ts:node-end-byte node))
  (format t "~%Start point: ~a, End point: ~a"
          (ts:node-start-point node)
          (ts:node-end-point node))
  (format t "~%Named?: ~a" (ts:node-named-p node))
  (format t "~%Children count: ~a" (ts:node-child-count node))
  (format t "~%---~%")
  (values))
