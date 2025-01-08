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
  (unless node
    (format t "Node was null")
    (return-from print-node))

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

(defun print-node (node &optional source)
  "Pretty print a treesitter node. If source is provided, shows both the
   tree structure and the actual text content."
  (if (null node)
      "Node was null"  ; Return string instead of using format
      (with-output-to-string (s)
        (format s "~%Node Type: ~a" (ts:node-type node))
        (format s "~%Tree structure: ~a" (ts:node-string node))
        (when source
          (format s "~%Text content:~%~a" (ts:node-text node source)))
        (format s "~%Start byte: ~a, End byte: ~a"
                (ts:node-start-byte node)
                (ts:node-end-byte node))
        (format s "~%Start point: ~a, End point: ~a"
                (ts:node-start-point node)
                (ts:node-end-point node))
        (format s "~%Named?: ~a" (ts:node-named-p node))
        (format s "~%Children count: ~a" (ts:node-child-count node))
        (format s "~%---"))))

(defun print-tree (node &optional (depth 0) source (stream *standard-output*))
  "Print a tree-sitter node and all its children in a hierarchical format.
   depth: Current indentation level
   source: Optional source text to show node content
   stream: Where to print the output (defaults to *standard-output*)"
  (when node
    (let ((indent (make-string (* depth 2) :initial-element #\Space)))
      ;; Print current node info
      (format stream "~&~A~A"
              indent
              (ts:node-type node))

      ;; Recursively print all children
      (let ((child-count (ts:node-child-count node)))
        (when (> child-count 0)
          (format stream "~%")
          (loop for i from 0 below child-count
                for child = (ts:node-child node i)
                when child
                do (print-tree child (1+ depth) source stream)))))))

(defun print-tree-to-string (node &optional source)
  "Return a string representation of the node tree."
  (with-output-to-string (s)
    (print-tree node 0 source s)))


;; And update logm to handle nil safely
(defun logm (string)
  (with-open-file (stream "~/temp/ts.log"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (write-line (or string "NIL") stream)))
