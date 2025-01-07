(defpackage :lem-treesitter/parser
  (:use :cl)
  (:local-nicknames (:ts :treesitter))
  (:export :*commonlisp*))
(in-package :lem-treesitter/parser)

;; TODO: Get correct default
(defun default-attribute (node)
  (declare (ignore node))
  'lem:syntax-constant-attribute)

(defvar *get-attr* #'default-attribute)

(defun put-node-attribute (node attribute buffer)
  (lem:with-point ((start (lem:buffer-start-point buffer))
                   (end (lem:buffer-end-point buffer)))
    (lem:move-to-bytes start (1+ (ts:node-start-byte node)))
    (lem:move-to-bytes end (1+ (ts:node-end-byte node)))
    (lem:put-text-property start end :attribute attribute)))

(defun walk-cursor (cursor buffer)
  (let* ((node (ts:cursor-node cursor)))
    (unless (ts:cursor-goto-first-child cursor)
      (put-node-attribute node (funcall *get-attr* node) buffer)
      (lem-treesitter::print-node node)
      (return-from walk-cursor))
    (loop do (walk-cursor cursor buffer)
             (unless (ts:cursor-goto-next-sibling cursor)
               (return)))
    (ts:cursor-goto-parent cursor)))

(defun walk-node-with-cursor (root-node buffer)
  (walk-cursor (ts:make-cursor root-node) buffer))

(defun walk-node-no-cursor (node buffer)
  (let ((children (ts:node-children node)))
    (if (not children)
        (put-node-attribute node (funcall *get-attr* node) buffer)
        (dolist (child children)
          (walk-node-no-cursor child buffer)))))
