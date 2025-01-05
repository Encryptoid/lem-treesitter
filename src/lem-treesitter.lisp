(defpackage :lem-treesitter
  (:use :cl :treesitter)
  (:local-nicknames (:ts :treesitter)))
(in-package :lem-treesitter)

(defun find-node-at-point (buffer-info byte-pos)
  "Find the node at the given byte position.
  Returns two values: immediate node at point and the containing top-level form (if any)."
  (let* ((root (ts:tree-root-node (lem-treesitter/buffer::buffer-tree buffer-info)))
       ;; Find the smallest node containing our pos
       (node (ts:node-descendant-for-range
              root byte-pos (1+ byte-pos)))
       (current node)
       (top-level nil))

  ;; Only proceed if we got a valid initial node
  (when (and node (not (ts:node-null-p node)))
    ;; Walk up the tree until we hit a top-level form or root
    (loop while (and current
                     (not (ts:node-null-p current)))
          do
             (let ((type (ts:node-type current)))
               ;; Check if this is a top-level form
               (when (member type '("defun" "defparameter" "defvar" "defmacro"
                                    "defgeneric" "defmethod" "defclass")
                             :test #'string=)
                 (setf top-level current)))
             ;; Move up to parent
             (setf current (ts:node-parent current))))
  (values node top-level)))

(lem:define-command ts-curr-point () ()
  "Show information about the node at point"
  (if (null lem-treesitter/buffer:*ts-buffer-info*)
    (lem:message "No tree-sitter buffer loaded. Use M-x ts-load-buffer first.")
    (let ((pos (lem/buffer/internal:point-bytes (lem:current-point))))
      (multiple-value-bind (node top-level)
          (find-node-at-point lem-treesitter/buffer:*ts-buffer-info* pos)
        (if node
            (lem:message "CurrentObject: ~A, CurrentPos: ~A ~:[~;, Top-Level-Node ~A~]"
                         (ts:node-type node)
                         pos
                         top-level
                         (when top-level (treesitter:node-type top-level)))
            (lem:message "No node found at position ~A" pos))))))



(defun find-node-at-point (buffer-info byte-pos)
  "Find the node at the given byte position.
Returns two values: immediate node at point and the containing top-level form (if any)."
  (let* ((root (treesitter:tree-root-node (lem-treesitter/buffer::buffer-tree buffer-info)))
         ;; Find the smallest node containing our pos
         (node (treesitter:node-descendant-for-range
                root byte-pos (1+ byte-pos)))
         ;; Print initial node info
         (_ (lem:message "Initial node: ~A" (treesitter:node-type node)))
         (current node)
         (top-level nil))

    ;; Only proceed if we got a valid initial node
    (when (and node (not (treesitter:node-null-p node)))
      ;; Walk up the tree until we hit a top-level form or root
      (loop while (and current
                       (not (treesitter:node-null-p current)))
            do
               (let ((type (treesitter:node-type current)))
                 ;; Debug: print each node as we walk up
                 (lem:message "Walking up: ~A" type)
                 ;; Check if this is a top-level form
                 (when (member type '("defun" "defparameter" "defvar" "defmacro"
                                      "defgeneric" "defmethod" "defclass")
                               :test #'string=)
                   (setf top-level current)))
               ;; Move up to parent
               (setf current (treesitter:node-parent current))))

    ;; Return both nodes for flexibility
    (values node top-level)))

(lem:define-command ts-top-expr () ()
  "Show information about the node at point"
  (if (null lem-treesitter/buffer:*ts-buffer-info*)
      (lem:message "No tree-sitter buffer loaded. Use M-x ts-load-buffer first.")
      (let ((pos (lem/buffer/internal:point-bytes (lem:current-point))))
        (multiple-value-bind (node top-level)
            (find-node-at-point lem-treesitter/buffer:*ts-buffer-info* pos)
          (lem:message "CurrentObject: ~A, CurrentPos: ~A, TopLevel: ~A~@[ ~A~]"
                       (treesitter:node-type node)
                       pos
                       (if top-level
                           (treesitter:node-type top-level)
                           "none")
                       (when top-level
                         (let ((name-node (treesitter:node-child top-level 1)))
                           (when name-node
                             (treesitter:node-text name-node (lem/buffer/internal:buffer-source lem-treesitter/buffer::buffer-info))))))))))
