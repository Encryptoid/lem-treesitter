(defpackage :lem-treesitter/playground
  (:use :cl :treesitter)
  (:local-nicknames (:ts :treesitter))
  (:import-from :lem-treesitter/buffer
                :buffer-source
                :buffer-info))
(in-package :lem-treesitter/playground)

(defun find-node-at-point (buf-info byte-pos)
 "Find the node at the given byte position.
  Returns two values: immediate node at point and the containing top-level form (if any)."
  (let* ((root (ts:tree-root-node (lem-treesitter/buffer::buffer-tree buf-info)))
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

(lem:define-command ts-max-parent () ()
  (if (null lem-treesitter/buffer:*ts-buffer-info*)
      (lem:message "No tree-sitter buffer loaded. Use M-x ts-load-buffer first.")
      ()
    ))


(defun node-at-point (buffer)
  "Gets the smallest named-node at the point"
  (let* ((start (lem:point-bytes (lem:buffer-point buffer)))
         (end start))
    (ts:node-descendant-for-range (ts:tree-root-node (lem:buffer-value buffer :tree))
                                  start
                                  end :named t)))

(node-at-point (lem:current-buffer))

(defun find-node-at-point (buf-info byte-pos)
  "Find the node at the given byte position.
Returns two values: immediate node at point and the containing top-level form (if any)."
  (let* ((root (treesitter:tree-root-node (lem-treesitter/buffer::buffer-tree buf-info)))
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
                             (treesitter:node-text name-node (lem-treesitter/buffer::buffer-source lem-treesitter/buffer::buffer-info))))))))))



(defun get-top-level-form (buf-info byte-pos)
  "Find and return detailed information about the current top-level form."
  (let* ((root (ts:tree-root-node (lem-treesitter/buffer::buffer-tree buf-info)))
         (node (ts:node-descendant-for-range root byte-pos (1+ byte-pos)))
         (current node)
         top-level-info)

    ;; Walk up the tree until we find a top-level form
    (loop while (and current (not (ts:node-null-p current)))
          do (let ((type (ts:node-type current)))
               ;; Check if this is a top-level form
               (when (member type '("defun" "defparameter" "defvar" "defmacro"
                                    "defgeneric" "defmethod" "defclass" "defpackage"
                                    "in-package" "eval-when" "define-condition")
                             :test #'string=)
                 ;; Extract useful information about the form
                 (let* ((source (lem-treesitter/buffer::buffer-source buf-info))
                        (start-byte (ts:node-start-byte current))
                        (end-byte (ts:node-end-byte current))
                        (name-node (ts:node-child current 1))  ; Usually the name is the second child
                        (name (when name-node
                                (ts:node-text name-node source)))
                        (start-point (ts:node-start-point current))
                        (end-point (ts:node-end-point current)))
                   (setf top-level-info
                         (list :type type
                               :name name
                               :node current
                               :start-pos start-byte
                               :end-pos end-byte
                               :start-line (car start-point)
                               :end-line (car end-point)))))
               ;; Move up to parent
               (setf current (ts:node-parent current))))
    top-level-info))

(lem:define-command show-current-form () ()
  "Display information about the current top-level form."
  (if (null lem-treesitter/buffer:*ts-buffer-info*)
      (lem:message "No tree-sitter buffer loaded. Use M-x ts-load-buffer first.")
      (let* ((pos (lem/buffer/internal:point-bytes (lem:current-point)))
             (form-info (get-top-level-form lem-treesitter/buffer:*ts-buffer-info* pos)))
        (if form-info
            (lem:message "In ~A~@[ ~A~] (lines ~D-~D)"
                         (getf form-info :type)
                         (getf form-info :name)
                         (1+ (getf form-info :start-line))  ; Convert to 1-based line numbers
                         (1+ (getf form-info :end-line)))
            (lem:message "Not in any top-level form")))))


(defun node-at-point (buffer)
  "Gets the smallest named-node at the point"
  (let* ((start (lem:point-bytes (lem:buffer-point buffer)))
         (end start))
    (ts:node-descendant-for-range (ts:tree-root-node (lem:buffer-value buffer :tree))
                                  start
                                  end :named t)))

(defun node-text-at-point (buffer)
  (ts:node-text (node-at-point buffer)
                (lem:buffer-text buffer)))

(node-text-at-point (lem:current-buffer))
