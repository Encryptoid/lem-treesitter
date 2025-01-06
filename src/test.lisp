(in-package :lem-treesitter)
(defparameter *top-level-forms*
  '("list" ; The raw list form that will contain defun, defvar etc.
    "comment"
    "block_comment"))

(defun find-current-form (buf-info point)
   "Find the current top-level form at POINT in BUF-INFO.
Returns (values node name type) where:
  NODE is the tree-sitter node
  NAME is the name of the form (for definitions)
  TYPE is the type of form (e.g. defun, defvar, etc)"
   (let* ((tree (ts:tree-root-node (buffer-tree buf-info)))
          (source (buffer-source buf-info))
          (node (ts:node-descendant-for-range tree point (1+ point))))

     (lem:message "Initial node type: ~A" (ts:node-type node))  ;; Debug

     (loop with current = node
           while (and current (not (ts:node-null-p current)))
           do (let ((type (ts:node-type current)))
                (lem:message "Walking up: ~A" type)  ;; Debug
                (when (string= type "list")
                  (let* ((first-child (ts:node-child current 0))
                         (def-type (when first-child
                                     (ts:node-text first-child source))))
                    (lem:message "List first child: ~A" def-type)  ;; Debug
                    (when (and def-type
                               (> (length def-type) 3)
                               (string= "def" def-type :end2 3))
                      (let* ((name-node (ts:node-child current 1))
                             (name (when name-node
                                     (ts:node-text name-node source))))
                        (return (values current name def-type))))))
                (setf current (ts:node-parent current)))
           finally (return (values nil nil nil)))))

(lem:define-command current-form () ()
   "Display information about the current top-level form."
   (if (null lem-treesitter/buffer:*ts-buffer-info*)
       (lem:message "No tree-sitter buffer loaded. Use M-x ts-load-buffer first.")
       (multiple-value-bind (node name type)
           (find-current-form
            lem-treesitter/buffer:*ts-buffer-info*
            (lem/buffer/internal:point-bytes (lem:current-point)))
         (if node
             (let ((start-point (ts:node-start-point node)))
               (lem:message "In ~A~@[ ~A~] (line ~D)"
                            type name (1+ (car start-point))))
             (lem:message "Not in any top-level form")))))

(defun find-current-form (buf-info point)
  "Find the current top-level form at POINT in BUF-INFO.
Returns (values node name type) where:
  NODE is the tree-sitter node
  NAME is the name of the form (for definitions)
  TYPE is the type of form (e.g. defun, defvar, etc)"
  (let* ((tree (ts:tree-root-node (lem-treesitter/buffer::buffer-tree buf-info)))
         (source (lem-treesitter/buffer:buffer-source buf-info))
         (node (ts:node-descendant-for-range tree point (1+ point))))

    (loop with current = node
          while (and current (not (ts:node-null-p current)))
          do (let ((type (ts:node-type current)))
               ;; If it's a list, check if it's a definition
               (when (string= type "list")
                 (let* ((first-child (ts:node-child current 0))
                        (def-type (when first-child
                                    (ts:node-text first-child source))))
                   (when (and def-type
                              (> (length def-type) 3)
                              (string= "def" def-type :end2 3))
                     (let* ((name-node (ts:node-child current 1))
                            (name (when name-node
                                    (ts:node-text name-node source))))
                       (return (values current name def-type))))))
               (setf current (ts:node-parent current)))
          finally (return (values nil nil nil)))))

(lem:define-command current-form () ()
  "Display information about the current top-level form."
  (if (null lem-treesitter/buffer:*ts-buffer-info*)
      (lem:message "No tree-sitter buffer loaded. Use M-x ts-load-buffer first.")
      (multiple-value-bind (node name type)
          (find-current-form
           lem-treesitter/buffer:*ts-buffer-info*
           (lem/buffer/internal:point-bytes (lem:current-point)))
        (if node
            (let ((start-point (ts:node-start-point node)))
              (lem:message "In ~A~@[ ~A~] (line ~D)"
                           type name (1+ (car start-point))))
            (lem:message "Not in any top-level form")))))
