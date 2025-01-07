(in-package :lem-treesitter)

(rename-package :lem-treesitter :lts '(:lem-treesitter))
(rename-package :lem-treesitter/buffer :ltsb '(:lem-treesitter/buffer))
(rename-package :treesitter :ts '(:treesitter))
(rename-package :treesitter/bindings :tsb '(:treesitter/bindings))

(setq lem/buffer/internal:*global-syntax-highlight* nil)
(setq lem/buffer/internal:*global-syntax-highlight* t)

;; Manually load open buffer all.py
(defun update-buf ()
  (setq buf (lem:get-buffer "all.py"))
  (setq text (lem:buffer-text buf))
  (setq current-tree (ltsb::ts-parse-text text))
  (setq root-node (tree-root-node current-tree))
  (lem-treesitter/parser::walk-node-with-cursor root-node buf))

(update-buf)

(lem:add-hook (lem:variable-value 'lem/buffer/internal:after-change-functions :buffer buf) 'handle-edit)

(defun make-ts-edit (&key start-byte old-end-byte new-end-byte
                                start-point old-end-point new-end-point)
  "Creates a ts-input-edit structure for Tree-sitter.
   Points should be provided as (cons row column)."
  (let ((edit (tsb::foreign-alloc '(:struct tsb:ts-input-edit))))
    ;; Set the byte positions
    (setf (tsb::foreign-slot-value edit '(:struct tsb:ts-input-edit) 'tsb::start-byte) start-byte
          (tsb::foreign-slot-value edit '(:struct tsb:ts-input-edit) 'tsb::old-end-byte) old-end-byte
          (tsb::foreign-slot-value edit '(:struct tsb:ts-input-edit) 'tsb::new-end-byte) new-end-byte)
    ;; Set the point structures
    (let ((start (tsb::foreign-slot-pointer edit '(:struct tsb:ts-input-edit) 'tsb::start-point)))
      (setf (tsb::foreign-slot-value start '(:struct tsb::ts-point) 'tsb::row) (car start-point)
            (tsb::foreign-slot-value start '(:struct tsb::ts-point) 'tsb::column) (cdr start-point)))

    (let ((old-end (tsb::foreign-slot-pointer edit '(:struct tsb:ts-input-edit) 'tsb::old-end-point)))
      (setf (tsb::foreign-slot-value old-end '(:struct tsb::ts-point) 'tsb::row) (car old-end-point)
            (tsb::foreign-slot-value old-end '(:struct tsb::ts-point) 'tsb::column) (cdr old-end-point)))

    (let ((new-end (tsb::foreign-slot-pointer edit '(:struct tsb:ts-input-edit) 'tsb::new-end-point)))
      (setf (tsb::foreign-slot-value new-end '(:struct tsb::ts-point) 'tsb::row) (car new-end-point)
            (tsb::foreign-slot-value new-end '(:struct tsb::ts-point) 'tsb::column) (cdr new-end-point)))
    edit))

;; Helper function to update syntax highlighting in a specific range
(defun update-syntax-in-range (start-pos end-pos)
 ;; (lem:with-point ((start start-pos)
 ;;                 (end end-pos))
  ;; Your existing syntax highlighting logic here, but only for this range
   (lem:message "START: ~a, END: ~a" start-pos end-pos))
;; ))
  ;(walk-tree-for-range (tree-root-node current-tree) buf start end)))

(defun handle-edit (start end old-len)
  (let* ((start-byte (lem:point-bytes start))
         (end-byte (lem:point-bytes end))
         (start-point (cons (lem/buffer/internal::point-linum start)
                            (lem/buffer/internal::point-charpos start)))
         (end-point (cons (lem/buffer/internal::point-linum end)
                          (lem/buffer/internal::point-charpos end)))
         (edit (make-ts-edit
                :start-byte start-byte
                :old-end-byte (+ start-byte old-len)
                :new-end-byte end-byte
                :start-point start-point
                :old-end-point start-point
                :new-end-point end-point)))

    ;; Inform tree-sitter about the edit
    (tsb:ts-tree-edit (ts::pointer current-tree) edit)

    ;; Parse with the edited tree
    (let* ((text (lem:buffer-text buf))
           (new-tree (ts:parser-parse-string
                      (lem-treesitter/core::ensure-parser :python)
                      text))
           ;; Get the changed ranges and find affected nodes
           (root-node (ts:tree-root-node new-tree))
           (changed-nodes (get-changed-nodes current-tree new-tree)))

      ;(logm (format t "Edit Triggered at: ~a"))
      ;; Update syntax for each changed node
      (dolist (node changed-nodes)
        ;; (lem-treesitter/parser::walk-node-no-cursor node buf)
        (setq ch node)
        ;(logm (print-node node))
        (test )
        )

      ;; Update the current tree
      (setf current-tree new-tree))))

(defvar ch nil)
(setq ch nil)

(lem:message "Row: ~a Col: ~a" (node-type ch) (ts::ts-point-column ch))

(defun test (point new-tree)
  (let* ((cursor (ts:make-cursor (ts:tree-root-node new-tree)))
         ;; Get current point from buffer
         (current-byte (lem:point-bytes point)))
    ;; Move cursor to first child containing the current byte position
    (ts:cursor-goto-first-child-for-byte cursor current-byte)
    ;; Get the node at cursor
    (let ((node-at-point (ts:cursor-node cursor)))
      (logm (format nil "Node found: ~a" (ts:node-type node-at-point)))
      ))
  )

(defun get-changed-nodes (old-tree new-tree)
  "Get nodes that have changed between the old and new trees."
  (let* ((length-ptr (cffi:foreign-alloc :uint32))
         (ranges (tsb:ts-tree-get-changed-ranges
                  (ts::pointer old-tree)
                  (ts::pointer new-tree)
                  length-ptr))
         (num-ranges (cffi:mem-ref length-ptr :uint32))
         (root-node (ts:tree-root-node new-tree))
         (changed-nodes nil))

    ;; For each changed range, find the smallest node that contains it
    (dotimes (i num-ranges)
      (let* ((range (cffi:mem-aptr ranges '(:struct tsb:ts-range) i))
             (start-byte (cffi:foreign-slot-value range '(:struct tsb:ts-range) 'tsb::start-byte))
             (end-byte (cffi:foreign-slot-value range '(:struct tsb:ts-range) 'tsb::end-byte))
             ;; Find the smallest node that contains this range
             (containing-node (ts::ts-node-descendant-for-byte-range (ts::pointer root-node) start-byte end-byte)))
        (push containing-node changed-nodes)))

    ;; Clean up allocated memory
    (cffi:foreign-free length-ptr)
    (cffi:foreign-free ranges)
    changed-nodes))

(defun walk-tree-node (node buffer)
  "Process a single node and its children for syntax highlighting."
  (let ((children (ts:node-children node)))
    (if (not children)
        ;; Leaf node - apply syntax attribute
        (put-node-attribute node (get-py-attribute node nil) buffer)
        ;; Internal node - recurse on children
        (dolist (child children)
          (walk-tree-node child buffer)))))
