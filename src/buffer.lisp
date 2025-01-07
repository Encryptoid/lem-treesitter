(in-package :lem-treesitter/buffer)

;; (lem:define-editor-variable *ts-edit-hook* '()
;;   "Hook run after tree-sitter edits with the changed nodes and buffer as arguments.")

(defvar *ts-edit-hook* '()
  "Hook run after tree-sitter edits with the changed nodes and buffer as arguments.")

(defclass ts-buffer-info ()
  ((tree :accessor buffer-tree)
   (cursor :accessor buffer-cursor)
   (source :accessor buffer-source)
   (parser :accessor buffer-parser)
   ;; Cache fields
   (root-node :accessor buffer-root-node :initform nil)
   (last-parse-time :accessor buffer-last-parse-time :initform 0)
   (last-position :accessor buffer-last-position :initform -1)
   (last-node :accessor buffer-last-node :initform nil)))

(defun store-buffer-info (buffer lang)
  (lem:message "store-buffer-info received lang: ~A" lang)
  (lem-treesitter/core:ensure-treesitter lang)
  (let* ((parser (lem-treesitter/core::ensure-parser lang))
         (text (lem:buffer-text (lem:current-buffer))))
    (setf (lem:buffer-value buffer :ts-tree)
          (ts:parser-parse-string parser text))
    (setf (lem:buffer-value buffer :ts-parser) parser))
  (lem:message "Stored treesitter buffer info. Lang: ~a" lang)
  (lem:add-hook (lem:variable-value 'lem/buffer/internal:after-change-functions :buffer buffer) 'handle-edit))

;(lem:variable-value 'lem/buffer/internal:after-change-functions :buffer (lem:get-buffer "all.py"))

(defun get-changed-nodes (old-tree new-tree)
  "Get nodes that have changed between the old and new trees."
  (let* ((length-ptr (cffi:foreign-alloc :uint32))
         (ranges (tsbind:ts-tree-get-changed-ranges
                  (ts::pointer old-tree)
                  (ts::pointer new-tree)
                  length-ptr))
         (num-ranges (cffi:mem-ref length-ptr :uint32))
         (root-node (ts:tree-root-node new-tree))
         (changed-nodes nil))
    ;; For each changed range, find the smallest node that contains it
    (dotimes (i num-ranges)
      (let* ((range (cffi:mem-aptr ranges '(:struct tsbind:ts-range) i))
             (start-byte (cffi:foreign-slot-value range '(:struct tsbind:ts-range) 'tsbind::start-byte))
             (end-byte (cffi:foreign-slot-value range '(:struct tsbind:ts-range) 'tsbind::end-byte))
             ;; Create a proper wrapped node instance
             (containing-node (make-instance 'ts:node
                                             :free #'ts::ts-node-delete
                                             :pointer (ts::ts-node-descendant-for-byte-range
                                                       (ts::pointer root-node)
                                                       start-byte
                                                       end-byte))))
        (push containing-node changed-nodes)))
    ;; Clean up allocated memory
    (cffi:foreign-free length-ptr)
    (cffi:foreign-free ranges)
    changed-nodes))


(defun make-ts-edit (&key start-byte old-end-byte new-end-byte
                          start-point old-end-point new-end-point)
  "Creates a ts-input-edit structure for Tree-sitter.
   Points should be provided as (cons row column)."
  (let ((edit (tsbind::foreign-alloc '(:struct tsbind:ts-input-edit))))
    ;; Set the byte positions
    (setf (tsbind::foreign-slot-value edit '(:struct tsbind:ts-input-edit) 'tsbind::start-byte) start-byte
          (tsbind::foreign-slot-value edit '(:struct tsbind:ts-input-edit) 'tsbind::old-end-byte) old-end-byte
          (tsbind::foreign-slot-value edit '(:struct tsbind:ts-input-edit) 'tsbind::new-end-byte) new-end-byte)
    ;; Set the point structures
    (let ((start (tsbind::foreign-slot-pointer edit '(:struct tsbind:ts-input-edit) 'tsbind::start-point)))
      (setf (tsbind::foreign-slot-value start '(:struct tsbind::ts-point) 'tsbind::row) (car start-point)
            (tsbind::foreign-slot-value start '(:struct tsbind::ts-point) 'tsbind::column) (cdr start-point)))

    (let ((old-end (tsbind::foreign-slot-pointer edit '(:struct tsbind:ts-input-edit) 'tsbind::old-end-point)))
      (setf (tsbind::foreign-slot-value old-end '(:struct tsbind::ts-point) 'tsbind::row) (car old-end-point)
            (tsbind::foreign-slot-value old-end '(:struct tsbind::ts-point) 'tsbind::column) (cdr old-end-point)))

    (let ((new-end (tsbind::foreign-slot-pointer edit '(:struct tsbind:ts-input-edit) 'tsbind::new-end-point)))
      (setf (tsbind::foreign-slot-value new-end '(:struct tsbind::ts-point) 'tsbind::row) (car new-end-point)
            (tsbind::foreign-slot-value new-end '(:struct tsbind::ts-point) 'tsbind::column) (cdr new-end-point)))
    edit))

(defun handle-edit (start end old-len)
  (let* ((buffer (lem:point-buffer start))
         (ts-tree (lem:buffer-value buffer :ts-tree))
         (start-byte (lem:point-bytes start))
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

    (handler-case
        (progn
          (tsbind:ts-tree-edit (ts::pointer ts-tree) edit)

          (let* ((text (lem:buffer-text (lem:point-buffer start)))
                 (new-tree (ts:parser-parse-string
                            (lem:buffer-value buffer :ts-parser)
                            text))
                 (changed-nodes (get-changed-nodes ts-tree new-tree)))

            ;; (lem:run-hooks (lem:make-per-buffer-hook :var lem-treesitter/buffer::*ts-edit-hook* :buffer buffer) changed-nodes buffer)
            (lem:run-hooks *ts-edit-hook* buffer changed-nodes)
            (setf (lem:buffer-value buffer :ts-tree) new-tree)))
      (error (c)
        (lem-treesitter/core::logm
         (format nil "Error in handle-edit: ~A" c))))))
