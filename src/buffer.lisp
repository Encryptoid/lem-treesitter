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
  (lem-treesitter/core:ensure-treesitter lang)
  (let* ((parser (lem-treesitter/core::ensure-parser lang))
         (text (lem:buffer-text buffer))) ; Changed from (lem:current-buffer) to buffer
    (let ((tree (ts:parser-parse-string parser text)))
      (unless tree
        (error "Failed to parse buffer text"))
      (setf (lem:buffer-value buffer :ts-tree) tree)
      (setf (lem:buffer-value buffer :ts-parser) parser))
  (lem:message "Stored treesitter buffer info. Lang: ~a" lang)
  (lem-treesitter/core::logm (format nil "store-buffer-info received lang: ~A. Text: ~%~a" lang text)))
  (lem:add-hook (lem:variable-value 'lem/buffer/internal:after-change-functions :buffer buffer) 'handle-edit))

;(lem:variable-value 'lem/buffer/internal:after-change-functions :buffer (lem:get-buffer "all.py"))


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
  (lem-treesitter/core::logm "Starting handle-edit")
  (let* ((buffer (lem:point-buffer start))
         (ts-tree (lem:buffer-value buffer :ts-tree))
         (start-byte (lem:point-bytes start))
         (end-byte (lem:point-bytes end))
         (start-point (cons (lem/buffer/internal::point-linum start)
                            (lem/buffer/internal::point-charpos start)))
         (end-point (cons (lem/buffer/internal::point-linum end)
                          (lem/buffer/internal::point-charpos end))))

    (lem-treesitter/core::logm
     (format nil "Edit details: start-byte=~A end-byte=~A old-len=~A start-point=~A end-point=~A"
             start-byte end-byte old-len start-point end-point))

    (unless ts-tree
      (lem-treesitter/core::logm "No tree-sitter tree found for buffer")
      (return-from handle-edit))

    (handler-case
        (let ((edit (make-ts-edit
                     :start-byte start-byte
                     :old-end-byte (+ start-byte old-len)
                     :new-end-byte end-byte
                     :start-point start-point
                     :old-end-point start-point  ; should this be different for single char?
                     :new-end-point end-point)))

          (tsbind:ts-tree-edit (ts::pointer ts-tree) edit)
          (lem-treesitter/core::logm "Applied tree edit")

          (let* ((text (lem:buffer-text buffer))
                 (parser (lem:buffer-value buffer :ts-parser))
                 (new-tree (ts:parser-parse-string parser text))
                 (_ (lem-treesitter/core::logm "Parsed new tree"))
                 (changed-nodes (get-changed-nodes ts-tree new-tree)))

            (lem-treesitter/core::logm
             (format nil "Found ~A changed nodes" (length changed-nodes)))

            (lem:run-hooks *ts-edit-hook* buffer changed-nodes)
            (setf (lem:buffer-value buffer :ts-tree) new-tree)))
      (error (c)
        (lem-treesitter/core::logm
         (format nil "Error in handle-edit: ~A" c))))))


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
    (lem-treesitter/core::logm (format nil "num-ranges: ~a" num-ranges))

     (if (zerop num-ranges)
        (let* ((point (lem:current-point)) ;; TODO Remove current-point here
               (byte-pos (lem:point-bytes point))
               (node (make-instance 'ts:node
                                    :free #'ts::ts-node-delete
                                    :pointer (ts::ts-node-descendant-for-byte-range
                                              (ts::pointer root-node)
                                              ;; (max 0 byte-pos)
                                              ;; (+ byte-pos 1)))))      ; TODO Check max file
                                              (max 0 (- byte-pos 1))
                                              byte-pos))))
          (when node
            (push node changed-nodes)))

        (dotimes (i num-ranges)
          (let* ((range (cffi:mem-aptr ranges '(:struct tsbind:ts-range) i))
                 (start-byte (cffi:foreign-slot-value range '(:struct tsbind:ts-range) 'tsbind::start-byte))
                 (end-byte (cffi:foreign-slot-value range '(:struct tsbind:ts-range) 'tsbind::end-byte))
                 (node (make-instance 'ts:node
                                      :free #'ts::ts-node-delete
                                      :pointer (ts::ts-node-descendant-for-byte-range
                                                (ts::pointer root-node)
                                                ;; (max 0 (- start-byte 1))
                                                ;; (max 0 (- end-byte 1))))))
                                                start-byte
                                                end-byte))))
            (when node
              (push node changed-nodes)))))

    (cffi:foreign-free length-ptr)
    (when ranges (cffi:foreign-free ranges))
    changed-nodes))
