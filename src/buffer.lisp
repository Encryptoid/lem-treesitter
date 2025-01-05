(defpackage :lem-treesitter/buffer
  (:use :cl)
  (:local-nicknames (:ts :treesitter))
  (:export :*ts-buffer-info*))
(in-package :lem-treesitter/buffer)

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

;; Only support a singular buffer for now to play with
(defvar *ts-buffer-info* nil)

(defun load-ts-buffer (buffer)
  "Load the BUFFER into a ts-buffer-info structure for querying"
  (when *ts-buffer-info*
    (when (slot-boundp *ts-buffer-info* 'cursor)
      (ts::ts-tree-cursor-delete (ts::pointer (buffer-cursor *ts-buffer-info*))))
    (when (slot-boundp *ts-buffer-info* 'tree)
      (ts::ts-tree-delete (ts::pointer (buffer-tree *ts-buffer-info*)))))
  (let* ((file-path (lem/buffer/internal::buffer-%filename buffer))
         (contents (uiop:read-file-string file-path))
         (parser (ts:make-parser :language lem-treesitter/core:*commonlisp*))
         (tree (ts:parser-parse-string parser contents))
         (info (make-instance 'ts-buffer-info)))

    (setf (buffer-parser info) parser
          (buffer-source info) contents
          (buffer-tree info) tree
          (buffer-cursor info) (ts:make-cursor (ts:tree-root-node tree))
          (buffer-root-node info) (ts:tree-root-node tree)
          (buffer-last-parse-time info) (get-universal-time)
          (buffer-last-position info) 0
          (buffer-last-node info) (ts:tree-root-node tree))

    ;; Store in global var
    (setf *ts-buffer-info* info)
    (lem:message "Loaded buffer ~A (~A bytes)"
                 (file-namestring file-path)
                 (length contents))))

(lem:define-command ts-load-buffer () ()
  (load-ts-buffer (lem:current-buffer)))
