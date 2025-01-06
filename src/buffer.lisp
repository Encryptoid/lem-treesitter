(defpackage :lem-treesitter/buffer
  (:use :cl)
  (:local-nicknames (:ts :treesitter))
 (:export :*ts-buffer-info*
           :ts-buffer-info
           :buffer-source
           :buffer-info))
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

;(defun ts-load-buffer (buffer lang)
;  (let* ((text (lem:buffer-text buffer))
;         (tree (ts:parser-parse-string (lem-treesitter/core::load-ts-parser lang) text)))
;    (setf (lem:buffer-value :ts-tree tree) tree)))

(defun ts-parse-text (text)
  (lem-treesitter/core::ensure-treesitter :c_sharp)
  (let ((parser (lem-treesitter/core::ensure-parser :c_sharp)))
    (ts:parser-parse-string parser text)))

(defun ts-parse-text (text)
  (lem-treesitter/core::ensure-treesitter :commonlisp)
  (let ((parser (lem-treesitter/core::ensure-parser :commonlisp)))
    (ts:parser-parse-string parser text)))

(defun ts-parse-text (text)
  (lem-treesitter/core::ensure-treesitter :python)
  (let ((parser (lem-treesitter/core::ensure-parser :python)))
    (ts:parser-parse-string parser text)))

(defun ts-load-buffer (buffer)
  (let* ((text (lem:buffer-text (lem:current-buffer)))
         (tree (ts-parse-text text)))
    (setf (lem:buffer-value buffer :ts-tree) tree)))

(lem:define-command ts-init-buffer () ()
  (ts-load-buffer (lem:current-buffer)))
