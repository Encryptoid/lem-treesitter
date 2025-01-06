(defpackage :lem-treesitter/core
  (:use :cl)
  (:local-nicknames (:ts :treesitter))
  (:export :*commonlisp*))
(in-package :lem-treesitter/core)

(defun join-directory (base-directory-pathname &rest sub-directory-path)
  (make-pathname :directory (append (pathname-directory base-directory-pathname)
                                    sub-directory-path)))

;; TODO: Is there a better way to get this dir
(defvar *lem-cache-dir* (uiop:parse-native-namestring (uiop:native-namestring "~/.local/share/lem") :ensure-directory t))
(defvar *lem-treesitter-bin* (join-directory *lem-cache-dir* "treesitter" "bin"))

(defvar *treesitters* (make-hash-table :test #'equal))
(defvar *parsers* (make-hash-table :test #'equal))

(defmacro load-treesitter (lang)
  (let ((lang-str (string-downcase (symbol-name lang))))
    `(progn
       (ts:include-language ,lang-str :search-path *lem-treesitter-bin*)
       (setf (gethash ,lang *treesitters*) (treesitter:make-language ,lang-str)))))

(defmacro load-ts-parser (lang)
  `(let ((ts-lang (load-treesitter ,lang)))
     (ts:make-parser :language ts-lang)))

;(load-ts-parser :commonlisp)

(defmacro mname (lang)
  (let ((lang-str (string-downcase (symbol-name lang))))
    `(format nil "~A" ,lang-str)))


(defmacro ensure-treesitter (lang)
  (let ((lang-str (string-downcase (symbol-name lang))))
    `(progn
       (treesitter:include-language ,lang-str :search-path *lem-treesitter-bin*)
       (setf (gethash ,lang *treesitters*) (treesitter:make-language ,lang-str)))))

(defun ensure-parser (language)
  (unless (gethash language *parsers*)
    (setf (gethash language *parsers*)
          (treesitter:make-parser :language (gethash language *treesitters*))))
  (gethash language *parsers*))
