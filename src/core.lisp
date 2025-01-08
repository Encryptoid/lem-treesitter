(in-package :lem-treesitter/core)

(defun join-directory (base-directory-pathname &rest sub-directory-path)
  (make-pathname :directory (append (pathname-directory base-directory-pathname)
                                    sub-directory-path)))

;; TODO: Is there a better way to get this dir
(defvar *lem-treesitter-bin* (uiop:parse-native-namestring (uiop:native-namestring "~/.local/share/lem/treesitter/bin") :ensure-directory t))

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

;(defmacro ensure-treesitter (&key lang)
;  (let ((lang-str (string-downcase (symbol-name lang))))
;    `(progn
;       (treesitter:include-language ,lang-str :search-path *lem-treesitter-bin*)
;       (setf (gethash ,lang *treesitters*) (treesitter:make-language ,lang-str)))))

;(defun ensure-treesitter (lang)
;  (let ((lang-str (string-downcase (symbol-name lang))))
;    (treesitter:include-language lang-str :search-path *lem-treesitter-bin*)
;    (setf (gethash lang *treesitters*) (treesitter:make-language lang-str))))

(defun ensure-treesitter (lang)
  (let ((lang-str (string-downcase lang)))
    (lem:message "Loading language: ~A" lang-str)
    (include-treesitter lang-str :search-path *lem-treesitter-bin*)
    (setf (gethash lang *treesitters*)
          (treesitter:make-language lang-str))))

(defun include-treesitter (lang &key search-path)
  (let* ((lang-str (string-downcase (etypecase lang
                                      (symbol (symbol-name lang))
                                      (string lang))))
         (lisp-name (intern (format nil "~:@(TREE-SITTER-~a~)" lang-str) :treesitter)))

    (cffi:load-foreign-library
     (format nil "libtree-sitter-~a.so" lang-str)
     :search-path (or search-path *lem-treesitter-bin* ts:*language-path*))

    ;; TODO Any other way to do this?
    (eval `(cffi:defcfun (,(format nil "tree_sitter_~a" lang-str) ,lisp-name) :pointer))

    (setf (gethash lang ts:*languages*) lisp-name))
  lang)

;(defun ensure-parser (lang)
;  (unless (gethash lang *parsers*)
;    (setf (gethash lang *parsers*)
;          (treesitter:make-parser :language (gethash lang *treesitters*))))
;  (gethash lang *parsers*))

(defun ensure-parser (lang)
  (lem-treesitter/core::logm (format nil "Ensuring parser for lang: ~A" lang))
  (let ((parser (gethash lang *parsers*)))
    (unless parser
      (lem-treesitter/core::logm (format nil "Creating new parser for ~A" lang))
      (let ((ts-lang (gethash lang *treesitters*)))
        (unless ts-lang
          (error "No treesitter language found for ~A" lang))
        (setf parser (treesitter:make-parser :language ts-lang))
        (setf (gethash lang *parsers*) parser)))
    (lem-treesitter/core::logm (format nil "Returning parser: ~A" parser))
    parser))
