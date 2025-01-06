(ql:quickload :lem)
(ql:quickload :rove)
(ql:quickload :lem-fake-interface)
(ql:quickload :lem-vi-mode/tests)
(ql:quickload :cl-treesitter)
(defpackage :lem-treesitter/tests
  (:use :cl
        :lem
        :rove)
  (:local-nicknames (:ts :treesitter))
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-treesitter/tests)

(make-test-buffer "hi" :content "lee")

(defun make-test-buffer (name &rest buffer-args
                              &key content (temporary t temporary-specified-p)
                              &allow-other-keys)
  (declare (ignore temporary))
  (unless temporary-specified-p
    (setf (getf buffer-args :temporary) t))
  (alexandria:remove-from-plistf buffer-args :name :content)

  (let ((buffer (apply #'make-buffer name buffer-args)))
    (when content
      (multiple-value-bind (buffer-text position visual-regions)
          (lem-vi-mode/tests/utils::parse-buffer-string content)
        (let ((point (buffer-point buffer)))
          (lem:insert-string point buffer-text)
          (lem:clear-buffer-edit-history buffer)
          (when position
            (move-to-position point position))
          (when visual-regions
            (let ((top-left-pos (car (first visual-regions)))
                  (bot-right-pos (cdr (car (last visual-regions)))))
              (with-point ((p point))
                (move-to-position p
                                  (if (= position top-left-pos)
                                      (1- bot-right-pos)
                                      top-left-pos))
                (setf lem-vi-mode/visual::*start-point* p))))
          (dolist (region visual-regions)
            (destructuring-bind (from . to) region
              (with-point ((start point)
                           (end point))
                (move-to-position start from)
                (move-to-position end to)
                (push (lem:make-overlay start end 'lem:region)
                      lem-vi-mode/visual::*visual-overlays*)))))))
    buffer))

(defun make-edit (start-byte old-end-byte new-end-byte
                  start-row start-col
                  old-end-row old-end-col
                  new-end-row new-end-col)
  "Create a ts-input-edit struct for node editing."
  (let ((edit (foreign-alloc '(:struct ts-input-edit))))
    ;; Set the byte positions
    (setf (foreign-slot-value edit '(:struct ts-input-edit) 'start-byte) start-byte
          (foreign-slot-value edit '(:struct ts-input-edit) 'old-end-byte) old-end-byte
          (foreign-slot-value edit '(:struct ts-input-edit) 'new-end-byte) new-end-byte)

    ;; Create and set the point structs
    (with-foreign-slots ((start-point old-end-point new-end-point)
                         edit (:struct ts-input-edit))
      ;; Start point
      (setf (foreign-slot-value start-point '(:struct ts-point) 'row) start-row
            (foreign-slot-value start-point '(:struct ts-point) 'column) start-col)
      ;; Old end point
      (setf (foreign-slot-value old-end-point '(:struct ts-point) 'row) old-end-row
            (foreign-slot-value old-end-point '(:struct ts-point) 'column) old-end-col)
      ;; New end point
      (setf (foreign-slot-value new-end-point '(:struct ts-point) 'row) new-end-row
            (foreign-slot-value new-end-point '(:struct ts-point) 'column) new-end-col))
    edit))


(deftest test-ts-edit
  (with-fake-interface ()
    ;; (with-test-buffer (buffer (make-text-buffer (lem-fake-interface::lines "(defun add (arg1 arg2)" "  (+ 1 1))")))
    (lem-vi-mode/tests/utils:with-vi-buffer (?#"(defun add (arg1 arg2)" "  (+ 1 1))")
        (let ((edit (make-edit (7 9 9 0 7 0 9 0 9))))
        )
      )))
