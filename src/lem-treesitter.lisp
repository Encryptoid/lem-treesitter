;(in-package :lem-user)
(in-package :lem-treesitter)

(lem-user::define-minor-mode treesitter-mode
    (:name "Trailing Spaces"
     :global t
     :enable-hook 'enable
     :disable-hook 'disable))

;(format t "~a" lem/buffer/file:*find-file-hook*)
(defun enable ()
  (lem:add-hook lem/buffer/file:*find-file-hook* 'init-treesitter)
  (lem:message "Treesitter Enabled!"))

;; TODO worth using hashmap?
(defparameter *supported-langs*
  '((lem-python-mode:python-mode . "python")))

;; (defgeneric init-mode (mode-str)
;;   (:method (mode-str)
;;     (lem:message "No treesitter for mode: ~A" mode-str)))

;; Job of init-mode:
;;  - Setup base parsing function?
;;  - Setup list of additional queries
;;    - Load from .scm
;;    - Accept user/ext scm
;;  - TODO: Should subscribing to changes be in or out of here?
(defgeneric init-mode (mode)
  (:method (mode)
    (lem:message "No treesitter for mode: ~A" mode)))

(defun init-treesitter (buffer)
  (let* ((mode (lem:buffer-major-mode buffer)) ;; Returns a symbol
        (lang (cdr (assoc mode *supported-langs*))))
  (unless lang
    (return-from init-treesitter))
    (lem-treesitter/buffer:store-buffer-info buffer lang)
    ;; (init-mode lang)
    (lem:message "mode: ~a" (describe mode t))
    (init-mode mode)
    (lem-treesitter/ext/highlight:init-highlighting buffer)))

(defun disable ()
  (lem:message "Treesitter Disabled"))

(lem:define-command ts-hl-buffer () ()
  (init-treesitter (lem:current-buffer)))

;; These modes look correct
(lem-core::detect-file-mode (lem:current-buffer))
(lem:buffer-major-mode (lem:current-buffer))
