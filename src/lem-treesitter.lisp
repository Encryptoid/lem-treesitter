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

(defun init-treesitter (buffer)
  (let ((lang (cdr (assoc (lem-core::detect-file-mode buffer) *supported-langs*))))
  (unless lang
    (return-from init-treesitter))
    (lem-treesitter/buffer:store-buffer-info buffer lang)
  (lem-treesitter/ext/highlight:init-highlighting buffer)))

(defun disable ()
  (lem:message "Treesitter Disabled"))

(lem:define-command ts-hl-buffer () ()
  (init-treesitter (lem:current-buffer)))
