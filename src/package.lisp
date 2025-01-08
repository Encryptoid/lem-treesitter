(defpackage :lem-treesitter/core
  (:use :cl)
  (:local-nicknames (:ts :treesitter))
  (:export :ensure-treesitter))

(defpackage :lem-treesitter/buffer
  (:use :cl)
  (:local-nicknames (:ts :treesitter)
                    (:tsbind :treesitter/bindings))
  (:export :*ts-buffer-info*
           :ts-buffer-info
           :buffer-source
           :buffer-info
           :store-buffer-info))

(defpackage :lem-treesitter/query
  (:use :cl)
  (:local-nicknames (:ts :treesitter)))

(defpackage :lem-treesitter/parser
  (:use :cl)
  (:local-nicknames (:ts :treesitter))
  )

(defpackage :lem-treesitter/ext/highlight
  (:use :cl)
  (:local-nicknames (:ts :treesitter))
  (:export :init-highlighting))

(defpackage :lem-treesitter
  (:use :cl :treesitter)
  (:local-nicknames (:ts :treesitter)))

(defpackage :lem-treesitter/python
  (:use :cl :lem-treesitter)
  (:local-nicknames (:ts :treesitter)
                    (:ts-bind :treesitter/bindings))
  (:export :*commonlisp*))
