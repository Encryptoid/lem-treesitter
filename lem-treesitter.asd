(asdf:defsystem "lem-treesitter"
  :description "Describe lem-treesitter here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on ("lem" "cl-treesitter" "cffi")
  :serial t
  :components (
               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "core")
                             (:file "printer")
                             (:file "buffer")
                             (:file "query")
                             (:file "parser")
                             (:file "ext/highlight")
                             (:file "lem-treesitter")
                             (:file "langs/python")
                             ))
               ))
