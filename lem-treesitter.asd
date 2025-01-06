(asdf:defsystem "lem-treesitter"
  :description "Describe lem-treesitter here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on ("lem" "cl-treesitter" "cffi")
  :serial t
  :components (
               ;(:file "test")
               (:module "src"
               :serial t
               :components ((:file "core")
                            (:file "buffer")
                            (:file "query")
                            (:file "utils")
                            (:file "lem-treesitter")
                            (:file "printer")
                            (:file "playground")
                            ))))
