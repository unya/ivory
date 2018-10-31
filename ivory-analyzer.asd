;;;; ivory-analyzer.asd

(asdf:defsystem #:ivory-analyzer
  :description "Describe ivory-analyzer here"
  :author "Your Name <your.name@example.com>"
  :depends-on (#:cl-intbytes #:alexandria #:serapeum)
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "word")
               (:file "memory")
               (:file "ivory-analyzer")))
