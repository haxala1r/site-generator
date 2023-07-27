(require :asdf)
(asdf:defsystem :site-generator
  :description "A Static Website Generator that can compile markdown and generate HTML & CSS from it."
  :build-operation program-op
  :build-pathname "site-generator"
  :entry-point "site-generator:entry-point"
  :depends-on (:uiop :asdf :cl-ppcre)
  :components ((:file "package")
               (:file "main" :depends-on ("package"))
               (:file "markdown" :depends-on ("main"))))
