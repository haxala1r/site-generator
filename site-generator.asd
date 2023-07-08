(require :asdf)
(asdf:defsystem :site-generator
  :description "A Static Website Generator that can compile markdown and generate HTML & CSS from it."
  :depends-on (:uiop :asdf)
  :components ((:file "package")
               (:file "main" :depends-on ("package"))
               (:file "markdown" :depends-on ("main"))))
