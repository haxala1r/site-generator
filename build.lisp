(require 'asdf)
(load "package.lisp")
(asdf:load-asd (truename #p"./site-generator.asd"))

#+ecl
(asdf:make-build :site-generator
		 :type :program
		 :move-here #p"./"
		 :epilogue-code '(site-generator::entry-point))

; for some reason this doesn't really work as nicely with ecl,
; so we use ecl's own interface in asdf above
; and the regular make operation on every other implementation.
#-ecl
(asdf:make :site-generator)
