(require 'asdf)
(load "package.lisp")
(asdf:load-asd "site-generator.asd")
(asdf:make-build :site-generator
		 :type :program
		 :move-here #p"./"
		 :epilogue-code '(progn
				   (in-package :site-generator)
				   (let ((args (uiop:raw-command-line-arguments)))
				     (if (> (length args) 2)
				       (apply #'site-generator:main (rest (uiop:raw-command-line-arguments)))
				       (format t "Usage: ~a template-file {one or more markdown files...}" (first args))))))
