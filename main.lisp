(in-package :site-generator)

; Some utility functions for fun.
(defun take (n list)
  (unless (or (<= n 0) (null list))
    (cons (car list)
	  (take (1- n) (cdr list)))))
(defun restn (n list)
  (if (<= n 0)
      list
      (restn (1- n) (rest list))))
(defun group (n list)
  (when list
    (cons (take n list) (group n (restn n list)))))
(defun pair (list)
  (group 2 list))

(defun sane-stringify (item)
  (if (stringp item)
      item
      (string-downcase (format nil "~a" item))))

; This is where the HTML DSL starts...
(defun str-attributes (attributes)
  "Takes in an attribute list for an html tag, of the form: ((\"attr1\" \"val1\") (\"attr2\" \"val2\"))
   to represent the attribute string: attr1=\"val1\" attr2=\"val2\" ... in HTML."
  (format nil "~:{ ~a=~S~}" attributes))

(defun construct-tag (tagname content &optional (attributes nil))
  "Takes a tag name, the content for the tag, and a list of attributes. All should be strings. Returns a HTML-string for the tag.
   Content should be a list of format-able objects."
  (format nil "<~a~a>~{~a~}</~a>" tagname (str-attributes attributes) content tagname))

(defun br ()
  "<br />")

(defun construct-css (selector pairs)
  "Takes a selector, and a list of key:value pairs."
  (format nil "~a{~:{~a:~a;~}}" selector pairs))

; here starts the macros that turn the above code-generation code
; into a proper domain-specific language
(defmacro tag (name &body body)
  "Expands into a construct-tag form. All forms in name and attributes will be passed through sane-stringify first (NO EVALUATION).
   If the first element of the body is a list, it is assumed to be the attributes list.
   Everything else in body is always assumed to be the contents of the tag, and will be concatenated after being evaluated.
   After some blood, sweat and tears, symbols are now also evaluated, Except when they're attribute names.
   usage examples:
   (tag p :attributes (name \"my paragraph\") \"my string in paragraph.\")
   (tag p \"Hello world in an unnamed paragraph.\")
   If the first element of body is :attributes, the second element is taken to be a list of attributes."
  `(construct-tag ,(sane-stringify name)
		  (list ,@(if (eql (first body) :attrs)
			      (cddr body)
			      body))
		  (list ,@(if (eql (first body) :attrs)
			      (mapcar (lambda (l) `(list ,(sane-stringify (car l)) ,(cadr l)))
				      (pair (second body)))
			      nil))))

(defmacro css (&body body)
  "Body will be (pair)'ed, sobody should be like: selector (key1 value1 key2 value2) selector2 (key3 value3) repeated as many times as necessary."
  `(reduce (lambda (x y) (concatenate 'string x y))
	   ',(loop for i in (pair body)
		   collect
		   (construct-css (sane-stringify (first i))
				  (pair (loop for j in (second i) collect (sane-stringify j)))))))

; Okay, since we need to create shortcut macros to specific tags here
; to make things easier, and since the code is almost the same for all of them,
; i decided to make a macro that defines macros.
(defmacro automate-tags (&rest names)
  "Takes a list of names, creates a shortcut macro for each tag name in the list..
   This lets us type (p ...) instead of (tag p ...),
   and this macro lets us automate the process instead of declaring a new macro manually for every single tag."
  `(progn
     ,@(loop for name in names collect 
	     `(defmacro ,name (&body body)
		(let ((name ',name))
		  `(tag ,name ,@body))))))

(automate-tags
 html head body div span style
 codeblock
 p b i a h1 h2 h3 h4 h5 h6 h7)

(defun generate-from-file (fname &optional out-stream)
  "Generates an HTML site from a file written in our HTML DSL."
  (with-open-file (st fname)
    (princ (eval (read st)) out-stream)))

(defun generate-html (in out)
  (with-open-file (st out :direction :output :if-exists :supersede)
    (generate-from-file in st)))


(defvar *markdown-file* nil)
(defun main (template &rest content-files)
  "template should be a pathname to a .lisp file that contains the actual lisp code to be executed.
   The lisp code should be made with the html tag macros defined above (like html, body, p, i etc.)
   and should (when evaluated) generate html code.
   In template, there should be one form that generates html code from the current markdown file
   using the global variable *markdown-file* (like (process-markdown *markdown-file*)).
   Then, content-files will be a list of markdown files to be processed with that template."
  (loop for i in content-files do
    (let ((*markdown-file* i))
      (generate-html template (make-pathname :type "html" :defaults i)))))
