(in-package :site-generator)

(defmacro if-bind (pred then &optional else)
  "This is a somewhat important and yet very simple little macro.
   It's like if, except it binds the result of its predicate to a variable named 'it'"
  `(let ((it ,pred))
     (if it
	 ,then
	 ,else)))

(defun partial-match (str1 str2)
  (let ((len (min (length str1) (length str2))))
    (string= (subseq str1 0 len) (subseq str2 0 len))))

(defstruct ast-node
  value
  type)

(defun simple-scanner (text)
  (let ((tokens
	  `(("*" star)
	    ("`" backtick)
	    ("[" linktext-begin)
	    ("]" linktext-end)
	    ("(" link-begin)
	    (")" link-end)
	    ("#" hash)
	    (,(format nil "~%") newline))))
    (unless (string= text "")
      (loop for i in tokens
	    if (and (>= (length text) (length (car i)))
		    (partial-match text (car i)))
	      return (make-ast-node :value (car i) :type (cadr i))))))

(defun text-scanner (text)
  (let ((res (loop for i from 0 to (length text)
		   if (simple-scanner (subseq text i))
		     return (subseq text 0 i)
		   finally (return text))))
    (unless (string= res "")
      (make-ast-node :value res :type 'text))))

(defparameter *scanners*
  (list
   #'simple-scanner
   #'text-scanner))



(defun tokenize (text)
  "Recursively goes over the text to tokenize. Each token is an instance of ast-node"
  (labels ((next-token (text)
	     (some (lambda (x) (funcall x text))
		   *scanners*))
	   (main (text)
	     (let ((ret (next-token text)))
	       (if ret
		   (cons ret
			 (main (subseq text (length (ast-node-value ret)))))
		   (list (make-ast-node :value "" :type 'eof))))))
    (main text)))


; Beyond here lie the parsers.
; the "parsers" in this case, are what I call "match functions".
; a match function takes a list of tokens, and if they can match whatever
; they are designed to match, return a list containing (in order) the 
; ast-node containing the tokens matched, and the remaining tokens.

; general-match is a function that makes defining arbitrary match functions easier.
; general-match returns a list containing (first) a list of ast-nodes describing the matched things.
; (second) remaining tokens.

; make-single-matcher is a function that returns a new function that matches a single
; token of the given type.

(defun match-parser (tokens funs)
  "funs holds a list of parsers. This function returns the first matched one."
  (when (and tokens funs)
    (if-bind (funcall (car funs) tokens)
	     it
	     (match-parser tokens (cdr funs)))))

(defun general-match (tokens &rest to-match)
  "to-match should be a list of any of:
     - single match-functions (matches that function)
     - a list of said functions (matches any one of those functions)
     - a list of said functions that ends with a * symbol (matches any one of those functions 0 or more times)
   returns a list containing
     1- a list of nodes that were matched.
     2- a list of the remaining tokens."
  (labels ((recurse (tokens collected to-match)
	     (cond
	       ((null to-match) (list (reverse collected) tokens))
	       ; if a symbol or a function, we can call directly
	       ((or (symbolp (first to-match)) (functionp (first to-match))) 
		(if-bind (funcall (first to-match) tokens)
			 (recurse (second it) (cons (first it) collected) (rest to-match))))
	       ((listp (first to-match))
		(cond
		  ((eql '* (car (last (first to-match)))) ; star
		   (if-bind (match-parser tokens (butlast (first to-match)))
			    (if-bind (recurse (second it) (cons (first it) collected) to-match);match, keep trying to match
				     it ; keeping trying to match succeeded, return. 
				     (recurse tokens collected (rest to-match))); matching more was a mistake, move on.
			    (recurse tokens collected (rest to-match)))) ;no match, move on
		  (t (if-bind (match-parser tokens (first to-match)) ;no star
			      (recurse (second it) (cons (first it) collected) (rest to-match)) ;match, move on
			      nil ; no match, fail.
			      ))))
	       (t nil)
	       )))
    (recurse tokens nil to-match)))

(defun match-one (tokens type)
  (if (and tokens
	   (or (eql (ast-node-type (car tokens)) type)
	       (eql type 'any)))
      (list (car tokens) (cdr tokens))
      nil))

(defun make-single-matcher (type)
  (lambda (tokens)
    (match-one tokens type)))

; a lot of parsers just match a single token of a type, so 
; this makes it faster to define those.
(defmacro define-single-matcher (type)
  "symbol used for type will also be part of the name."
  `(defun ,(intern (string-upcase (format nil "~a-parser" type)))
       (tokens)
     (match-one tokens ',type)))

(define-single-matcher any)
(define-single-matcher newline)
(define-single-matcher eof)
(define-single-matcher text)
(define-single-matcher star)
(define-single-matcher hash)
(define-single-matcher backtick)

(defun code-block-parser (tokens)
  "Matches(/consumes) one backtick, then any and all tokens until another backtick (match includes the other backtick so that it is consumed).
   If no other backtick can be found, no match happens."
  (let* ((any-but-backtick (lambda (tokens)
			     (unless (backtick-parser tokens)
			       (any-parser tokens))))
	 (matched (general-match tokens
				 #'backtick-parser
				 (list any-but-backtick '*)
				 #'backtick-parser)))
    (when matched
      (list (make-ast-node :value (rest (butlast (first matched)))
			   :type 'code-block)
	    (second matched)))))


(defun italic-parser (tokens)
  "I know you can do it. You are smart. This thing we've built is beautiful, and it will be even more so once finished.
   So try your best. You're doing great so far, keep doing that. I love you."
  (if-bind (general-match tokens
			  #'star-parser
			  '(code-block-parser link-parser bold-parser text-parser *)
			  #'star-parser)
	   (list (make-ast-node :value (rest (butlast (first it)))
				:type 'italic)
		 (second it))
	   nil))

(defun bold-parser (tokens)
  (if-bind (general-match tokens
			  #'star-parser #'star-parser
			  '(code-block-parser link-parser italic-parser text-parser *)
			  #'star-parser #'star-parser)
	   (list (make-ast-node :value (cddr (butlast (first it) 2))
				:type 'bold)
		 (second it))
	   nil))

(defun link-text-parser (tokens)
  (let ((opening (make-single-matcher 'linktext-begin))
	(end (make-single-matcher 'linktext-end)))
    (if-bind (general-match tokens
			    opening
			    '(code-block-parser bold-parser italic-parser text-parser *)
			    end)
	     (list (make-ast-node :value (rest (butlast (first it)))
				  :type 'link-text)
		   (second it))
	     nil)))

(defun link-link-parser (tokens)
  (let ((opening (make-single-matcher 'link-begin))
	(any-but-end (lambda (ts)
		       (unless (match-one ts 'link-end)
			 (any-parser ts))))
	(end (make-single-matcher 'link-end)))
    (if-bind (general-match tokens
			    opening
			    (list any-but-end '*)
			    end)
	     (list (make-ast-node :value (rest (butlast (first it))) :type 'link-link)
		   (second it))
	     nil)))

(defun link-parser (tokens)
  (if-bind (general-match tokens
			  'link-text-parser
			  'link-link-parser)
	   (list (make-ast-node :value (first it) :type 'link)
		 (second it))
	   nil))


(defun paragraph-parser (tokens)
  "Parses an entire paragraph. A paragraph may be made up of any combination these elements:
   code blocks, bold text, italic text, regular text, links... (more to come!)"
  (let* ((two-newlines (lambda (ts)
			 (if-bind (general-match ts 'newline-parser 'newline-parser)
				  (list (make-ast-node :value (format nil "~%~%") :type 'newline)
					(second it)))))
	 (not-two-newlines (lambda (ts)
			     (unless (funcall two-newlines ts)
			       (if-bind (general-match ts 'any-parser)
					(list (make-ast-node :value (first it) :type 'any)
					      (second it)))))))
    (if-bind (general-match tokens
			    `(code-block-parser link-parser bold-parser italic-parser text-parser ,not-two-newlines *)
			    `(,two-newlines eof-parser))
	     (list (make-ast-node :value (butlast (first it)) :type 'paragraph)
		   (second it))
	     nil)))

(defun skip (list n)
  (if (<= n 0)
      list
      (skip (cdr list) (1- n))))

(defun h-parser (tokens n)
  (if-bind (apply #'general-match tokens
		  (append (loop for i from 1 to n collect #'hash-parser)
			  '((code-block-parser link-parser bold-parser italic-parser text-parser *)
			    (newline-parser eof-parser))))
	   (list (make-ast-node :value (butlast (skip (first it) n))
				:type (intern (format nil "H~a" n)))
		 (second it))
	   nil))


(defun header-parser (tokens)
  "does a header."
  (some #'identity
	(mapcar (lambda (n)
		  (h-parser tokens n))
		(loop for i from 1 to 6 collect i))))
	     

(defun keep-parsing (tokens)
  (if-bind (match-parser tokens '(header-parser paragraph-parser))
	   (cons (first it) (keep-parsing (second it)))
	   nil))

(defun conc-strs (l)
  "Concatenate a list of strings."
  (apply #'concatenate 'string l))

(defun eval-markdown (ast)
  "After the markdown source is processed into an abstract syntax tree, this function generates html from it."
  (let ((default (lambda (ss) (conc-strs ss)))
	(evaluators `((text . ,(lambda (ss) (conc-strs ss)))
		      (italic . ,(lambda (ss) (i (conc-strs ss))))
		      (bold . ,(lambda (ss) (b (conc-strs ss))))
		      (code-block . ,(lambda (ss) (code (conc-strs ss))))
		      (newline . ,(lambda (ss) (declare (ignore ss)) " "))
		      (eof . ,(lambda (ss) (declare (ignore ss)) ""))
		      (link-text . ,(lambda (ss) (conc-strs ss)))
		      (link-link . ,(lambda (ss) (conc-strs ss)))
		      (link . ,(lambda (ss) (a :attrs (href (second ss)) (first ss))))
		      (paragraph . ,(lambda (ss) (p (conc-strs ss))))
		      (h1 . ,(lambda (ss) (h1 (conc-strs ss))))
		      (h2 . ,(lambda (ss) (h2 (conc-strs ss))))
		      (h3 . ,(lambda (ss) (h3 (conc-strs ss))))
		      (h4 . ,(lambda (ss) (h4 (conc-strs ss))))
		      (h5 . ,(lambda (ss) (h5 (conc-strs ss))))
		      (h6 . ,(lambda (ss) (h6 (conc-strs ss)))))))
    (funcall (or (cdr (assoc (ast-node-type ast) evaluators)) default)
	     (if (stringp (ast-node-value ast))
		 (list (ast-node-value ast))
		 (mapcar #'eval-markdown (ast-node-value ast))))))

(defun read-file (f)
  (with-open-file (st f)
    (labels ((r () (read-line st nil nil))
	     (c (col l)
	       (if l
		   (c (cons l col) (r))
		   (reverse col))))
      (format nil "~{~a~%~}" (c nil (r))))))

(defun process-markdown-string (string)
  (reduce (lambda (x y) (concatenate 'string x y))
	  (mapcar #'eval-markdown
		  (keep-parsing (tokenize string)))))

(defun process-markdown (file)
  "Parses a markdown file into my own HTML DSL, which can then easily be converted to HTML."
  (let ((file-contents (read-file file)))
    (process-markdown-string file-contents)))
    
