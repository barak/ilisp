;;;; guile-ilisp.scm --- ILISP support functions for GUILE
;;;; Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de> 

;;; This file is part of ILISP.
;;; Version: 5.10.1
;;;
;;; Copyright (C) 2000 Matthias Koeppe
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'majordomo@cons.org' to be included in the
;;; ILISP mailing list. 'ilisp@cons.org' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.


(define-module (guile-user)
  :use-module (guile-ilisp))

(define-module (guile-ilisp)
  :use-module (ice-9 debug)
  :use-module (ice-9 session)
  :use-module (ice-9 documentation)
  :use-module (ice-9 regex))

(define (read-from-string str)
  (call-with-input-string str read))

(define (read-from-string/source str filename line column)
  "Read from string STR, pretending the source is the given FILENAME, LINE, COLUMN."
  (call-with-input-string
   str
   (lambda (port)
     (set-port-filename! port filename)
     (set-port-line! port (- line 1))
     (set-port-column! port (- column 1))
     (read port))))

(define (string->module str)
  (let ((v (call-with-input-string str read)))
    (cond
     ((eq? 'nil v) (current-module))
     ((pair? v) (resolve-module v))
     (else (resolve-module (list v))))))

(define (first-line s)
  (let ((i (string-index s #\newline)))
    (if i
	(substring s 0 i)
	s)))

(define (doc->arglist doc with-procedure?)
  "Parse DOC to find the arglist and return it as a string.  If
WITH-PROCEDURE?, include the procedure symbol."
  (let ((pattern " - primitive: "))
    (cond
     ((string=? (substring doc 0 (string-length pattern))
		pattern)
      ;; Guile 1.4.1 primitive procedure documentation, passed through
      ;; TeXinfo:
      ;;
      ;;  - primitive: assoc key alist
      ;;     Behaves like `assq' but uses `equal?' for key comparison.
      ;;
      (let ((index
	     (if with-procedure?
		 (string-length pattern)
		 (1+ (string-index doc #\space
				   (string-length pattern))))))
	(string-append "("
		       (first-line (substring doc index))
		       ")")))
     ((string=? (substring doc 0 1) "(")
      ;; Guile <= 1.4 primitive procedure documentation and other
      ;; conventions:
      ;;
      ;; (help [NAME])
      ;; Prints useful information.  Try `(help)'.
      ;;
      (if with-procedure?
	  (first-line doc)
	  (let ((index (string-index doc #\space)))
	    (if index
		(string-append "("
			       (substring (first-line doc)
					  (+ index 1)))
		"()"))))     
     (else (string-append "CAN'T PARSE THIS DOCUMENTATION:\n"
			  doc)))))

(define (info-message sym package expensive? arglist-only?)
  "Evaluate SYM in PACKAGE and return an informational message about
the value.  For procedures, return procedure symbol and arglist, or
fall back to a message on the arity; if ARGLIST-ONLY?, return the
arglist only.  If EXPENSIVE?, take some more effort."
  ;; The code here is so lengthy because we want to return a
  ;; meaningful result even if we aren't allowed to read the
  ;; documentation files (EXPENSIVE? = #f).
  (let ((obj (catch #t
		    (lambda ()
		      (eval-in-package sym
				       (string->module package)))
		    (lambda args
		      #f))))
    (cond
     ((closure? obj)
      (let ((formals (cadr (procedure-source obj))))
	(if arglist-only? formals (cons sym formals))))
     ((or
       (and expensive?
	    (false-if-exception
	     ;; object-documentation was introduced in Guile 1.4,
	     ;; There is no documentation for primitives in earlier
	     ;; versions.
	     (object-documentation obj)))
       (and (procedure? obj)
	    (procedure-property obj 'documentation)
	    ;; The documentation property is attached to a primitive
	    ;; procedure when it was read from the documentation file
	    ;; before.
	    ))
      => (lambda (doc)
	   (doc->arglist doc (not arglist-only?))))
     ((and (macro? obj)
	   (macro-transformer obj)
	   (closure? (macro-transformer obj))
	   (procedure-documentation (macro-transformer obj)))
      ;; Documentation may be in the doc string of the transformer, as
      ;; is in session.scm (help).
      => (lambda (doc)
	   (doc->arglist doc (not arglist-only?))))
     ((procedure? obj)
      ;; Return a message about the arity of the procedure.
      (with-output-to-string
	(lambda () (arity obj))))
     (else #f))))

(define-public (ilisp-print-info-message sym package)
  "Evaluate SYM in PACKAGE and print an informational message about
the value.  For procedures, the arglist is printed.
This procedure is invoked by the electric space key."
  (cond
   ((info-message sym package #f #f)
    => (lambda (message)
	 (display message)
	 (newline)))))

(define-public (ilisp-arglist symbol package)
  "Evaluate SYMBOL in PACKAGE and print the arglist if we have a
procedure. This procedure is invoked by `arglist-lisp'."  
  (cond
   ((info-message symbol package #t #t)
    => (lambda (message)
	 (display message)
	 (newline)))
   (else
    (display "Can't get arglist.")
    (newline))))

(define (word-separator? ch)
  (or (char=? ch #\-)
      (char=? ch #\:)
      (char=? ch #\_)
      (char=? ch #\/)))

(define (string-pred-rindex str pred)
  (let loop ((index (- (string-length str) 1)))
    (cond
     ((negative? index) #f)
     ((pred (string-ref str index)) index)
     (else (loop (- index 1))))))

(define (separate-fields-before-predicate pred str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-pred-rindex str pred)
      => (lambda (w) (loop (cons (make-shared-substring str w) fields)
			   (make-shared-substring str 0 w))))
     (else (apply ret str fields)))))

(define (make-word-regexp str)
  (apply string-append
	 (cons "^"
	       (map (lambda (word)
		      (string-append (regexp-quote word) "[^-:/_]*"))
		    (separate-fields-before-predicate word-separator?
						      str list)))))	      

(define-public (ilisp-matching-symbols string package function? external? prefix?)
  (map (lambda (sym) (list (symbol->string sym)))
       (let ((regexp (if (eq? prefix? 't)
			 (string-append "^" (regexp-quote string))
			 (make-word-regexp string)))
	     (a-i apropos-internal))
	 (save-module-excursion
	  (lambda ()
	    (set-current-module (string->module package))
	    (a-i regexp))))))

(define (last l)
  (cond ((and (pair? l) (not (null? (cdr l))))
	 (last (cdr l)))
	(else (car l))))

(define eval-in-package
  ;; A two-argument version of `eval'
  (if (= (car (procedure-property eval 'arity)) 2)
      (lambda (expression environment)	; we have a R5RS eval
	(save-module-excursion
	 (lambda ()
	   (eval expression environment))))
      (lambda (expression environment)	; we have a one-arg eval (Guile <= 1.4)
	(save-module-excursion
	 (lambda ()
	   (set-current-module environment)
	   (eval expression))))))  

(define-public (ilisp-get-package sequence-of-defines)
  "Get the last module name defined in the sequence of define-module forms."
  ;; First eval the sequence-of-defines.  This will register the
  ;; module with the Guile interpreter if it isn't there already.
  ;; Otherwise `resolve-module' will give us a bad environment later,
  ;; which just makes trouble.
  (let ((module-last-name
	 (eval-in-package 
	  (append sequence-of-defines
		  '(module-name (current-module)))
	  (string->module "(guile-user)"))))
    ;; Now we have the name of the module -- but only the last
    ;; component.  No idea how to get the full one; so we need to
    ;; "parse" the sequence-of-defines ourselves.
    (let ((last-form (last sequence-of-defines)))
      (cond ((and (pair? last-form)
		  (eq? (car last-form) 'define-module))
	     (cadr last-form))
	    (else '(guile-user))))))

(define-public (ilisp-in-package package)
  (set-current-module (string->module package)))

(define-public (ilisp-eval form package filename line)
  "Evaluate FORM in PACKAGE recording FILENAME as the source file
and LINE as the source code line there."
  (eval-in-package
   (read-from-string/source form filename line 1)
   (string->module package)))

(define-public (ilisp-trace symbol package breakp)
  (trace (eval-in-package symbol (string->module package)))
  *unspecified*)

(define-public (ilisp-untrace symbol package)
  (untrace (eval-in-package symbol (string->module package)))
  *unspecified*)

(define (or-map* f list)
  "Apply f to successive elements of l until exhaustion or improper end
or while f returns #f. If returning early, return the return value of f."
  (let loop ((result #f)
	     (l list))
    (or result
	(and (pair? l)
	     (loop (f (car l)) (cdr l))))))

(define-public (ilisp-source-file symbol package)
  "Find the source file of SYMBOL's definition in PACKAGE."
  (catch #t
	 (lambda ()
	   (let ((value (eval-in-package (read-from-string symbol)
					 (string->module package))))
	     (cond
	      ((and (procedure? value)
		    (procedure-source value))
	       => (lambda (source)
		    (and=>
		     (or-map* (lambda (s)
				(false-if-exception
				 (source-property s 'filename)))
			      source)
		     (lambda (filename) (throw 'result filename))))))
	     'nil))
	 (lambda (key . args)
	   (if (eq? key 'result)
	       (begin (write (car args)) (newline) #t)
	       'nil))))

(define-public (ilisp-macroexpand-1 expression package)
  (save-module-excursion
   (lambda ()
     (set-current-module (string->module package))
     (macroexpand-1 (read-from-string expression)))))

(define-public (ilisp-macroexpand expression package)
  (save-module-excursion
   (lambda ()
     (set-current-module (string->module package))
     (macroexpand (read-from-string expression)))))
