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

(define (proc-doc-first-line obj)
  (let ((f-l (first-line (proc-doc obj))))
    (if (equal? (substring f-l 0 6) "No doc")
	#f
	f-l)))

(define-public (ilisp-print-info-message sym package)
  ;; taken from session.scm (help)
  (let ((obj (catch #t
		    (lambda ()
		      (save-module-excursion
		       (lambda ()
			 (set-current-module (string->module package))
			 (eval sym))))
		    (lambda args
		      #f))))
    (cond
     ((closure? obj)
      (let ((formals (cadr (procedure-source obj))))
	(display (cons sym formals))))
;;; too expensive...
;;     ((and (procedure? obj)
;;           (let ((d (proc-doc-first-line obj)))
;;            (if d (display d))
;;            d)))
;;     ((and (macro? obj) (macro-transformer obj)
;;          (let ((d (proc-doc-first-line (macro-transformer obj))))
;;            (if d (display d))
;;            d)))
     ((procedure? obj)
      (arity obj))))
  (newline))

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

(define-public (ilisp-get-package sequence-of-defines)
  "Get the last module name defined in the sequence of define-module forms."
  (let ((last-form (last sequence-of-defines)))
    (cond ((and (pair? last-form)
		(eq? (car last-form) 'define-module))
	   (cadr last-form))
	  (else '(guile-user)))))

(define-public (ilisp-in-package package)
  (set-current-module (string->module package)))

(define-public (ilisp-eval form package filename)
  "Evaluate FORM in PACKAGE recording FILENAME as the source file."
  (save-module-excursion
   (lambda ()
     (set-current-module (string->module package))
     (eval-string form))))

(define (eval-in-package expression package)
  (save-module-excursion
   (lambda ()
     (set-current-module (string->module package))
     (eval expression))))

(define-public (ilisp-trace symbol package breakp)
  (trace (eval-in-package symbol package))
  *unspecified*)

(define-public (ilisp-untrace symbol package)
  (untrace (eval-in-package symbol package))
  *unspecified*)

(define-public (ilisp-arglist symbol package)
  ;; This is a hack.
  (let* ((s (with-output-to-string
	     (lambda () (ilisp-print-info-message symbol package))))
	 (l (with-input-from-string s read)))
    (cond ((pair? l) (cdr l))
	  (else *unspecified*))))
      
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
