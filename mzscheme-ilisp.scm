;;;; mzscheme-ilisp.scm --- ILISP support functions for MzScheme
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

(define (ilisp-matching-symbols string package function? external? prefix?)
  (let loop ((syms (make-global-value-list))
	     (result '()))
    (if (null? syms)
	result
	(let ((sym-str (symbol->string (caar syms))))
	  (if (and (>= (string-length sym-str) (string-length string))
		   (string=? (substring sym-str 0 (string-length string))
			     string))
	      (loop (cdr syms) (cons (list (symbol->string (caar syms))) result))
	      (loop (cdr syms) result))))))
