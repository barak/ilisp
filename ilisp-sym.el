;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-sym.el --

;;; This file is part of ILISP.
;;; Version: 5.10.1
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996-2000 Marco Antoniotti and Rick Campbell
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



;;;
;;; ILISP Lisp symbol utils.
;;;

;;; Notes:
;;;
;;; 19990804 Marco Antoniotti
;;; This should become a DEFSTRUCT
;;;
;;; (defstruct (lisp-symbol (:constructor lisp-symbol))
;;;   package
;;;   (delimiter "::")
;;;   name)

;;;%%Symbol
(defun lisp-symbol (package delimiter name)
  "Create a LISP symbol."
  (list package (when package (or delimiter "::")) name))

(defun lisp-symbol-name (symbol)
  "Return the name of SYMBOL."
  (car (cdr (cdr symbol))))

(defun lisp-symbol-package (symbol)
  "Return the package of SYMBOL."
  (car symbol))

(defun lisp-symbol-delimiter (symbol)
  "Return the qualifier of SYMBOL."
  (car (cdr symbol)))

;;;
(defun lisp-symbol= (symbol1 symbol2)
  "Return T is SYMBOL1 is equal to SYMBOL2."
  (and (string= (lisp-symbol-name symbol1) (lisp-symbol-name symbol2))
       (string= (lisp-symbol-package symbol1) (lisp-symbol-package symbol2))
       (string= (lisp-symbol-delimiter symbol1)
		(lisp-symbol-delimiter symbol2))))

;;; end of file -- ilisp-sym.el --
