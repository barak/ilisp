;;; -*- Mode: Emacs-Lisp -*-

;;; ilcompat.el --

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


;;;============================================================================
;;; Global definitions/declarations

(defconst +ilisp-emacs-version-id+
    (cond ((string-match "XEmacs" emacs-version)
	   'xemacs)
	  ((string-match "Lucid" emacs-version)
	   (if (string-match "^19.[0-7][^0-9]" emacs-version)
	       'lucid-19
	       'lucid-19-new))
	  ((string-match "^19" emacs-version)
	   'fsf-19)
	  ((string-match "^2[01]" emacs-version)
	   'fsf-20)
	  (t 'fsf-18))
  "The version of Emacs ILISP is running in.
Declared as '(member fsf-19 fsf-19 fsf-20 lucid-19 lucid-19-new xemacs).
Set in ilcompat.el.")

(defconst +ilisp-emacs-minor-version-number+
    (cond ((eq +ilisp-emacs-version-id+ 'fsf-18) 59)
	  ((or  (eq +ilisp-emacs-version-id+ 'lucid-19)
		(eq +ilisp-emacs-version-id+ 'lucid-19-new)
		)
	   12)				; Does emacs-minor-version work?
	  ((eq +ilisp-emacs-version-id+ 'xemacs) 14)
	  (t emacs-minor-version)))


;;;============================================================================
;;; Code

(cond ((or (eq +ilisp-emacs-version-id+ 'lucid-19)
	   (eq +ilisp-emacs-version-id+ 'lucid-19-new))
       (load "illuc19"))
      ((eq +ilisp-emacs-version-id+ 'xemacs) (load "ilxemacs"))
      ((eq +ilisp-emacs-version-id+ 'fsf-18) (load "ilfsf18"))
      ((eq +ilisp-emacs-version-id+ 'fsf-19) (load "ilfsf19"))
      ((eq +ilisp-emacs-version-id+ 'fsf-20) (load "ilfsf20"))
      )

;;;============================================================================
;;; Epilogue

(provide 'compat)

;;; end of file -- compat.el --
