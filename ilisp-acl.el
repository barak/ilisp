;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-acl.el --

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
;;; ILISP Allegro Common Lisp dialect definition
;;;

;;; Various patches provided by Kimball Collins
;;; <kpc@ptolemy-ethernet.arc.nasa.gov>


;;;%%%Allegro
(defvar ilisp-allegro-init-file "allegro.lisp")

(defun allegro-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((old-level (if (and old (eq 1 (string-match "[0-9]+" old)))
 			(string-to-int (substring old 1))
 			0))
 	 (new-level (if (eq 1 (string-match "[0-9]+" new))
 			(string-to-int (substring new 1))
 			0)))
    (<= new-level old-level)))
 
;;;
(defdialect allegro "Allegro Common LISP"
  common-lisp
  (ilisp-load-init 'allegro ilisp-allegro-init-file)
  (setq comint-fix-error ":pop"
	ilisp-reset ":reset"
	comint-continue ":cont"
	comint-interrupt-regexp  "Error: [^\n]* interrupt\)")
  (setq comint-prompt-status 
	(function (lambda (old line)
		    (comint-prompt-status old line 'allegro-check-prompt))))
  ;; <cl> or package> at top-level
  ;; [0-9c] <cl> or package> in error
  ;; (setq comint-prompt-regexp "^\\(\\[[0-9]*c*\\] \\|\\)\\(<\\|\\)[^>]*> ")
  ;; (setq comint-prompt-regexp "^\\(\\[[0-9]+i?c?\\] \\|\\[step\\]\\)?\\(<?[-A-Za-z]* ?[0-9]*?>\\|[-A-Za-z0-9]+([0-9]+):\\) ")

  ;; Patch by kpc 94/8/30: allow prompts that look like this:
  ;; USER(23): USER(23):
  (setq comint-prompt-regexp "^\\(\\(\\[[0-9]+i?c?\\] \\|\\[step\\] \\)?\\(<?[-A-Za-z]* ?[0-9]*?>\\|[-A-Za-z0-9]+([0-9]+):\\) \\)+")
   
  (setq ilisp-error-regexp
	"\\(ILISP:[^\"]*\\)\\|\\(Error:[^\n]*\\)\\|\\(Break:[^\n]*\\)")

  (setq ilisp-source-types (append ilisp-source-types '(("any"))))

  (setq ilisp-find-source-command 
	"(ILISP:ilisp-source-files \"%s\" \"%s\" \"%s\")")

  ;; Note:
  ;; 19990920
  ;; The global definition should now take care to find out the
  ;; proper extension.  See file 'ilisp-cl.el'.
  ;; (setq ilisp-binary-command "excl:*fasl-default-type*")

  ;;(setq ilisp-init-binary-command
  ;;	;; Patch provided by kpc 94/8/30: distinguish among
  ;;	;; fasl-incompatible allegro versions
  ;;	"(let ((ext (or #+m68k \"68fasl\"
  ;;		        #+sparc \"sfasl\"
  ;;		        #+iris4d \"ifasl\"
  ;;                    #+dec3100 \"pfasl\"
  ;;                    excl:*fasl-default-type*)))
  ;;           #+allegro-v4.0 (setq ext (concatenate 'string ext \"40\"))
  ;;           #+allegro-v4.1 (setq ext (concatenate 'string ext \"41\"))
  ;;           #+allegro-v4.2 (setq ext (concatenate 'string ext \"42\"))
  ;;           ext)")

  ;; FI:CLMAN support

  (setf ilisp-*use-fi-clman-interface-p* t)

  ;; ILD Support

  (setq ild-abort-string ":pop"
	ild-continue-string ":cont"
	ild-next-string ":dn"
	ild-next-string-arg ":dn %s"
	ild-previous-string ":up"
	ild-previous-string-arg ":up %s"
	ild-top-string ":to"
	ild-bottom-string ":bo"
	ild-backtrace-string ":bt"
	ild-locals-string ":local"
	ild-local-string-arg ":local %s"
	ild-return-string nil		;needs work
	ild-retry-string ":rest"
	ild-trap-on-exit-string ":boe")
  )

(unless allegro-program (setq allegro-program "cl"))

;;; end of file -- ilisp-acl.el --
