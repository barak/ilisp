;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-luc.el --

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
;;; ILISP Lucid Common Lisp dialect definition
;;;



;;;%%%Lucid
(defvar ilisp-lucid-init-file "lucid.lisp")


(defun lucid-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((old-level (if (and old (eq 0 (string-match "\\(->\\)+" old)))
 			(- (match-end 0) (match-beginning 0))
 			0))
	 (new-level (if (eq 0 (string-match "\\(->\\)+" new))
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect lucid "Lucid Common LISP"
  common-lisp
  (ilisp-load-init 'lucid ilisp-lucid-init-file)
  (setq comint-prompt-regexp "^\\(->\\)+ \\|^[^> ]*> "
	comint-fix-error ":a"
	ilisp-reset ":a :t"
	comint-continue ":c"
	comint-interrupt-regexp ">>Break: Keyboard interrupt"
	comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'lucid-check-prompt))))
  (setq ilisp-error-regexp "ILISP:[^\"]*\\|>>[^\n]*")
  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-find-source-command 
	"(ILISP:ilisp-source-files \"%s\" \"%s\" \"%s\")")

  ;; Note:
  ;; 19990920
  ;; The global definition should now take care to find out the
  ;; proper extension.  See file 'ilisp-cl.el'.
  
  ;;(setq ilisp-binary-command 
  ;;	"(first (last lucid::*load-binary-pathname-types*))")

  ;; ILD Support
  (setq ild-abort-string ":A"
       ild-continue-string ":C"
       ild-next-string ":N"
       ild-next-string-arg ":N %s"
       ild-previous-string ":P"
       ild-previous-string-arg ":P %s"
       ild-top-string ":<"
       ild-bottom-string ":>"
       ild-backtrace-string ":B"
       ild-locals-string ":V"
       ild-local-string-arg ":L %s"
       ild-return-string ":R"
       ild-retry-string ":F"
       ild-trap-on-exit-string ":X T")
  )

(unless lucid-program (setq lucid-program "lisp"))

(defdialect liquid "Liquid Common Lisp" lucid)

(unless liquid-program (setq liquid-program "lisp"))

;;; end of file -- ilisp-luc.el --
