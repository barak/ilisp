;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-cl.el --

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
;;; ILISP Common Lisp dialect definition
;;;


;;;%%Common LISP

(defvar ilisp-cl-ilisp-package-file "ilisp-pkg.lisp")

(defvar ilisp-cl-ilisp-init-file "cl-ilisp.lisp")


;;; common-lisp --
;;;
;;; Notes:
;;; 19990806 Martin Atzmueller
;;; Added several package related entries.
;;;
;;; 19990806 Marco Antoniotti
;;; MAJOR CHANGE. Changed the name of the dialect to COMMON-LISP

(defdialect common-lisp "Common LISP"
  ilisp
  (setq ilisp-load-or-send-command 
	"(or (and (load \"%s\" :if-does-not-exist nil) t)
             (and (load \"%s\" :if-does-not-exist nil) t))")

  ;; The following line is an incredible kludge to bypass the behavior
  ;; of ilisp-load-init and to stick the package file in front of
  ;; everything.
  ;; Check what ilisp-load-init does to understand why I am forced to
  ;; do this.
  ;; Marco Antoniotti 19941122
  (ilisp-load-init 'ilisp-package-kludge ilisp-cl-ilisp-package-file)

  ;; 19990912 Marco Antoniotti
  ;; Changed the argument below from 'clisp to 'common-lisp.
  (ilisp-load-init 'common-lisp ilisp-cl-ilisp-init-file)
  (setq ilisp-package-regexp
	"^[ \t]*(in-package[ \t\n]*"

	ilisp-package-command
	"(let ((*package* *package*)) %s (package-name *package*))"

	ilisp-in-package-command-string
	"in-package"
         
	ilisp-defpackage-command-string
	"defpackage"

	ilisp-package-name-command
	"(package-name *package*)"

	ilisp-in-package-command
	"(in-package \"%s\")"

         ilisp-defpackage-regexp
         "^[ \t]*(defpackage[ \t\n]*"
 
         ilisp-hash-form-regexp
         "\\(^[ \t]*#[+-].\\)\\|\\(^[ \t]*(defpackage[ \t\n]\\)\\|\\(^[ \t]*(in-package[ \t\n]*\\)"

	ilisp-last-command
	"*"

	ilisp-save-command
	"(progn (ILISP:ilisp-save) %s\n)"

	ilisp-restore-command
	"(ILISP:ilisp-restore)"

	ilisp-block-command
	"(progn %s\n)"

	ilisp-eval-command
	"(ILISP:ilisp-eval \"%s\" \"%s\" \"%s\")"

	ilisp-defvar-regexp
	"(defvar[ \t\n]")

  (setq ilisp-defvar-command 
	"(ILISP:ilisp-eval \"(let ((form '%s)) (progn (makunbound (second form)) (eval form)))\" \"%s\" \"%s\")")

  (setq ilisp-compile-command
	"(ILISP:ilisp-compile \"%s\" \"%s\" \"%s\")"

	ilisp-describe-command
	"(ILISP:ilisp-describe \"%s\" \"%s\")"

	ilisp-inspect-command
	"(ILISP:ilisp-inspect \"%s\" \"%s\")"

	ilisp-arglist-command
	"(ILISP:ilisp-arglist \"%s\" \"%s\")")

  (setq ilisp-documentation-types
	'(("function") ("variable")
	  ("structure") ("type")
	  ("setf") ("class")
	  ("(qualifiers* (class ...))")))

  (setq ilisp-documentation-command
	"(ILISP:ilisp-documentation \"%s\" \"%s\" \"%s\")")

  (setq ilisp-macroexpand-1-command 
	"(ILISP:ilisp-macroexpand-1 \"%s\" \"%s\")")

  (setq ilisp-macroexpand-command
	"(ILISP:ilisp-macroexpand \"%s\" \"%s\")")

  (setq ilisp-complete-command 
	"(ILISP:ilisp-matching-symbols \"%s\" \"%s\" %s %s %s)")

  (setq ilisp-locator 'lisp-locate-clisp)

  (setq ilisp-source-types 
	'(("function") ("macro") ("variable")
	  ("structure") ("type")
	  ("setf") ("class")
	  ("(qualifiers* (class ...))")))

  (setq ilisp-callers-command
	"(ILISP:ilisp-callers \"%s\" \"%s\")"

	ilisp-trace-command
	"(ILISP:ilisp-trace \"%s\" \"%s\" \"%s\")"

	ilisp-untrace-command
	"(ILISP:ilisp-untrace \"%s\" \"%s\")")

  (setq ilisp-directory-command
	"(namestring *default-pathname-defaults*)"

	ilisp-set-directory-command
	"(setq *default-pathname-defaults* (parse-namestring \"%s\"))")

  ;; Note:
  ;; 19990912 Marco Antoniotti
  ;; This is probably the best and simplest way to fix things.
  ;; cfr. Hannu Koivisto's posting on 'ilisp@cons.org'.

  (setf ilisp-binary-command
	"(pathname-type (compile-file-pathname \"ILISP-DUMMY-STRING\"))"

	ilisp-init-binary-command ilisp-binary-command)

  (setq ilisp-load-command
	"(load \"%s\")")

  (setq ilisp-compile-file-command 
	"(ILISP:ilisp-compile-file \"%s\" \"%s\")")

  (setq ilisp-print-info-message-command
	"(ILISP:ilisp-print-info-message \"%s\" \"%s\")" ))

;;; end of file -- ilisp-cl.el --
