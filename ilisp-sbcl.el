;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-sbcl.el --
;;; ILISP SB Common Lisp dialect definition
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-sbcl.el,v 1.9 2001/05/12 22:10:53 marcoxa Exp $

;;;%%%SB Common Lisp

(defvar sblisp-source-directory-regexp 
  "\\/afs\\/cs\\.cmu\\.edu\\/usr\\/stuff\\/sbcl\\/src\\/[0-9]*\\/"
  "*Regexp to match sblisp source code directory.")

(defvar sblisp-local-source-directory
  nil
  "*Where the sblisp sources really are.")

(defvar ilisp-sblisp-init-file
  ;; Note: The init file source extension (".lisp") needs to be
  ;; present in the filename, otherwise ILISP-LOAD-OR-SEND gets
  ;; confused trying to add compiled-file extensions (e.g. ".x86f"),
  ;; because it's hard-wired to look for a period (".") in order to
  ;; decide where to append the compiled-file extension.
  "sblisp")

(defun sblisp-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (string-match "]+" old)))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (string-match "]+" new))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect sblisp "SB Common LISP"
  common-lisp
  (ilisp-load-init 'sb ilisp-sblisp-init-file)

  (setq comint-prompt-regexp "^\\([0-9]+\\]+\\|\\*\\) "
	ilisp-trace-command "(ILISP:sblisp-trace \"%s\" \"%s\" \"%s\")"
	comint-prompt-status 
	(function (lambda (old line)
		    (comint-prompt-status old line 'sblisp-check-prompt)))

	ilisp-error-regexp "\\(ILISP:[^\"]*\\)\\|\\(error [^\n]*\n\n\\)\\|\\(debugger invoked on [^:]*:\\)"
	;; The above regexp has been suggested by
	;; hunter@work.nlm.nih.gov (Larry Hunter)
        
	ilisp-arglist-command "(ILISP:ilisp-arglist \"%s\" \"%s\")"

        ilisp-directory-command "(sb-unix:unix-current-directory)"
        ilisp-set-directory-command "(sb-unix:unix-chdir \"%s\")"

	ilisp-find-source-command "(ILISP:source-file \"%s\" \"%s\" \"%s\")"

        comint-fix-error ":abort"

	comint-continue ":go"

	ilisp-reset ":q"

	comint-interrupt-regexp "Interrupted at"

	;; Note:
	;; 19990806 Marco Antoniotti
	;; As Martin Atzmueller has pointed out, these hardcoded
	;; constraints are very nasty.
	;; However, before hacking the code right here, I'd like to
	;; see an all-out solution to the binary file extension problem.

	ilisp-binary-extension "x86f"
	ilisp-init-binary-extension "x86f"
	ilisp-binary-command "\"x86f\""
	)

  ;; ILD Support

  (setq ild-abort-string ":abort"
	ild-continue-string ":go"
	ild-next-string ":down"
	ild-next-string-arg nil		;needs work
	ild-previous-string ":up"
	ild-previous-string-arg nil	;needs work
	ild-top-string ":bottom"
	ild-bottom-string ":top"
	ild-backtrace-string ":backtrace"
	ild-locals-string ":l"
	ild-local-string-arg "(debug:arg %s)"
	ild-return-string nil		; needs work (debug:debug-return x)
	ild-retry-string nil		; needs work
	ild-trap-on-exit-string nil	; needs work
	)
  )

(unless sblisp-program (setq sblisp-program "sbcl --noinform"))

;;; end of file -- ilisp-sbcl.el --

