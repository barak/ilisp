;;;  -*- Mode: Emacs-Lisp -*-

;;; .ilisp --

(if (file-executable-p "/usr/sbin/register-common-lisp-source")
    (defvar ilisp-*directory* "/usr/share/common-lisp/source/ilisp/")
    (defvar ilisp-*directory* "/usr/share/ilisp/"))


(autoload 'run-ilisp "ilisp" "Select a new inferior LISP." t)

(autoload 'clisp-hs  "ilisp" "Inferior CLISP Common LISP." t)

;;; CMULISP
(autoload 'cmulisp  "ilisp" "Inferior CMU Common LISP." t)

(autoload 'cmulisp-small  "ilisp" "Inferior CMU Common LISP -- small core." t)

(autoload 'cmulisp-normal  "ilisp" "Inferior CMU Common LISP -- normal core." t)

(autoload 'cmulisp-safe  "ilisp" "Inferior CMU Common LISP -- safe core" t)

(autoload 'sbcl  "ilisp" "Inferior Steel Bank Common LISP." t)

(autoload 'scheme    "ilisp" "Inferior generic Scheme." t)

(autoload 'guile    "ilisp" "Inferior Guile Scheme." t)

(setq scheme-program "/usr/bin/guile")

(setq ilisp-*use-frame-for-output* nil)
(setq *ilisp-use-frame-for-output* nil)

;;; If you run cmu-cl then set this to where your source files are.
(setq cmulisp-local-source-directory "/usr/src/cmucl/")

(setq common-lisp-hyperspec-root
      "http://localhost/doc/hyperspec/")

;;; This makes reading a lisp file load in ilisp.

(set-default 'auto-mode-alist
	     (append '(("\\.lisp$" . lisp-mode)
		       ("\\.lsp$" . lisp-mode)
		       ("\\.cl$" . lisp-mode)) auto-mode-alist))

(add-hook
 'scheme-mode-hook (function 
		    (lambda ()
		      (require 'ilisp))))
