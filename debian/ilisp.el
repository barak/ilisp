;;;  -*- Mode: Emacs-Lisp -*-

;;; .ilisp --

(autoload 'run-ilisp "ilisp" "Select a new inferior LISP." t)

(autoload 'clisp     "ilisp" "Inferior generic Common LISP." t)

;;; CMULISP
(autoload 'cmulisp  "ilisp" "Inferior CMU Common LISP." t)

(autoload 'sblisp  "ilisp" "Inferior CMU Common LISP." t)

(autoload 'scheme    "ilisp" "Inferior generic Scheme." t)

(autoload 'guile    "ilisp" "Inferior generic Scheme." t)

(setq clisp-program "clisp")

(setq cmulisp-program "/usr/bin/lisp")

(setq scheme-program "/usr/bin/guile")

(setq guile-program "/usr/bin/guile")

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

(add-hook 'ilisp-site-hook
	  (function
	   (lambda ()
	     (setq ilisp-init-binary-extension "x86f")
	     (setq ilisp-init-binary-command "(progn \"x86f\")")
	     (setq ilisp-binary-extension "x86f")
	     (setq ilisp-binary-command "\"x86f\"")
	     )))


