;;;  -*- Mode: Emacs-Lisp -*-
;;;
;;; Copyright (c) 2002 Kevin Rosenberg GNU License

(defvar ilisp-*directory* "/usr/lib/ilisp/")

(autoload 'run-ilisp "ilisp" "Select a new inferior LISP." t)

;; CLisp
(autoload 'clisp-hs  "ilisp" "Inferior CLISP Common LISP." t)
(setq clisp-hs-progam "clisp -a -I")

;; AllegroCL
(autoload 'allegro "ilisp" "Inferior Allegro Common Lisp." t)
(setq allegro-program "/usr/bin/acl")
(defun alisp ()
  (interactive)
  (setq allegro-program "/usr/bin/alisp")
  (allegro))
(defun alisp8 ()
  (interactive)
  (setq allegro-program "/usr/bin/alisp8")
  (allegro))
(defun mlisp ()
  (interactive)
  (setq allegro-program "/usr/bin/mlisp")
  (allegro))
(defun mlisp8 ()
  (interactive)
  (setq allegro-program "/usr/bin/mlisp8")
  (allegro))

;; Lispworks
(autoload 'lispworks "ilisp" "Inferior Lispworks Common Lisp." t)
(setq lispworks-program "/usr/bin/lw-console -multiprocessing")

;;; CMULISP
(autoload 'cmulisp  "ilisp" "Inferior CMU Common LISP." t)
(setq cmulisp-program "/usr/bin/lisp")
(defun cmucl-normal ()
  "Inferior CMU Common LISP -- normal core."
  (interactive)
  (setq cmulisp-program "/usr/bin/lisp -core /usr/lib/cmucl/lisp-normal.core")
  (cmulisp))
(defun cmucl-small () 
  "Inferior CMU Common LISP -- small core."
  (interactive)
  (setq cmulisp-program "/usr/bin/lisp -core /usr/lib/cmucl/lisp-small.core")
  (cmulisp))
(defun cmucl-safe ()
 "Inferior CMU Common LISP -- safe core."
  (interactive)
  (setq cmulisp-program "/usr/bin/lisp -core /usr/lib/cmucl/lisp-safe.core")
  (cmulisp))

;; SBCL
(autoload 'sbcl  "ilisp" "Inferior Steel Bank Common LISP." t)
(setq sbcl-program "/usr/bin/sbcl")

;; Guile
(autoload 'scheme  "ilisp" "Inferior generic Scheme." t)
(autoload 'guile "ilisp" "Inferior Guile Scheme." t)
(setq scheme-program "/usr/bin/guile")


;;; Default paths
(setq cmulisp-local-source-directory "/usr/src/cmucl/")
(setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
(setq cltl2-root-url "file:///usr/share/doc/cltl/")

(setq ilisp-*use-frame-for-output* nil)
(setq ilisp-*use-fsf-compliant-keybindings* t)


;;; Loading lisp files starts ilisp
(set-default 'auto-mode-alist
	     (append '(("\\.lisp$" . lisp-mode)
		       ("\\.lsp$" . lisp-mode)
		       ("\\.cl$" . lisp-mode)) auto-mode-alist))

(add-hook 'lisp-mode-hook '(lambda () (require 'ilisp)))

;;; Load hooks
(add-hook
 'scheme-mode-hook (function 
		    (lambda ()
		      (require 'ilisp))))

(add-hook 'ilisp-load-hook
	  '(lambda ()
             ;; Set a keybinding for the COMMON-LISP-HYPERSPEC command
             ;; (defkey-ilisp "" 'common-lisp-hyperspec)

	     (setq ilisp-*use-frame-for-output* nil)
	     (setq ilisp-*use-fsf-compliant-keybindings* t)
	     (setq ilisp-*use-fsf-compliant-keybindings* t)

	     ;; Make sure that you don't keep popping up the 'inferior
             ;; Lisp' buffer window when this is already visible in
             ;; another frame. Actually this variable has more impact
             ;; than that. Watch out.
             ; (setq pop-up-frames t)

             (message "Running ilisp-load-hook")
             ;; Define LispMachine-like key bindings, too.
             ; (ilisp-lispm-bindings) Sample initialization hook.

             ;; Set the inferior Lisp directory to the directory of
             ;; the buffer that spawned it on the first prompt.
             (add-hook 'ilisp-init-hook
                       '(lambda ()
                          (default-directory-lisp ilisp-last-buffer)))

             ))

