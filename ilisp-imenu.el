;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-out.el --

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

;;; 2000-03-03 Martin Atzmueller
;;; use imenu for displaying a fancy function/definition list.


;;; modified for a better display of function+arglist!
;;; let tokens contain spaces and test with string-equal.

(defun imenu--completion-buffer (index-alist &optional prompt)
  "Let the user select from INDEX-ALIST in a completion buffer with PROMPT.

Returns t for rescan and otherwise a position number."
  ;; Create a list for this buffer only when needed.
  (let ((name (thing-at-point 'symbol))
	choice)
    (cond (prompt)
	  ((and name (imenu--in-alist name index-alist))
	   (setq prompt (format "Index item (default %s): " name)))
	  (t (setq prompt "Index item: ")))
    (if (eq imenu-always-use-completion-buffer-p 'never)
  	(setq name (completing-read prompt
  				    index-alist
 				    nil t nil 'imenu--history-list name))
      (save-window-excursion
	;; Display the completion buffer
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list
	   (all-completions "" index-alist )))
	(let ((minibuffer-setup-hook
	       (function
		(lambda ()
		  (let ((buffer (current-buffer)))
		    (save-excursion
		      (set-buffer "*Completions*")
		      (setq completion-reference-buffer buffer)))))))
	  ;; Make a completion question
	  (setq name (completing-read prompt
				      index-alist
				      #'string-equal
                                      t nil 'imenu--history-list name)))))
    (cond ((not (stringp name))
	   nil)
	  ((string= name (car imenu--rescan-item))
	   t)
	  (t
	   (setq choice (assoc name index-alist))
	   (if (imenu--subalist-p choice)
	       (imenu--completion-buffer (cdr choice) prompt)
	     choice)))))
;;;---

;; Return the previous+current sexp and the location of the sexp (its
;; beginning) without moving the point.
(defun ilisp-imenu-function--name-and-position ()
  (save-excursion
    (forward-sexp -1)
    ;; [ydi] modified for imenu-use-markers
    (let* ((beg (if imenu-use-markers (point-marker) (point)))
           (end (progn (forward-sexp) (point)))
           (name (buffer-substring beg end) )
           (beg2 (progn (forward-sexp) (forward-sexp -1) (point) ) )
           (end2 (progn (forward-sexp) (point) ) )
           (args (buffer-substring beg2 end2) ) )
      (cons (concat name args) 
	    beg))))

(defun ilisp-imenu-general--name-and-position ()
  (save-excursion
    (forward-sexp -1)
    ;; [ydi] modified for imenu-use-markers
    (let ((beg (if imenu-use-markers (point-marker) (point)))
	  (end (progn (forward-sexp) (point))))
      (cons (buffer-substring beg end)
	    beg))))


(defun ilisp-imenu-extract-index-name ()
  ;; `imenu-extract-index-name-function' is set to this.
  ;; generates a flat index of definitions in a lisp file.
  (save-match-data
    (and (looking-at "(def")
	 (condition-case nil
	     (progn
	       (down-list 1)
	       (forward-sexp 2)
	       (let ((beg (point))
		     (end (progn (forward-sexp -1) (point))))
		 (buffer-substring beg end)))
	   (error nil)))))

(defun ilisp-imenu-create-lisp-index ()
  ;; `imenu-create-index-function' is set to this.
  ;; generates a nested index of definitions.
  (let ((index-fun-alist '())
	(index-var-alist '())
        (index-const-alist '())
	(index-type-alist '())
	(index-unknown-alist '())
	prev-pos)
    (goto-char (point-max))
    (imenu-progress-message prev-pos 0)
    ;; Search for the function
    (while (beginning-of-defun)
      (imenu-progress-message prev-pos nil t)
	  (save-match-data
	    (and (looking-at "(def")
		 (save-excursion
	       (down-list 1)
		   (cond
		((looking-at "def\\(var\\|constant\\|parameter\\)")
		     (forward-sexp 2)
		     (push (ilisp-imenu-general--name-and-position)
			   index-var-alist))
		((looking-at "def\\(un\\|macro\\|method\\|generic\\)")
		     (forward-sexp 2)
		     (push (ilisp-imenu-function--name-and-position)
			   index-fun-alist))
		((looking-at "def\\(type\\|struct\\|class\\|ine-condition\\)")
		     (forward-sexp 2)
 		 (if (= (char-after (1- (point))) ?\))
			 (progn
 		       (forward-sexp -1)
			   (down-list 1)
 		       (forward-sexp 1)))
		     (push (ilisp-imenu-general--name-and-position)
			   index-type-alist))
                ((looking-at "def")
                 (forward-sexp 2)
		     (push (ilisp-imenu-function--name-and-position)
			   index-fun-alist))
		    (t
		     (forward-sexp 2)
		     (push (ilisp-imenu-general--name-and-position)
		       index-unknown-alist)))))))
    (imenu-progress-message prev-pos 100)
    (and index-var-alist
	 (push (cons "Variables" index-var-alist)
	       index-fun-alist))
    (and index-type-alist
 	 (push (cons "Types" index-type-alist)
  	       index-fun-alist))
    (and index-unknown-alist
	 (push (cons "Syntax-unknown" index-unknown-alist)
	       index-fun-alist))
    index-fun-alist))



;;;---

;;;###autoload
(defun ilisp-imenu-add-menubar-index ()
  "Add an Imenu \"Index\" entry on the menu bar for the current buffer.

A trivial interface to `imenu-add-to-menubar' suitable for use in a hook."
  (interactive)
  (imenu-add-to-menubar "Index"))


(add-hook 'lisp-mode-hook
          	  (lambda () 
                    (when (featurep 'imenu)
                      (setq imenu-extract-index-name-function
                            'ilisp-imenu-extract-index-name)
                      (setq imenu-create-index-function
                            'ilisp-imenu-create-lisp-index)
                      (ilisp-imenu-add-menubar-index) ) ) )
