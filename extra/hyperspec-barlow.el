;;; hyperspec-barlow.el --
;;; 
;;; I'm sure that just about everybody who's a competent Lisp
;;; programmer has already written something like this for their own
;;; use. Can I find one _anywhere_ on the Web?  Nope.  Well, I'm not a
;;; competent Lisp programmer, but here's my take on it anyway.  It's
;;; public domain, do what you want with it, but I'd appreciate it if
;;; you dropped me mail if you found it useful.
;;;
;;; Sun Aug 30 23:43:59 1998 Daniel Barlow <dan@telent.net>
;;;
;;; Tested with FSF emacs 20.2, I have no idea about other versions.
;;; I map this to C-c D from my .emacs, as I don't have the Franz online
;;; manual.  Use a hook something like
;;;
;;;(add-hook 'lisp-mode-hook 
;;;          '(lambda () 
;;;             (require 'ilisp)
;;;             (setq indent-tabs-mode nil)
;;;             (require 'hyperspec)
;;;             (define-key lisp-mode-map "\C-cD" 'hyperspec-lookup-lisp)))
;;;
;;; Note that ilisp must be loaded before keys are set up, or it will
;;; trash conflicting definitions.
;;;
;;; Possible enhancements would include (a) some degree of error-checking,
;;; and (b) the ability to load the index from a url instead of only a
;;; local file.

(require 'browse-url)
(require 'thingatpt)

(defcustom hyperspec-root-url "file://localhost/usr/doc/HyperSpec/"
  "Root URL for the preferred HyperSpec copy.  
Appending \"FrontMatter/index.html\" should give the Welcome page"
  :group 'lisp)

(defcustom hyperspec-symbol-table "/usr/doc/HyperSpec/Data/Symbol-Table.text"
  "The HyperSpec symbol table file.  Must be a regular file."
  :group 'lisp)


(defun hyperspec-lookup-lisp (symbol)
  "Display the HyperSpec entry associated with SYMBOL in a web browser. 
The default for SYMBOL is the symbol at or near point.  Uses the BROWSE-URL 
function (q.v.) "
  (interactive (list (read-string "HyperSpec entry: " 
				  (thing-at-point 'symbol))))
  (let ((index-buffer (find-file-noselect hyperspec-symbol-table)))
    (save-excursion
      (set-buffer index-buffer)
      (goto-char (point-min))
      (search-forward (concat "\n" (upcase symbol) "\n"))
      (browse-url 
       (concat hyperspec-root-url "Data/" (thing-at-point 'line))))))


;;; Interface compatibility.

(defun hyperspec-lookup (symbol)
  (hyperspec-lookup-lisp symbol))


(provide 'hyperspec)

;;; end of file -- hyperspec-barlow.el --
