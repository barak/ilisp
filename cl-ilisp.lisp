;;; -*- Mode: Lisp -*-

;;; cl-ilisp.lisp --

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



;;; Common Lisp initializations
;;; Author: Chris McConnell, ccm@cs.cmu.edu

;;;
;;; ange-ftp hack added by ivan Wed Mar 10 12:30:15 1993
;;; ilisp-errors *gc-verbose* addition ivan Tue Mar 16 03:21:51 1993
;;;
;;; Rcs_Info: clisp.lisp,v 1.26 1993/09/03 02:05:07 ivan Rel $
;;;
;;; Revision 1.19  1993/08/24  22:01:52  ivan
;;; Use defpackage instead of just IN-PACKAGE.
;;; Renamed FUNCTION to FUN in ilisp-arglist to get around CMUCL 17b bug.
;;;
;;; Revision 1.16  1993/06/29  05:51:35  ivan
;;; Added Ed Gamble's #'readtable-case fix and Hans Chalupsky's
;;; allegro-4.1 addition.
;;;
;;; Revision 1.8  1993/06/28  00:57:42  ivan
;;; Stopped using 'COMPILED-FUNCTION-P for compiled check.
;;;
;;; Revision 1.3  1993/03/16  23:22:10  ivan
;;; Added breakp arg to ilisp-trace.
;;;
;;;


#+(or allegro-v4.0 allegro-v4.1)
(eval-when (compile load eval)
  (setq excl:*cltl1-in-package-compatibility-p* t))


(in-package "ILISP")

;;;
;;; GCL 2.2 and GCL 2.3 do not have defpackage (yet) so we need to put
;;; the export here. (toy@rtp.ericsson.se)
;;;
;;; Please note that while the comment and the fix posted by R. Toy
;;; are correct, they are deprecated by at least one of the ILISP
;;; maintainers. :) By removing the 'nil' in the following #+, you
;;; will fix the problem but will not do a good service to the CL
;;; community.  The right thing to do is to install DEFPACKAGE in your
;;; GCL and to write the GCL maintainers and to ask them to
;;; incorporate DEFPACKAGE in their standard builds.
;;; 19960715 Marco Antoniotti
;;; 20000404 Martin Atzmueller: same for ecl/ecls

#+(or (and nil gcl) (and nil ecl))
(export '(ilisp-errors
	  ilisp-save
	  ilisp-restore
	  ilisp-symbol-name
	  ilisp-find-symbol
	  ilisp-find-package
	  ilisp-eval
	  ilisp-compile
	  ilisp-describe
	  ilisp-inspect
	  ilisp-arglist
	  ilisp-documentation
	  ilisp-macroexpand
	  ilisp-macroexpand-1
	  ilisp-trace
	  ilisp-untrace
	  ilisp-compile-file
	  ilisp-casify
          ilisp-print-info-message
	  ilisp-matching-symbols))


;;;
(defvar *ilisp-old-result* nil "Used for save/restore of top level values.")

(defvar *ilisp-message-addon-string* "ILISP:")

;;; Martin Atzmueller 2000-01-15
;;; ilisp-message was mostly set up because Clisp expects an
;;; ~& or ~% before the message-string, otherwise it does not display anything!"

(defun ilisp-message (format-output-stream format-control-string &rest args)
  "ilisp-message provides an interface to create 'special' ILISP messages, i.e. \"ILISP: ... \" in an uniform way."
  (let* ((format-string (apply #'format nil " ~@?" format-control-string args))
         (concat-string (if (equal (char format-string 0) #\")
                            ""
                          (if format-output-stream
                              "\""
                            ""))))
    (format format-output-stream
            (concatenate 'string "~&" concat-string *ilisp-message-addon-string* format-string concat-string))))


#+:ANSI-CL
(defun special-form-p (symbol)
  "Backward compatibility for non ANSI CL's."
  (special-operator-p symbol))

#+(and :CLTL2 (not :ANSI-CL))
(defun special-form-p (symbol)
  "For CLTL2 Lisp just use the old one."
  (lisp:special-form-p symbol))
;;;
(defmacro ilisp-handler-case (expression &rest handlers)
  "Evaluate EXPRESSION using HANDLERS to handle errors."
  handlers
  (if (macro-function 'handler-case)
      `(handler-case ,expression ,@handlers)
      #+allegro `(excl::handler-case ,expression ,@handlers)
      #+lucid `(lucid::handler-case ,expression ,@handlers)
      #-(or allegro lucid) expression))


;;; ilisp-readtable-case --
;;;
;;; 19991218 Marco Antoniotti
;;; READTABLE-CASE is ANSI.  However, I feel magnanimous today, so I
;;; leave the check in to make it easier for non conforming
;;; implementations.

(defun ilisp-readtable-case (readtable)
  (if (fboundp 'readtable-case)
      (readtable-case readtable)
      #+allegro (case excl:*current-case-mode*
		  (:case-insensitive-upper :upcase)
		  (:case-insensitive-lower :downcase)
		  (otherwise :preserve))
      #-allegro :upcase))

;;;
(defmacro ilisp-errors (form)
  "Handle errors when evaluating FORM."
  `(let ((*standard-output* *terminal-io*)
	 (*error-output* *terminal-io*)
	 #+cmu
	 (ext:*gc-verbose* nil) ; cmulisp outputs "[GC ...]" which
				; doesn't read well...
	 #+ecl
	 (sys:*gc-verbose* nil) ; ecolisp also outputs "[GC ...]"
	 )
     (princ " ")			; Make sure we have output

     ;; 19990912 Martin Atzmuller
     ;; Gross CLisp HS hack so that the command-index stays the same
     ;; after an ILISP-command that has to use the inferior lisp
     ;;
     ;; 19990912 Marco Antoniotti
     ;; Put here since the change is very localized and not requiring
     ;; a separate init file.
     #+:clisp
     (setq system::*command-index* (max 0 (- system::*command-index* 2)))

     (ilisp-handler-case
      ,form	
      (error (error)
             (ilisp-message nil "~A" error)))))


;;;
(defun ilisp-save ()
  "Save the current state of the result history."
  (declare (special / // /// + ++ +++))
  (unless *ilisp-old-result*
    (setq *ilisp-old-result* (list /// // +++ ++ + /))))

;;;
(defun ilisp-restore ()
  "Restore the old result history."
  (declare (special / // + ++ * ** -))
    
  (setq // (pop *ilisp-old-result*)
	** (first //)
	/  (pop *ilisp-old-result*)
	*  (first /)
	++  (pop *ilisp-old-result*)
	+   (pop *ilisp-old-result*)
	-   (pop *ilisp-old-result*))

    ;; Martin Atzmueller 2000-01-26
    (let ((new/ (pop *ilisp-old-result*)))
      (if (some #'(lambda (new+)
		    (and (stringp new+)
			 (search *ilisp-message-addon-string* new+)))
		new/)
	  nil
	(values-list new/))))
  
;;; ilisp-symbol-name --
;;;
;;; ':capitalize' case added under suggestion by Rich Mallory.
(defun ilisp-symbol-name (symbol-name)
  "Return SYMBOL-NAME with the appropriate case as a symbol."
  (case (ilisp-readtable-case *readtable*)
    (:upcase (string-upcase symbol-name))
    (:downcase (string-downcase symbol-name))
    (:capitalize (string-capitalize symbol-name))
    (:preserve symbol-name)))


;;; ilisp-find-package --
;;;
;;; Notes:
;;; 19990806 Unknown Author (blame Marco Antoniotti for this)
;;; Added test for KEYWORD case.

(defun ilisp-find-package (package-name)
  "Return package PACKAGE-NAME or the current package."
  (cond ((string-equal package-name "nil") *package*)
	((string-equal package-name "") (find-package "KEYWORD"))
	(t (or (find-package (ilisp-symbol-name package-name))
	       (error "Package ~A not found" package-name)))))

;;;
(defun ilisp-find-symbol (symbol-name package-name)
  "Return the symbol associated with SYMBOL-NAME in PACKAGE-NAME.
The trick is to try to handle print case issues intelligently."
  (find-symbol (ilisp-symbol-name symbol-name)
	       (ilisp-find-package package-name)))


;;; The following two functions were in version 5.5.
;;; They disappeared in version 5.6. I am putting them back in the
;;; distribution in order to make use of them later if the need
;;; arises.
;;; Marco Antoniotti: Jan 2 1995
#|
(defun ilisp-filename-hack (filename)
  "Strip `/user@machine:' prefix from filename."
  ;; Ivan's hack for getting away with dumb /ivan@bu-conx:/foo/bar/baz
  ;; filenames...
  (let ((at-location (position #\@ filename))
	(colon-location (position #\: filename)))
    (if (and at-location colon-location)
	(subseq filename (1+ colon-location))
	filename)))


(defun ilisp-read-form (form package)
  "Read string FORM in PACKAGE and return the resulting form."
  (let ((*package* (ilisp-find-package package)))
    (read-from-string form)))
|#

;;;
(defun ilisp-eval (form package filename)
  "Evaluate FORM in PACKAGE recording FILENAME as the source file."
  (princ " ")
  ;; Ivan's hack for getting away with dumb /ivan@bu-conx:/foo/bar/baz
  ;; filenames...
  (let* ((at-location (position #\@ filename))
	 (colon-location (position #\: filename))
	 (filename
	  (if (and at-location colon-location)
	      (subseq filename (1+ colon-location))
	      filename))
	 (*package* (ilisp-find-package package))
	 #+allegro (excl::*source-pathname* filename)
	 #+allegro (excl::*redefinition-warnings* nil)
	 #+lucid (lucid::*source-pathname*
		  (if (probe-file filename)
		      (truename filename)
		      (merge-pathnames filename)))
	 #+lucid (lucid::*redefinition-action* nil)
	 #+lispworks (compiler::*input-pathname* (merge-pathnames filename))
	 #+lispworks (compiler::*warn-on-non-top-level-defun* nil)
	 ;; The LW entries are a mix of Rich Mallory and Jason
	 ;; Trenouth suggestions
	 ;; Marco Antoniotti: Jan 2 1995.
	 )
    filename
    (eval (read-from-string form))))

;;;
(defun ilisp-compile (form package filename)
  "Compile FORM in PACKAGE recording FILENAME as the source file."
  (princ " ")
  ;; This makes sure that function forms are compiled
  ;; NOTE: Rich Mallory proposed a variation of the next piece of
  ;; code. for the time being we stick to the following simpler code.
  ;; Marco Antoniotti: Jan 2 1995.
  #-lucid
  (ilisp-eval
   (format nil "(funcall (compile nil '(lisp:lambda () ~A)))"
	   form)
   package
   filename)
  
  ;; The following piece of conditional code is left in the
  ;; distribution just for historical purposes.
  ;; It will disappear in the next release.
  ;; Marco Antoniotti: Jan 2 1995.
  #+lucid-ilisp-5.6
  (labels ((compiler (form env)
		     (if (and (consp form)
			      (eq (first form) 'function)
			      (consp (second form)))
			 #-LCL3.0
		       (evalhook `(compile nil ,form) nil nil env)
		       #+LCL3.0
		       ;; If we have just compiled a named-lambda, and the
		       ;; name didn't make it in to the procedure object,
		       ;; then stuff the appropriate symbol in to the
		       ;; procedure object.
		       (let* ((proc (evalhook `(compile nil ,form)
					      nil nil env))
			      (old-name (and proc (sys:procedure-ref proc 1)))
			      (lambda (second form))
			      (name (and (eq (first lambda)
					     'lucid::named-lambda)
					 (second lambda))))
			 (when (or (null old-name)
				   (and (listp old-name)
					(eq :internal (car old-name))))
			       (setf (sys:procedure-ref proc 1) name))
			 proc)
		       (evalhook form #'compiler nil env))))
	  (let ((*evalhook* #'compiler))
	    (ilisp-eval form package filename)))
  #+lucid
  ;; Following form is a patch provided by Christopher Hoover
  ;; <ch@lks.csi.com>
  (let ((*package* (ilisp-find-package package))
 	(lcl:*source-pathname* (if (probe-file filename)
 				   (truename filename)
 				 (merge-pathnames filename)))
 	(lcl:*redefinition-action* nil))
    (with-input-from-string (s form)
			    (lucid::compile-in-core-from-stream s)
			    (values)))
  )

;;;
(defun ilisp-describe (sexp package)
  "Describe SEXP in PACKAGE."
  (ilisp-errors
   (let ((*package* (ilisp-find-package package)))
     (let ((item-to-be-described (read-from-string sexp)))
       (if (atom item-to-be-described)
	   (describe item-to-be-described)
	   (describe (eval item-to-be-described)))))))

;;;
(defun ilisp-inspect (sexp package)
  "Inspect SEXP in PACKAGE."
  (ilisp-errors
   (let ((*package* (ilisp-find-package package)))
    (let ((item-to-be-described (read-from-string sexp)))
       (if (atom item-to-be-described)
	   (inspect item-to-be-described)
	   (inspect (eval item-to-be-described)))))))

;;;
(defun ilisp-arglist (symbol package)
  "Returns the argument list of SYMBOL from PACKAGE."
  (ilisp-errors
    (let ((fn (ilisp-find-symbol symbol package))
	  (*print-length* nil)
	  (*print-pretty* t)
	  (*package* (ilisp-find-package package)))
      (cond ((null fn)
	     (format t "Symbol ~s not present in ~s." symbol package))
	    ((not (fboundp fn))
	     (format t "~s: undefined~%" fn))
	    (t
	     (print-function-arglist fn)))))
  (values))


;;; print-function-arglist --
;;; This function is really invoked only by the #\Space binding of
;;; ILISP-PRINT-INFO-MESSAGE.

;;; 19991218 Marco Antoniotti
;;; Unfortunately the function GET-FUNCTION-ARGLIST may default to
;;; DOCUMENTATION, which returns a string.  Hence the change.
;;;
;;; 19991218 Marco Antoniotti
;;; Using the arglist command bound to #\Space would probably be
;;; better.  Anyway...

(defun print-function-arglist (fn)
  "Pretty arglist printer"
  (let* ((arglist-doc (get-function-arglist fn))
	 (desc (ilisp-function-short-description fn)))
    (format t "~&~s~a" fn (or desc ""))
    (write-string ": ")
    (typecase arglist-doc
      (string (write-string arglist-doc))
      (list (let ((arglist (ldiff arglist-doc
				  (member '&aux arglist-doc))))
	      (if arglist
		  (write arglist :case :downcase :escape nil)
		  (write-string "()"))))
      (t (error (ilisp-message nil
			       "arglist doc very messed up [~S]."
			       arglist-doc))))
    (terpri)
    (values)))

#| Original version
(defun print-function-arglist (fn)
  "Pretty arglist printer"
  (let* ((a (get-function-arglist fn))
 	 (arglist (ldiff a (member '&aux a))))
	 (desc (ilisp-function-short-description fn)))
    (break "sun la")
    (format t "~&~s~a" fn (or desc ""))
    (write-string ": ")
    (if arglist
	(write arglist :case :downcase :escape nil)
      (write-string "()"))
    (terpri)
    (values)))
|#



(defun ilisp-generic-function-p (symbol)
  (let ((generic-p
	 (find-symbol "GENERIC-FUNCTION-P"
		      (or (find-package "PCL")
			  *package*))))
    (and generic-p
	 (fboundp generic-p)
	 (funcall generic-p symbol))))


  
(defun ilisp-function-short-description (symbol)
  (cond ((macro-function symbol)
	 " (Macro)")
	((special-form-p symbol)
	 " (Special Form)")
	((ilisp-generic-function-p symbol)
	 " (Generic)")))



(defun get-function-arglist (symbol)
  (let ((fun (symbol-function symbol)))
    (cond ((ilisp-generic-function-p symbol)
	   (funcall
	    (find-symbol "GENERIC-FUNCTION-PRETTY-ARGLIST"
			 (or (find-package "PCL") *package*))
	    fun))
	  (t
	   #+allegro
	   (excl::arglist symbol)

	   #+(or ibcl kcl ecl gcl)
	   (help symbol)

	   #+lucid
	   (lucid::arglist symbol)
	   
	   #+lispworks
	   (system::function-lambda-list symbol)

	   #+clisp
	   (arglist symbol)

	   #+cmu
	   (arglist symbol (symbol-package symbol))
	   
	   #+:sbcl
	   (arglist symbol (symbol-package symbol))
	   
	   #-(or allegro lucid kcl ibcl ecl clisp cmu :sbcl)
	   (documentation symbol 'function)))))


(defun ilisp-print-info-message (symbol package)
  "Returns the argument list or the value of SYMBOL from PACKAGE.
Error messages are generated appropriately."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package))
	 (*print-length* nil)
	 (*print-level* nil)
	 (*package* (ilisp-find-package package)))
     ;; (break "Cheking ILISP-PRINT-INFO-MESSAGE")
     (cond ((null real-symbol)
	    (format t "")
	    ;; (ilisp-message t "symbol ~S not present in ~S." symbol package)
	    (values))
	   ((special-form-p real-symbol)
	    (format t "~S: special-operator." real-symbol)
	    (values))
	   ((fboundp real-symbol)
	    (print-function-arglist real-symbol))
	   ((ignore-errors (boundp real-symbol))
	    (format t "~S is bound to ~S."
		    real-symbol (symbol-value real-symbol))
	    (values))
	   (t
	    (format t "Symbol ~S is unbound." real-symbol)
	    (values))))))


;;;
(defun ilisp-documentation (symbol package type)
  "Return the TYPE documentation for SYMBOL in PACKAGE.
If TYPE is \(qualifiers* (class ...)), the appropriate method will be found."
  (ilisp-errors
   (let* ((real-symbol (ilisp-find-symbol symbol package))
	  (type (if (and (not (zerop (length type)))
			 (eq (elt type 0) #\())
		    (let ((*package* (ilisp-find-package package)))
		      (read-from-string type))
		    (ilisp-find-symbol type package))))
     (when (listp type)
       (setq real-symbol
	     (funcall
	      (find-symbol "FIND-METHOD" (or (find-package "CLOS")
					     (find-package "PCL")
					     *package*))
	      (symbol-function real-symbol)
	      (reverse
	       (let ((quals nil))
		 (dolist (entry type quals)
		   (if (listp entry)
		       (return quals)
		       (setq quals (cons entry quals))))))
	      (reverse
	       (let ((types nil))
		 (dolist (class (first (last type)) types)
		   (setq types
			 (cons (funcall
				(find-symbol "FIND-CLASS"
					     (or (find-package "CLOS")
						 (find-package "PCL")
						 *package*))
				class) types))))))))
     (if real-symbol
	 (if (symbolp real-symbol)
	     (documentation real-symbol type)
	     ;; Prevent compiler complaints
	     (eval `(documentation ,real-symbol)))
	 (format nil "~A has no ~A documentation" symbol type)))))

;;;
(defun ilisp-macroexpand (expression package)
  "Macroexpand EXPRESSION as long as the top level function is still a macro." 
  (ilisp-errors
   (let ((*print-length* nil)
	 (*print-level* nil)
	 (*package* (ilisp-find-package package)))
     (pprint (#-allegro macroexpand #+allegro excl::walk
			(read-from-string expression))))))

;;;
(defun ilisp-macroexpand-1 (expression package)
  "Macroexpand EXPRESSION once."
  (ilisp-errors
   (let ((*print-length* nil)
	 (*print-level* nil)
	 (*package* (ilisp-find-package package)))
     (pprint (macroexpand-1 (read-from-string expression))))))


(defun ilisp-trace (symbol package breakp)
  "Trace SYMBOL in PACKAGE."
  (declare (ignore breakp)) ; No way to do this in CL.
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (when real-symbol (eval `(trace ,real-symbol))))))


(defun ilisp-untrace (symbol package)
  "Untrace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (when real-symbol (eval `(untrace ,real-symbol))))))


;;; ilisp-compile-file-extension --
;;;
;;; 19990806 Marco Antoniotti

(defun ilisp-compile-file-extension ()
  (pathname-type (compile-file-pathname "ilisp-foo")))

   
;;;
(defun ilisp-compile-file (file extension)
  "Compile FILE putting the result in FILE+EXTENSION."
  (ilisp-errors
   (compile-file file
		 :output-file 
		 (merge-pathnames (make-pathname :type extension) file))))

;;;
(defun ilisp-casify (pattern string lower-p upper-p)
  "Return STRING with its characters converted to the case of PATTERN.
It continues with the 'last case' beyond the end."
  (cond (lower-p (string-downcase string))
	(upper-p (string-upcase string))
	(t
	 (let (case)
	   (concatenate
	    'string
	    (map 'string
		 #'(lambda (p s)
		     (setq case (if (upper-case-p p)
				    #'char-upcase
				    #'char-downcase))
		     (funcall case s))
		 pattern string)
	    (map 'string case (subseq string (length pattern))))))))

;;;
(defun ilisp-words (string)
  "Return STRING broken up into words.
Each word is (start end delimiter)."
  (do* ((length (length string))
	(start 0)
	(end t)
	(words nil))
       ((null end) (nreverse words))
    (if (setq end (position-if-not #'alphanumericp string :start start))
	(setq words (cons (list end (1+ end) t)
			  (if (= start end)
			      words
			      (cons (list start end nil) words)))
	      start (1+ end))
	(setq words (cons (list start length nil) words)))))

;;;
(defun ilisp-match-words (string pattern words)
  "Match STRING to PATTERN using WORDS."
  (do* ((strlen (length string))
	(words words (cdr words))
	(word (first words) (first words))
	(start1 (first word) (first word))
	(end1 (second word) (second word))
	(delimiter (third word) (third word))
	(len (- end1 start1) (and word (- end1 start1)))
	(start2 0)
	(end2 len))
       ((or (null word) (null start2)) start2)
    (setq end2 (+ start2 len)
	  start2
	  (if delimiter
	      (position (elt pattern start1) string :start start2)
	      (when (and (<= end2 strlen)
			 (string= pattern string
				  :start1 start1 :end1 end1
				  :start2 start2 :end2 end2))
		(1- end2))))
    (when start2 (incf start2))))

;;;
(defun ilisp-matching-symbols (string package
				      &optional
				      (function-p nil)
				      (external-p nil)
				      (prefix-p nil))
  "Return a list of the symbols that have STRING as a prefix in PACKAGE.
FUNCTION-P indicates that only symbols with a function value
should be considered.  EXTERNAL-P indicates that only external symbols
should be considered.  PREFIX-P means that partial matches should not
be considered.  The returned strings have the same case as the
original string."
  (ilisp-errors
   (let* ((lower-p (notany #'upper-case-p string))
	  (upper-p (notany #'lower-case-p string))
	  (no-casify (eq (ilisp-readtable-case *readtable*) :preserve))
	  (symbol-string (ilisp-symbol-name string))
	  (length (length string))
	  (results nil)
	  (*print-length* nil)
	  (*package* (ilisp-find-package package)))
     (labels
	 (
	  ;; Check SYMBOL against PATTERN
	  (check-symbol (symbol pattern)
	    (let ((name (symbol-name symbol)))
	      (when (and (or (not function-p) (fboundp symbol))
			 (>= (length name) length)
			 (string= pattern name :end2 length))
		(push (list (if no-casify
				name
				(ilisp-casify pattern name lower-p upper-p)))
		      results))))
	  ;; Check SYMBOL against PATTERN using WORDS 
	  (check-symbol2 (symbol pattern words)
	    (let ((name (symbol-name symbol)))
	      (when (and (or (not function-p) (fboundp symbol))
			 (ilisp-match-words name pattern words))
		(push (list (if no-casify
				name
				(ilisp-casify pattern name lower-p upper-p)))
		      results)))))
       (if external-p
	   (do-external-symbols (symbol *package*)
	     (check-symbol symbol symbol-string))
	   (progn
	     ;; KCL does not go over used symbols.
	     #+(or kcl ibcl ecl)
	     (dolist (used-package (package-use-list *package*))
	       (do-external-symbols (symbol used-package)
		 (check-symbol symbol symbol-string)))
	     (do-symbols (symbol *package*)
	       (check-symbol symbol symbol-string))))
       (unless (or results prefix-p)
	 (let ((words (ilisp-words symbol-string)))
	   (if external-p
	       (do-external-symbols (symbol *package*)
		 (check-symbol2 symbol symbol-string words))
	       (progn
		 ;; KCL does not go over used symbols.
		 #+(or kcl ibcl ecl)
		 (dolist (used-package (package-use-list *package*))
		   (do-external-symbols (symbol used-package)
		     (check-symbol2 symbol symbol-string words)))
		 (do-symbols (symbol *package*)
		   (check-symbol2 symbol symbol-string words))))))
       ;; 19990806 Unknown Author (blame Marco Antoniotti for this)
       ;; () doesn't depend on *PACKAGE*
       ;;
       ;; (prin1 results)
       (if results (prin1 results) (princ "()"))
       nil))))


(eval-when (load eval)
  (when
      #+(and :CMU :CMU17)
      (eval:interpreted-function-p #'ilisp-matching-symbols)
      #-(and :CMU :CMU17)
      (not (compiled-function-p #'ilisp-matching-symbols))
      (ilisp-message *standard-output*
		     "File is not compiled, use M-x ilisp-compile-inits")))


;;; end of file -- cl-ilisp.lisp --
