;;; -*- Mode: Lisp -*-

;;; sblisp.lisp --

;;; This file is part of ILISP.
;;; Version: 5.10.1
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996, 1997, 1998, 1999 Marco Antoniotti and Rick Campbell
;;;               2000 Matthias Hölzl
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
;;; Todd Kaufmann    May 1990
;;;
;;; Make CMU CL run better within GNU inferior-lisp (by ccm).
;;;
;;; This init file is compatible with version of SBCL (version >= 0.6.2!)


(in-package "ILISP")

;;;% Stream settings, when running connected to pipes.
;;;
;;; This fixes a problem when running piped: When CMU is running as a piped
;;; process, *terminal-io* really is a terminal; ie, /dev/tty.  This means an
;;; error will cause lisp to stop and wait for input from /dev/tty, which it
;;; won't be able to grab, and you'll have to restart your lisp.  But we want
;;; it to use the same input that the user is typing in, ie, the pipe (stdin).
;;; This fixes that problem, which only occurs in the CMU cores of this year.
;;;

(defvar *Fix-pipe-streams* T
  "Set to Nil if you want them left alone.  And tell me you don't get stuck.")

(when (and *Fix-pipe-streams*
	   (sb-impl::synonym-stream-p *terminal-io*)
	   (eq (sb-impl::synonym-stream-symbol *terminal-io*)
	       'sb-impl::*tty*))
  (setf *terminal-io* (make-two-way-stream sb-impl::*stdin* sb-impl::*stdout*))
  ;; *query-io* and *debug-io* are synonym streams to this, so this fixes
  ;; everything.
  )


;;;% Debugger extensions

;;;%% Implementation of a :pop command for CMU CL debugger

;;;
;;; Normally, errors which occur while in the debugger are just ignored, unless
;;; the user issues the "flush" command, which toggles this behavior.
;;;
(setq sb-debug:*flush-debug-errors* nil)  ; allow multiple error levels.

;;; This implementation of "POP" simply looks for the first restart that says
;;; "Return to debug level n" or "Return to top level." and executes it.
;;;
(sb-debug::def-debug-command "POP"  ()
  ;; find the first "Return to ..." restart
  (if (not (boundp 'sb-debug::*debug-restarts*))
      (error "You're not in the debugger; how can you call this!?")
      (labels ((find-return-to (restart-list num)
		 (let ((first
			(member-if
			 #'(lambda (restart)
			     (string= (funcall
				       (sb-conditions::restart-report-function 
					restart)
				       nil)
				      "Return to " :end1 10))
			 restart-list)))
		   (cond ((zerop num) (car first))
			 ((cdr first) (find-return-to (cdr first)
						      (1- num)))))))
	(let* ((level (sb-debug::read-if-available 1))
	       (first-return-to (find-return-to 
				 sb-debug::*debug-restarts* (1- level))))
	  (if (null first-return-to)
	      (format *debug-io* "pop: ~d is too far" level)
	      (sb-debug::invoke-restart-interactively first-return-to)
	      )))))


;;;%% arglist/source-file utils.

(defun get-correct-fn-object (sym)
  "Deduce how to get the \"right\" function object and return it."
  (let ((fun (or (macro-function sym)
		 (and (fboundp sym) (symbol-function sym)))))
    (cond (fun
	   (when (and (= (sb-impl::get-type fun) #.sb-vm:closure-header-type)
		      (not (sb-eval:interpreted-function-p fun)))
	     (setq fun (sb-impl::%closure-function fun)))
	   fun)
	  (t
	   (error "Unknown function ~a.  Check package." sym)
	   nil))))



(export '(arglist source-file sblisp-trace))

;;;%% arglist - return arglist of function - former version

;;(defun arglist (symbol package)
;;  (ilisp-errors
;;   (let* ((x (ilisp-find-symbol symbol (package-name package)))
;;          (fun (get-correct-fn-object x)))
;;     (values
;;      (cond ((sb-eval:interpreted-function-p fun) 
;;             (sb-eval:interpreted-function-arglist fun))
;;            ((= (sb-impl::get-type fun)
;;                #.sb-vm:funcallable-instance-header-type) 
;;             ;; generic function / method
;;             (sb-pcl::generic-function-pretty-arglist fun))
;;            ((compiled-function-p fun)
;;             (let ((string-or-nil
;;                    (sb-impl::%function-arglist fun)))
;;               (if string-or-nil
;;                   (read-from-string string-or-nil)
;;                   "No argument info.")))
;;            (t (error "Unknown type of function")))))))
;;

;;; 2000-04-02: Martin Atzmueller
;;; better (more bulletproof) arglist code adapted from cmulisp.lisp:

(defun extract-function-info-from-name (sym)
  (let ((mf (macro-function sym)))
    (if mf
	(values mf :macro)
	(if (fboundp sym)
	    (values (symbol-function sym) :function)
	    (values nil nil)))))

(defun arglist (symbol package)
  (ilisp-errors
   (let* ((package-name (if (packagep package)
			    (package-name package)
			    package))
	  (x (ilisp-find-symbol symbol package-name)))
     (flet ((massage-arglist (args)
	      (typecase args
		(string (if (or (null args) (string= args "()"))
			    ""
			    args))
		(list (format nil "~S" args))
		(t ""))))

       (multiple-value-bind (func kind)
	   (extract-function-info-from-name x)
	 ;; (print func *trace-output*)
	 ;; (print kind *trace-output*)
	 (if (and func kind)
	     (case (sb-impl::get-type func)
	       ((#.sb-vm:closure-header-type
		 #.sb-vm:function-header-type
		 #.sb-vm:closure-function-header-type)
		(massage-arglist
		 (funcall #'sb-impl::%function-arglist
			  func)))

	       (#.sb-vm:funcallable-instance-header-type
		(typecase func
		  (sb-kernel:byte-function
		   "Byte compiled function or macro, no arglist available.")
		  (sb-kernel:byte-closure
		   "Byte compiled closure, no arglist available.")
		  ((or generic-function sb-pcl::generic-function)
		   (sb-pcl::generic-function-pretty-arglist func))
		  (sb-eval:interpreted-function
		   (massage-arglist
		    (sb-eval::interpreted-function-arglist func)))
		
		  (t (print 99 *trace-output*) "No arglist available.")
		  ))			; typecase
	       (t "No arglist available.")) ; case
	     "Unknown function - no arglist available." ; For the time
					; being I just
					; return this
					; value. Maybe
					; an error would
					; be better.
	     ))))))

;;; source-file symbol package type --
;;; New version provided by Richard Harris <rharris@chestnut.com> with
;;; suggestions by Larry Hunter <hunter@work.nlm.nih.gov>.

(defun source-file (symbol package type)
  (declare (ignore type))
  (ilisp-errors
   (let* ((x (ilisp-find-symbol symbol package))
	  (fun (get-correct-fn-object x)))
     (when (and fun (not (sb-eval:interpreted-function-p fun)))
	   ;; The hack above is necessary because CMUCL does not
	   ;; correctly record source file information when 'loading'
	   ;; a non compiled file.
	   ;; In this case we fall back on the TAGS machinery.
	   ;; (At least as I underestand the code).
	   ;; Marco Antoniotti 11/22/94.
	   (cond ((sb-pcl::generic-function-p fun)
		  (dolist (method (sb-pcl::generic-function-methods fun))
		    (print-simple-source-info
		     (or (sb-pcl::method-fast-function method)
			 (sb-pcl::method-function method))))
		  t)
		 (t (print-simple-source-info fun)))))))

;;; Old version. Left here for the time being.
;(defun source-file (symbol package type)
;  (declare (ignore type))
;  (ilisp-errors
;   (let* ((x (ilisp-find-symbol symbol package))
;	  (fun (get-correct-fn-object x)))
;     (when fun
;       (cond ((= (sb-impl::get-type fun)
;		 #.sb-vm:funcallable-instance-header-type)
;	      ;; A PCL method! Uh boy!
;	      (dolist (method (sb-pcl::generic-function-methods fun))
;		(print-simple-source-info
;		 (sb-impl::%closure-function (sb-pcl::method-function method))))
;	      t)
;	     (t (print-simple-source-info fun)))))))


;;; Patch suggested by Richard Harris <rharris@chestnut.com>

;;; FUN-DEFINED-FROM-PATHNAME takes a symbol or function object.  It
;;; returns a pathname for the file the function was defined in.  If it was
;;; not defined in some file, then nil is returned.
;;;
;;; FUN-DEFINED-FROM-PATHNAME is from hemlock/rompsite.lisp (cmucl17f), 
;;; with added read-time conditionalization to work in older versions
;;; of cmucl.  It may need a little bit more conditionalization for
;;; some older versions of cmucl.

(defun fun-defined-from-pathname (function)
  "Returns the file where FUNCTION is defined in (if the file can be found).
Takes a symbol or function and returns the pathname for the file the
function was defined in.  If it was not defined in some file, nil is
returned."
  (flet ((frob (code)
	   (let ((info (sb-kernel:%code-debug-info code)))
	     (when info
	       (let ((sources (sb-c::debug-info-source info)))
		 (when sources
		   (let ((source (car sources)))
		     (when (eq (sb-c::debug-source-from source) :file)
		       (sb-c::debug-source-name source)))))))))
    (typecase function
      (symbol (fun-defined-from-pathname (fdefinition function)))
      (sb-kernel:byte-closure
       (fun-defined-from-pathname
	(sb-kernel:byte-closure-function function)))
      (sb-kernel:byte-function
       (frob (sb-c::byte-function-component function)))
      (function
       (frob (sb-kernel:function-code-header
	      (sb-kernel:%function-self function))))
      (t nil))))


;;; print-simple-source-info --
;;; Patches suggested by Larry Hunter <hunter@work.nlm.nih.gov> and
;;; Richard Harris <rharris@chestnut.com>
;;; Nov 21, 1994.

(defun print-simple-source-info (fun)
  (let ((path (fun-defined-from-pathname fun)))
    (when (and path (probe-file path))
      (print (namestring (truename path)))
      t)))


;;; Old version (semi patched). Left here for the time being.
;(defun print-simple-source-info (fun)
;  (let ((info (sb-kernel:%code-debug-info
;	       (sb-kernel:function-code-header fun))))
;    (when info
;	  (let ((sources (sb-c::compiled-debug-info-source info)))
;	    (when sources
;		  (dolist (source sources)
;			  (let ((name (sb-c::debug-source-name source)))
;			    (when (eq (sb-c::debug-source-from source) :file)
;				  ;; Patch suggested by
;				  ;; hunter@work.nlm.nih.gov (Larry
;				  ;; Hunter) 
;				  ;; (print (namestring name)) ; old
;				  (print (truename name))
;				  )))
;		  t)))))


(defun sblisp-trace (symbol package breakp)
  "Trace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (setq breakp (read-from-string breakp))
     (when real-symbol (eval `(trace ,real-symbol :break ,breakp))))))

;;; end of file -- sblisp.lisp --

