;;; -*- Mode: Lisp -*-

;;; sbcl.lisp --

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


;;; This file was adapted from cmulisp.lisp, which was written by Todd
;;; Kaufmann in May 1990
;;;
;;; This init file was last tested with SBCL 0.6.12.21

(in-package "ILISP")

;;;% CMU CL does not define defun as a macro
(defun ilisp-compile (form package filename)
  "Compile FORM in PACKAGE recording FILENAME as the source file."
  (ilisp-errors
   (ilisp-eval
    (format nil "(funcall (compile nil '(lambda () ~A)))" form)
    package filename)))

;;;% Stream settings, when running connected to pipes.
;;;
;;; When SBCL is running as a piped process, it still manages to open
;;; /dev/tty to use it for the *terminal-io* stream.  This means that an
;;; error will cause lisp to stop and wait for input from /dev/tty, which is
;;; probably not available and certainly not what you were expecting.
;;;
;;; We want it to use the same input that the user is typing at, ie,
;;; the pipe (stdin).

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

;;; Normally, errors which occur while in the debugger are just ignored, unless
;;; the user issues the "flush" command, which toggles this behavior.

(setq sb-debug:*flush-debug-errors* nil)  ; allow multiple error levels.


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

(export '(arglist source-file sbcl-trace))

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
  (sb-impl-errors
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


(defun sbcl-trace (symbol package breakp)
  "Trace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (setq breakp (read-from-string breakp))
     (when real-symbol (eval `(trace ,real-symbol :break ,breakp))))))

;;; end of file -- sbcl.lisp --

