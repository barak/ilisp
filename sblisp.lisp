;;; -*- Mode: Lisp -*-

;;; sblisp.lisp --
;;; Make SBCL work with ILISP.
;;;
;;; This init file is compatible with a version of SBCL,
;;; where version >= 0.6.10!
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: sblisp.lisp,v 1.9 2001/05/12 22:10:53 marcoxa Exp $


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

;;; Normally, errors which occur while in the debugger are just ignored, unless
;;; the user issues the "flush" command, which toggles this behavior.
;;;
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



(export '(arglist source-file sblisp-trace))

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

;;; Patch suggested by Richard Harris <rharris@chestnut.com>

;;; FUN-DEFINED-FROM-PATHNAME takes a symbol or function object.  It
;;; returns a pathname for the file the function was defined in.  If it was
;;; not defined in some file, then nil is returned.

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

(defun sblisp-trace (symbol package breakp)
  "Trace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (setq breakp (read-from-string breakp))
     (when real-symbol (eval `(trace ,real-symbol :break ,breakp))))))

;;; end of file -- sblisp.lisp --

