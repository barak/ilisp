;;; -*- Mode: Lisp -*-

;;; ilisp-pkg.lisp --
;;; ANSI CL DEFPACKAGE definition for ILISP.
;;;
;;; Common Lisp initializations
;;;
;;; Author: Marco Antoniotti, marcoxa@cs.nyu.edu
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-pkg.lisp,v 1.2 2001/05/12 22:10:53 marcoxa Exp $

;;;----------------------------------------------------------------------------
;;; Prologue

#+(or allegro-v4.0 allegro-v4.1)
(eval-when (compile load eval)
  (setq excl:*cltl1-in-package-compatibility-p* t))


;;;----------------------------------------------------------------------------
;;; Definitions

;;; ILISP package --

;;;
;;; GCL 2.2 doesn't have defpackage (yet) so we need to put the export
;;; here. (toy@rtp.ericsson.se)
;;;
;;; Please note that while the comment and the fix posted by Richard
;;; Toy are correct, they are deprecated by at least one of the ILISP
;;; maintainers. :) By removing the 'nil' in the following #+, you
;;; will fix the problem but will not do a good service to the CL
;;; community.  The right thing to do is to install DEFPACKAGE in your
;;; GCL and to write the GCL maintainers and to ask them to
;;; incorporate DEFPACKAGE in their standard builds.
;;; Marco Antoniotti <marcoxa@icsi.berkeley.edu> 19960715
;;;

#-(and nil gcl)
(defpackage "ILISP" (:use "COMMON-LISP" #+:CMU "CONDITIONS")
  ;; The following symbols should properly 'shadow' the inherited
  ;; ones.
  (:export "ILISP-ERRORS"
	   "ILISP-SAVE"
	   "ILISP-RESTORE"
	   "ILISP-SYMBOL-NAME"
	   "ILISP-FIND-SYMBOL"
	   "ILISP-FIND-PACKAGE"
	   "ILISP-EVAL"
	   "ILISP-COMPILE"
	   "ILISP-DESCRIBE"
	   "ILISP-INSPECT"
	   "ILISP-ARGLIST"
	   "ILISP-DOCUMENTATION"
	   "ILISP-MACROEXPAND"
	   "ILISP-MACROEXPAND-1"
	   "ILISP-TRACE"
	   "ILISP-UNTRACE"
	   "ILISP-COMPILE-FILE-EXTENSION"
	   "ILISP-COMPILE-FILE"
	   "ILISP-CASIFY"
	   "ILISP-MATCHING-SYMBOLS"
	   "ILISP-CALLERS"
	   "ILISP-SOURCE-FILES"
	   "ILISP-PRINT-INFO-MESSAGE"
	   )
  )
;;; ILISP --

;;; end of file -- ilisp-pkg.lisp --
