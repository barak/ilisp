;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ilisp.asd
;;;; Purpose:       ASDF definition file for ILISP
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2002
;;;;
;;;; $Id: ilisp.asd,v 1.2 2002/09/16 17:07:47 kevinrosenberg Exp $
;;;; This file, part of ILISP, is Copyright (c) 2002 by Kevin Rosenberg.
;;;; It's is licensed with the GNU Public License.
;;;; *************************************************************************

(in-package :asdf)

#+(or cmu sbcl openmcl clisp lispworks allegro)
(defsystem ilisp
  :components ((:file "ilisp-pkg")
	       (:file "cl-ilisp" :depends-on ("ilisp-pkg"))
	       #+cmu (:file "cmulisp" :depends-on ("cl-ilisp"))
	       #+sbcl (:file "sbcl" :depends-on ("cl-ilisp"))
	       #+openmcl (:file "openmcl" :depends-on ("cl-ilisp"))
	       #+clisp (:file "cl-chs-init" :depends-on ("cl-ilisp"))
	       #+lispworks (:file "lispworks" :depends-on ("cl-ilisp"))
	       #+allegro (:file "allegro" :depends-on ("cl-ilisp"))
	       ))
