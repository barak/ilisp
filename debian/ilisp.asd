;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ilisp.asd
;;;; Purpose:       ASDF system definition file for ilisp package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  March 2003
;;;;
;;;; $Id: ilisp.asd,v 1.4 2003/03/01 17:43:19 kevinrosenberg Exp $
;;;;
;;;; UFFI users are granted the rights to distribute and use this software
;;;; as governed by the terms of the ILISP license.
;;;; *************************************************************************

(in-package :asdf)

#+(or allegro lispworks cmu mcl sbcl scl)
(defsystem ilisp
  :name "uffi"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :version "1.0.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "ILISP license"
  :description "Inferiour lisp system loader"
  
  :perform (load-op :after (op ilisp)
    (pushnew :ilisp cl:*features*))

  :components
  ((:module :src
	    :components
	    ((:file "ilisp-pkg")
	     (:file "cl-ilisp")
	     (:file "cl-chs-init")
	     #+allegro (:file "allegro.lisp" :depends-on "ilisp-pkg")
	     #+cmu (:file "cmulisp.lisp" :depends-on "ilisp-pkg")
	     #+lispworks (:file "lispworks.lisp" :depends-on "ilisp-pkg")
	     #+openmcl (:file "openmcl.lisp" :depends-on "ilisp-pkg")
	     #+sbcl (:file "sbcl.lisp" :depends-on "ilisp-pkg")
	))))

