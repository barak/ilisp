;;; -*- Mode: Emacs-Lisp -*-

;;; custom-ilisp.el --

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

;;; ILISP Customization.

;;; WARNING!  This file is not loaded yet.  Loading it may cause some
;;; problems.


(defgroup ILisp nil
  "The ILisp Inferior Lisp System."
  :group 'programming
  :group 'lisp)

(defgroup ILisp-interaction nil
  "ILisp interaction customization."
  :group 'ILisp)

(defcustom ilisp-*prefix* "\C-z"
  "Prefix sequence for ILisp commands."
  :group 'ILisp-interaction
  )

(defcustom lisp-no-popper 'message
  "*T if all output goes to the inferior LISP rather than in a pop-up window.
Use 'message' if you want output of one line to go to the echo area
(usually the Minibuffer) or to a pop-up window if more.  You should
probably also set 'comint-always-scroll' to T as well so that output is
always visible."
  :group 'ILisp-interaction
  :options '(nil t message)
  )


;;; end of file -- custom-ilisp.el -*-

