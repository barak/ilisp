;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-val.el --

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



;;;
;;; ILISP buffer value interface
;;;
;;;

;;;
(defun ilisp-value (variable &optional no-error-p)
  "Return the value of VARIABLE in the ILISP buffer.
If NO-ERROR-P is NIL, then an error will be signalled if VARIABLE is nil."
  (save-excursion
    (set-buffer (ilisp-buffer))
    (let ((value (eval variable)))
      (if value
	  value
	  (if no-error-p
	      nil
	      (error "%s is not defined." variable))))))

;;;
(defun set-ilisp-value (variable value)
  "Set the value of VARIABLE in the ILISP buffer."
  (save-excursion
    (set-buffer (ilisp-buffer))
    (set variable value)))

;;; end of file -- ilisp-val.el --
