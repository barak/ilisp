;Here goes.  Actually there are two files.  There's hyperspec.el and
;another file called elisp-file-basic.el which is some file utilities
;used by hyperspec.el.  There's also hyperspec-completions.el but
;that's a 185k file that can be regenerated.

;Note that the suggested keybinding is C-M-h which isn't too good.  I
;change it to C-c D.

;Here's hyperspec.el:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CUT HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: emacs-lisp; -*-
;;;
;;; Author: Stephen Carney <carney@gvc.dec.com>
;;; Last modified: Tue Jul  8 14:57:53 1997

;;;; This package provides an interface to the Common Lisp HyperSpec 
;;;; by Kent Pitman <kmp@harlequin.com> at Harlequin.  The HyperSpec
;;;; can be browsed at
;;;; <http://www.harlequin.com/books/HyperSpec/FrontMatter/index.html>
;;;; and the kit can be downloaded from 
;;;; <http://www.harlequin.com/books/HyperSpec/index.html>.
;;;;
;;;;
;;;; USAGE
;;;;
;;;; In a Lisp mode buffer, the hyperspec-lookup command (normally bound
;;;; to C-M-h) fetches the web page corresponding to a HyperSpec
;;;; definition near the editing point.  The web page may be fetched
;;;; from a local copy of the HyperSpec, or from a network (Internet)
;;;; accessible copy.
;;;;
;;;; The hyperspec-feature command fetches various top-level web pages.
;;;;
;;;;
;;;; INSTALLATION
;;;;
;;;; 1. Ensure that the following files are in your load-path:
;;;;      hyperspec.el 
;;;;        (<http://www-digital.cern.ch/carney/emacs/hyperspec.el>)
;;;;      hyperspec-completions.el
;;;;        (<http://www-digital.cern.ch/carney/emacs/hyperspec-completions.el>)
;;;;      browse-url.el (a public package, with a copy at:
;;;;         <http://www-digital.cern.ch/carney/emacs/browse-url.el>)
;;;;
;;;;    If you use the emacs w3 (GNUscape) browser, then you must ensure
;;;;    that it is in your path.  The w3 browser is avilable at:
;;;;      <http://www.cs.indiana.edu/elisp/w3/download.html>
;;;;
;;;; 2. (optional) M-x byte-compile-file the following files:
;;;;      hyperspec.el
;;;;      hyperspec-completions.el
;;;;      browse-url.el
;;;;
;;;; 3. Add the following to your .emacs file:
;;;;      ;; Autoload hyperspec
;;;;      (autoload 'hyperspec-lookup "hyperspec" "Load hyperspec" t)
;;;;      ;; Bind key C-M-h in Lisp mode buffers to hyperspec-lookup
;;;;      (define-key lisp-mode-map 
;;;;          (if (string-match "XEmacs" (emacs-version))
;;;;              '(meta control h)
;;;;              "\M-\C-h")
;;;;        'hyperspec-lookup)
;;;;
;;;; 4. If you have a machine-local copy of the HyperSpec,
;;;;    add the following (or the appropriate) to your .emacs file:
;;;;      (setq *hyperspec-url* "file:/doc/lisp/hyperspec/HyperSpec/")
;;;;    The *hyperspec-url* should point to the base of the HyperSpec
;;;;    directory structure.  See the documentation for *hyperspec-url*.
;;;;
;;;;
;;;; CUSTOMIZATION
;;;;
;;;; By default, Hyperspec lookups will go to Harlequin's web site.
;;;; The access speed will depend on your network connectivity.
;;;; If you experience slow access, consider installing a local copy.
;;;; 
;;;; The HyperSpec interface requires the public browse-url package.
;;;; Read the beginning of the file to select which browser to display
;;;; the HyperSpec web pages.  The default browser is Netscape. 
;;;;
;;;;
;;;; REBUILDING hyperspec-completions.el
;;;;
;;;; The HyperSpec completions file, hyperspec-completions.el, was generated
;;;; using M-x hyperspec-save-completions.  You can regenerate the completions
;;;; file if you have the data files (provided in the downloadable
;;;; HyperSpec kit).  However, you should only need to do this if the
;;;; HyperSpec changes.  To regenerate the completions:
;;;;
;;;;   - Ensure that elisp-file-basic.el is in your load-path
;;;;     (<http://www-digital.cern.ch/carney/emacs/elisp-file-basic.el>).
;;;;   - Set *hyperspec-url* to the approriate local directory
;;;;     (something starting with "file:.../HyperSpec/") 
;;;;   - Set *hyperspec-completion-file* to be the output file name.
;;;      You might try: (expand-file-name hyperspec-completion-file-name "~")
;;;;   - M-x hyperspec-save-completions
;;;;
;;;;
;;;; Please send comments, questions, and bugs to Steve Carney
;;;; <carney@gvc.dec.com>.
;;;;
;;;; Happy Lisp browsing...

(require 'cl)

;;;
;;; Constants
;;;

(defconst hyperspec-completion-name "hyperspec-completions"
  "Define the file name (without an extension) of the file containing the
completion table.")

(defconst hyperspec-completion-file-name (concat hyperspec-completion-name ".el")
  "Define the file name (with an extension) of the file containing the
completion table.")

(defconst hyperspec-public-url "http://www.harlequin.com/books/HyperSpec/"
  "Define the public HyperSpec base URL on the Internet.  The URL must
end with a `/' to indicate that it is a directory reference.")

(defconst hyperspec-version "1.1"
  "Define the version of the HyperSpec emacs interface.")

;;;
;;; Public variables
;;;

(defvar *hyperspec-url* hyperspec-public-url
  "*Define the base URL to retrieve HyperSpec web pages.  The URL must
end with a `/' to indicate that it is a directory reference.")

(defvar *hyperspec-completion-file* hyperspec-completion-file-name
  "*Define the file where the HyperSpec completions are saved.
If you are creating a completions file, then you will probably
want to set this variable explicitly to include an absolute
directory reference.")

(defvar *hyperspec-w3-fetch* 
  (if (eq (if (fboundp 'device-type) (device-type) window-system)
          'x)
      'w3-fetch-other-frame
      'w3-fetch-other-window)
  "*Define a variable to allow split windows browsing for the emacs w3
browser.  The varible is the name of a w3 function:
  w3-fetch
  w3-fetch-other-frame
  w3-fetch-other-window")

;;;
;;; Private variables
;;;

(defvar *hyperspec-data-files-names* 
  '("Issue-Cross-Refs.text"
    "Issue-Writeups.text"
    "Symbol-Table.text")
  "Define the names of the HyperSpec files containing key and file references.")

(defvar *hyperspec-features* 
  (let* ((home "FrontMatter/index.html")
         (features
          (append 
           (list (list "Home"   home)
                 (list "Index"  home)    ;Synonym for home
                 (list "Top"    home))   ;Synonym for home
           '(("Starting-Points" "FrontMatter/Starting-Points.html")
             ("Highlights"      "FrontMatter/Highlights.html")
             ("Contents"        "FrontMatter/Chapter-Index.html")
             ("Master-Index"    "FrontMatter/Master-Index.html")
             ("Symbol-Index"    "FrontMatter/Symbol-Index.html")
             ("Glossary"        "Body/sec_26-1.html")
             ("X3J13-Issues"    "FrontMatter/X3J13-Issues.html")
             ("Help"            "FrontMatter/About-HyperSpec.html#Top")))))
    (append
     features
     (apply 'append
            (mapcar #'(lambda (fn)
                        (mapcar #'(lambda (pair)
                                    (list (funcall fn (first pair)) (second pair)))
                                features))
                    '(downcase upcase)))))
  "Define a completion table for top-level entry points to the
HyperSpec.  The first value of each entry is the completion string, and
the second value is the corresponding file reference relative to
*hyperspec-url*.")

(defvar *hyperspec-completion-table* nil
  "Define the global variable that will contained the HyperSpec
completion table.")

(defvar *hyperspec-last-lookup* nil
  "Define a variable to save the last lookup.  This is used to provide a
default if nothing can be found near the editor's point.")

(defvar *hyperspec-last-lookup-entry* nil
  "Define a variable to save the last file lookup for an entry which had
multiple file entries.")

(defvar *hyperspec-last-feature* nil
  "Define a variable to save the last feature lookup.")

(defvar *hyperspec-w3-frame* nil
  "Define a variable to save a w3 frame for browsing")


;;;
;;; Private Functions
;;;

; (autoload 'byte-compile-file "bytecomp" "Load bytecomp")
(autoload 'find-tag-default "etags" "Load etags")
(autoload 'w3-fetch "w3" "autoload w3" t)
(autoload 'with-input-file "elisp-file-basic" "Load elisp-file-basic" nil 'macro)
(autoload 'with-output-file "elisp-file-basic" "Load elisp-file-basic" nil 'macro)

;;; Although http references do not contain directory and file references
;;; per se, it is convenient to think of them in this way for the
;;; HyperSpec.  In the HyperSpec, http references are directly mapped into
;;; directory and file references.
(defun hyperspec-parse-url (&optional url)
  "Parse a URL and return 3 values: protocol, directory, and file.  The
(required) protocol must be `file:' or `http:'.  If the protocol is http,
the hostname part is also included in the returned protocol.  The (required)
directory is the component following the protocol, and includes up to
the last `/'.  The (optional) file component follows the last `/'."
  (let ((url (or url *hyperspec-url*)))
    (if (string-match "^\\(file:\\|http://[^/]+\\)\\(.*/\\)\\([^/]*\\)?$" url)
      (let ((protocol (substring url (match-beginning 1) (match-end 1)))
            (directory (substring url (match-beginning 2) (match-end 2)))
            (file (if (match-beginning 3)
                      (unless (= (match-beginning 3) (match-end 3))
                        (substring url (match-beginning 3) (match-end 3))))))
        (values protocol directory file))
      (error "Cannot parse HyperSpec url %s." url))))

(defun hyperspec-protocol (&optional url)
  (multiple-value-bind (protocol)
      (hyperspec-parse-url)
    protocol))

(defun hyperspec-directory (&optional url)
  (multiple-value-bind (protocol directory)
      (hyperspec-parse-url url)
    (ignore protocol)
    directory))

(defun hyperspec-file (&optional url)
  (multiple-value-bind (protocol directory file)
      (hyperspec-parse-url url)
    (ignore protocol directory)
    file))

(defun hyperspec-data-directory ()
  "Return the data directory that contains the text files used
to build the completions."
  (expand-file-name "Data" (hyperspec-directory)))

(defun hyperspec-data-files ()
  "Return a list of the HyperSpec data (text) files."
  (let ((data-dir (hyperspec-data-directory)))
    (mapcar #'(lambda (file)
                (expand-file-name file data-dir))
            *hyperspec-data-files-names*)))

(defun hyperspec-expand-file (file &optional base-directory-p)
  "Expand a file reference into a URL."
  (concat (hyperspec-protocol)
          (expand-file-name file (if base-directory-p
                                     (hyperspec-directory)
                                     (hyperspec-data-directory)))))

(defun hyperspec-read-completion-file (&optional prompt)
  "Read the name of the file containing completions from the user."
  (read-file-name (or prompt "File: ")
                  *hyperspec-completion-file*
                  *hyperspec-completion-file*))

;;; The HyperSpec data file format is assumed to have lines alternating
;;; between HyperSpec key and its corresponding relative (to the data
;;; directory) file reference.
(defun hyperspec-build-index (file)
  "Given a HyperSpec data file, build a list of key-file pairs.
For each entry, additional key-file pairs might be generated to provide
pairs which accommodate various character cases of the key (original,
lowercase, and uppercase)."
  (message "Build index for file %s" file) (sit-for 2)
  (let ((regexp "^\\(.*\\)$")
        (entries nil)
        (lazy-message-time 0)
        (lazy-message-count 0)
        (lazy-message-limit 100))
    (flet ((lazy-message (&rest args)
             (when (or (> (incf lazy-message-count) lazy-message-limit)
                       (not (= lazy-message-time
                               (setq lazy-message-time (second (current-time))))))
               (setq lazy-message-limit 0)
               (apply 'message args))))
      (with-input-file buffer file
        (while (and (re-search-forward regexp nil t)
                    (< (point) (point-max)))
          (when (match-beginning 1)
            (let ((key (buffer-substring (match-beginning 0) (match-end 0))))
              (end-of-line)
              (when (re-search-forward regexp nil t)
                (let ((file (buffer-substring (match-beginning 0) (match-end 0))))
                  (end-of-line)
                  (lazy-message "%s  file: %s" key file)
                  (push (list key file) entries)
                  (when (string-match "[a-z]" key)
                    (push (list (upcase key) file) entries))
                  (when (string-match "[A-Z]" key)
                    (push (list (downcase key) file) entries)))))))))
    (message "Built index for file %s" file)
    entries))

(defun hyperspec-build-indexes (&rest files)
  "Merge multiple HyperSpec data (text) files into a list of key-files
pairs.  Keys that are string-equal are coalesced into a singe entry by
merging unique file references."
  (let ((entries nil))
    (mapc #'(lambda (entry)
              (multiple-value-bind (key file)
                  entry
                (let ((existing-entry (assoc* key entries :test 'string-equal)))
                  (if existing-entry
                      (multiple-value-bind (existing-key existing-files)
                          existing-entry
                        (ignore existing-key)
                        (unless (member* file existing-files :test 'equal)
                          ;; I really want to do a (setf (assoc* ...)), 
                          ;; but there isn't one.
                          (setq entries
                                (delete* key entries :key 'first :test 'string-equal))
                          (push (list key (append existing-files (list file)))
                                entries)))
                      (push (list key (list file)) entries)))))
          (apply 'append (mapcar 'hyperspec-build-index files)))
    entries))

;;;
;;; Public functions
;;;

(defun hyperspec-version ()
  "Display the current version of the HyperSpec emacs interface."
  (interactive)

  (message "HyperSpec %s" hyperspec-version))


;;; A new hyperspec-completions.el should only need to be regenerated if
;;; the HyperSpec changes.  The completions file is valid regardless of
;;; the setting of *hyperspec-url*.
;;;
;;; Generating a completions file is not a "quick" operation.  It takes
;;; approximately 2.6 minutes on a DEC 3000/500 (150MHz Alpha).
(defun hyperspec-save-completions (&optional file)
  "Generate a HyperSpec completions file, along with a corresponding
compiled version."
  (interactive (list (hyperspec-read-completion-file "Save to file: ")))

  (let ((file (or file *hyperspec-completion-file*))
        (completion-table (stable-sort 
                           (prog2
                               (message "Build competition table...")
                               (apply 'hyperspec-build-indexes (hyperspec-data-files))
                             (message "Sort completion table..."))
                           'string-lessp
                           :key 'first)))

    (message "Write completion table to %s" file)
    (with-output-file-new buffer file
      (set-buffer buffer)
      (insert ";;; -*- Mode: emacs-lisp; -*-\n"
              ";;;\n"
              ";;; Generated: " (current-time-string) "\n\n"
              "(defvar *hyperspec-completion-table*)\n"
              "(setq *hyperspec-completion-table*\n      '(")
      (let ((firstp t)
            (indentation "        "))
        (dolist (entry completion-table)
          (insert (format "%s%S\n" 
                          (concat (if firstp 
                                      (progn (setq firstp nil) "" )
                                      indentation))
                          entry)))
        (insert (concat indentation "))\n\n")
                "(provide '" hyperspec-completion-name ")\n")))
    (byte-compile-file file t)
    (message "Done.")
    file))

(defun hyperspec-fetch (url)
  (require 'browse-url)
  (message "Fetch %s" url)
  (if (eq browse-url-browser-function 'browse-url-w3)
      (if (eq *hyperspec-w3-fetch* 'w3-fetch-other-frame)
          (let ((w3-notify 'bully))
            (unless (frame-live-p *hyperspec-w3-frame*)
              (setq *hyperspec-w3-frame* (make-frame)))
            (select-frame *hyperspec-w3-frame*)
            (w3-fetch url))
          (funcall *hyperspec-w3-fetch* url))
      (funcall browse-url-browser-function url))
  (message "%s sent to browser" url)
  url)

(defun hyperspec-feature (feature)
  "Fetch a HyperSpec top-level feature, such as the index."
  (interactive 
   (list 
    (setq *hyperspec-last-feature*
          (completing-read "Feature: " *hyperspec-features* nil t 
                           (or *hyperspec-last-feature*
                               (first (first *hyperspec-features*)))))))

  (let ((file (second (assoc* feature *hyperspec-features* :test 'string-equal))))
    (unless file
      (error "Cannot find file associated with HyperSpec feature %s." feature))
    (hyperspec-fetch (hyperspec-expand-file file t))))

(defun hyperspec-lookup (tag)
  "Lookup a definition in the HyperSpec."
  (interactive 
   (list 
    (progn
      (require (intern hyperspec-completion-name))
      (setq *hyperspec-last-lookup*
            (completing-read 
             "HyperSpec tag: " *hyperspec-completion-table* nil t
             (let ((default (find-tag-default)))
               (if (assoc default *hyperspec-completion-table*)
                   default
                   *hyperspec-last-lookup*)))))))

  (unless (interactive-p)
    (require (intern hyperspec-completion-name)))

  (unless (stringp tag)
    (error "The HyperSpec lookup tag must be a string."))

  (let* ((entry (assoc tag *hyperspec-completion-table*))
         (files (second entry))
         (file-count (length files)))
    (unless entry
      (error "Unknown HyperSpec tag: %s." tag))
    (when (zerop file-count)
      (error "There were no files associated with HyperSpec tag %s." tag))
    
    (let ((url (hyperspec-expand-file 
                (cond ((or (= file-count 1) (not (interactive-p)))
                       (first files))
                      (t (setq *hyperspec-last-lookup-entry*
                               (completing-read 
                                "Subentry: " 
                                (mapcar 'list files) nil t
                                (when (and *hyperspec-last-lookup-entry*
                                           (member* *hyperspec-last-lookup-entry*
                                                    files :test 'equal))
                                  *hyperspec-last-lookup-entry*))))))))
      (hyperspec-fetch url)
      url)))

(provide 'hyperspec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CUT HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Here's elisp-file-basic.el:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CUT HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- MODE: emacs-lisp; -*-
;;;
;;; File:          elisp-file.el
;;; Last modified: Sat Jun 28 15:45:28 1997

(require 'cl)
(require 'backquote)

;;;
;;; Recursive files and directory procedures
;;;

(defvar *mapover-directory-softlinks-p* nil
  "Whether to follow symbolic links when mapping over a directory.")

(defun mapover-directories (md-base-directory md-paired-return-p md-function
                                              &rest md-args)
  (let* ((md-function-value (apply md-function md-base-directory md-args))
         (md-directories (list (if md-paired-return-p
                                   (list (file-name-as-directory md-base-directory)
                                         md-function-value)
                                   md-function-value))))
    (dolist (md-directory (directory-files md-base-directory t))
      (let ((md-name (file-name-nondirectory md-directory)))
        (when (and (not (string= md-name "."))
                   (not (string= md-name ".."))
                   (file-directory-p md-directory)
                   (or *mapover-directory-softlinks-p*
                       (not (file-symlink-p (directory-file-name md-directory))))
                   ;; file-accessible-directory-p doesn't work for
                   ;; ange-ftp.  If it is writeable, assume we have
                   ;; access (usually, the case)
                   (or (file-accessible-directory-p md-directory)
                       (file-writable-p md-directory)))
          (setq md-directories
                (append md-directories
                        (apply (function mapover-directories)
                               (file-name-as-directory md-directory)
                               md-paired-return-p
                               md-function md-args))))))
    md-directories))

(defun mapover-files-internal (mfi-directory
                               mfi-paired-return-p
                               mfi-file-regexp
                               mfi-function
                               &rest mfi-args)
  (let* ((mfi-files (directory-files mfi-directory t mfi-file-regexp))
         (mfi-return-values nil))
    ;;(message "mfi-args: %s" mfi-args) (sleep-for 1)
    ;;(message "mfi-file-regexp: %s" mfi-file-regexp) (sleep-for 1)
    ;;(message "mfi-directory: %s" mfi-directory) (sleep-for 1)
    ;;(message "mfi-files: %s" mfi-files) (sleep-for 1)
    ;;(message "mfi-function: %s" mfi-function) (sleep-for 1)
    (if mfi-files
        (progn
          (dolist (mfi-file mfi-files)
            (let ((mfi-val (apply mfi-function mfi-file mfi-args)))
              (push 
               (if mfi-paired-return-p
                   (list mfi-file mfi-val)
                   (list mfi-val))
               mfi-return-values)))
          (reverse mfi-return-values))
        (values))))

(defun mapover-files (mf-base-directory mf-file-regexp 
                                        mf-paired-return-p mf-function
                                        &optional mf-include-directory-pairing-p
                                        &rest mf-args)
  (let ((mf-directory-values
         (apply 'mapover-directories mf-base-directory mf-paired-return-p
                'mapover-files-internal mf-paired-return-p mf-file-regexp
                mf-function mf-args)))
    (if mf-include-directory-pairing-p
        (if mf-paired-return-p
            mf-directory-values
            (apply 'append (apply 'append mf-directory-values)))
        (if mf-paired-return-p
            (let ((mf-nondirectory nil))
              (dolist (mf-directory-value mf-directory-values)
                ;;(message "cddr mf-d-v: %s" (cadr mf-directory-value)) (sleep-for 1)
                (setq mf-nondirectory 
                      (append mf-nondirectory (cadr mf-directory-value))))
              mf-nondirectory)
            (apply 'append (apply 'append mf-directory-values))))))

;;;
;;; Output file open and closing encapsulation
;;;

;;;
;;; Provide a context macro for writing to an output file.  This macro
;;; could be a little smarter, in that it would only read in the file
;;; *only* if it  is being appended.
;;;
(defmacro with-output-file (buffer-variable filespec append-p &rest body)
  (` (let (((, buffer-variable) 
            (find-file-noselect 
             (funcall (if (fboundp 'maybe-substitute-in-file-name)
                          'maybe-substitute-in-file-name
                          'substitute-in-file-name)
                      (, filespec)))))
       (unwind-protect
           (progn
             (save-excursion
               (set-buffer (, buffer-variable))
               ;; Disable backups of the output file
               (make-local-variable 'make-backup-files)
               (setq make-backup-files nil)
               ;; Turn off autosaving
               (auto-save-mode nil)
               ;; Either erase the buffer or append to it.
               (if (, append-p)
                   (goto-char (point-max))
                 (progn (erase-buffer))))
             (prog1 
                 (progn 
                   (,@ body))
               (save-excursion
                 (set-buffer (, buffer-variable))
                 (save-buffer))))
         (when (and (, buffer-variable) (bufferp (, buffer-variable)))
           ;; Mark the buffer as not modified to avoid querying the
           ;; user about killing the buffer.
           (save-excursion
             (set-buffer (, buffer-variable))
             (set-buffer-modified-p nil))
           (kill-buffer (, buffer-variable)))))))

(put 'with-output-file 'lisp-indent-function 3)

(defmacro with-output-file-new (buffer-variable filespec &rest body)
  (` (with-output-file (, buffer-variable)
         (, filespec)
         nil
       (,@ body))))

(put 'with-output-file-new 'lisp-indent-function 2)

(defmacro with-output-file-appended (buffer-variable filespec &rest body)
  (` (with-output-file (, buffer-variable)
                       (, filespec)
                       t
                       (,@ body))))

(put 'with-output-file-appended 'lisp-indent-function 2)

(defun generate-temporary-name (&optional errorp)
  (concat (user-login-name)
          "-" 
          (or (and (system-name) 
                   (not (string-equal (system-name) ""))
                   (system-name))
              (if errorp 
                  (error "Cannot guarantee globally unique temporary filename.")
                  "unknown"))
          "-"
          (number-to-string (emacs-pid))
          "-"
          (mapconcat 'identity (current-time) "")
          ".tmp"))

;; Generate a globally unique filename.  This will avoid potential
;; temporary filename collisions with emacsen on other hosts
;; (significant when writing to shared NFS directories).
(defun generate-temporary-filespec (&optional directory errorp)
  (expand-file-name 
   (concat 
    (if directory 
        (if (string-equal directory "") 
            ""
            (file-name-as-directory directory))
        (file-name-as-directory "~"))
    (generate-temporary-name errorp))))

(defmacro with-temporary-file (buffer-variable &rest body)
  (let ((tmp-file (gentemp)))
    (` (let (((, tmp-file) (generate-temporary-filespec)))
         (unwind-protect
             (with-output-file-new
              (, buffer-variable) 
              (, tmp-file)
              (,@ body))
           (when (file-exists-p (, tmp-file))
             (delete-file (, tmp-file))))))))

;;;
;;; Provide a context macro for reading an input file.
;;;
(defmacro with-input-file (buffer-variable filespec &rest body)
  (let ((buffer-symbol (or buffer-variable (gensym)))
        (filespec-symbol (gensym)))
    (` (let* (((, filespec-symbol) 
               (funcall (if (fboundp 'maybe-substitute-in-file-name)
                          'maybe-substitute-in-file-name
                          'substitute-in-file-name)
                      (, filespec))))

         (unless (file-exists-p (, filespec-symbol))
           (error "File %s does not exist." (, filespec-symbol)))
         (unless (file-readable-p (, filespec-symbol))
           (error "File %s is not readable." (, filespec-symbol)))
         (when (file-directory-p (, filespec-symbol))
           (error "%s is a directory instead of a file." (, filespec-symbol)))

         (let (((, buffer-symbol) 
                ;;Create a unique buffer that does not maintain undo information.
                (get-buffer-create (concat " tmp-" (generate-temporary-name)))))
           (unwind-protect
                (save-excursion
                  (set-buffer (, buffer-symbol))
                  (auto-save-mode nil)  ;Disable autosaving
                  (make-local-variable 'make-backup-files) ; Disable backups
                  (setq make-backup-files nil)
                  (multiple-value-bind (absolute-name) ;Length is 2nd value returned
                      (insert-file-contents (, filespec-symbol))
                    (setq buffer-file-name absolute-name))
                  (setq buffer-read-only t) ;Make the buffer read-only
                  (,@ body))
             (when (and (, buffer-symbol) (bufferp (, buffer-symbol)))
               ;; Mark the buffer as not modified to avoid querying the
               ;; user about killing the buffer.
               (save-excursion
                 (set-buffer (, buffer-symbol))
                 (set-buffer-modified-p nil))
               (kill-buffer (, buffer-symbol)))))))))

(put 'with-input-file 'lisp-indent-function 2)

;;;
;;; Define a macro to read a file, manipulate it, and write it back out.
;;;

(defmacro with-update-file (buffer-variable filespec &rest body)
  (let ((buffer-symbol (or buffer-variable (gensym))))
    `(with-input-file ,buffer-symbol ,filespec
      (unwind-protect
           (prog2
               (setq buffer-read-only nil)
               ,@body)
        (when (buffer-modified-p ,buffer-symbol)
          (save-excursion
            (set-buffer ,buffer-symbol)
            (save-buffer)))))))

(put 'with-update-file 'lisp-indent-function 2)

(provide 'elisp-file-basic)


;;; Local Variables:
;;; eval: (put 'with-output-file 'lisp-indent-function 3)
;;; eval: (put 'with-output-file-new 'lisp-indent-function 2)
;;; eval: (put 'with-output-file-appended 'lisp-indent-function 2)
;;; eval: (put 'with-input-file 'lisp-indent-function 2)
;;; eval: (put 'with-update-file 'lisp-indent-function 2)
;;; End:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CUT HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
