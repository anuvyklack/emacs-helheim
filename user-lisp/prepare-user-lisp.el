;;; -*- lexical-binding: t -*-
;;; Commentary:
;;
;; All recursive contents in "user-lisp" directory will be byte-compiled,
;; scraped for autoload cookies and ensured to be in 'load-path'. You can
;; also invoke the 'prepare-user-lisp' command manually at any time.
;;
;; Backported from 31.1
;;
;; https://cgit.git.savannah.gnu.org/cgit/emacs.git/commit/?id=1b931fbe42bdad1507768872dfe034caa236adab
;;
;;; Code:

(defcustom user-lisp-directory
  (locate-user-emacs-file "user-lisp/")
  "Activate all Lisp files in this directory, if it exists.
All regular files below directories are byte-compiled, scraped for
autoload cookies and ensured to be in `load-path' at startup.  To
restrict what subdirectories to process, see
`user-lisp-ignored-directories'.  Note that byte-compilation and
autoload scraping is lazy, occurring only if the file timestamps
indicate that it is necessary.  For details on how to override this
behavior, consult `prepare-user-lisp'.

If you need Emacs to pick up on updates to this directory that occur
after startup, you can also invoke the `prepare-user-lisp' manually.  To
disable auto-scraping, see `user-lisp-auto-scrape'.

Note that this variable must be set in your early-init file, as the
variable's value is used before loading the regular init file.
Therefore, if you customize it via Customize, you should save your
customized setting into your `early-init-file'."
  :initialize #'custom-initialize-delay
  :type 'directory
  :version "31.1")

(defcustom user-lisp-ignored-directories
  '(".git" ".hg" "RCS" "CVS" ".svn" "_svn" ".bzr")
  "List of directory names for `prepare-user-lisp' to not descend into.
Each entry of the list is a string that denotes the file name without a
directory component.  If during recursion any single entry matches the
file name of any directory, `prepare-user-lisp' will ignore the contents
of the directory.  This option is most useful to exclude administrative
directories that do not contain Lisp files."
  :type '(choice (repeat (string :tag "Directory name")))
  :version "31.1")

(declare-function byte-recompile-file "bytecomp" (filename &optional force arg load))

;;;###autoload
(defun prepare-user-lisp (&optional just-activate autoload-file force)
  "Byte-compile, scrape autoloads and prepare files in `user-lisp-directory'.
Write the autoload file to AUTOLOAD-FILE. If JUST-ACTIVATE is non-nil,
then the more expensive operations (byte-compilation and autoload
scraping) are skipped, in effect only processing any previous autoloads.
If AUTOLOAD-FILE is nil, store the autoload data in a file next to DIR.
If FORCE is non-nil, or if invoked interactively with a prefix argument,
re-create the entire autoload file and byte-compile everything
unconditionally."
  (interactive (list nil nil current-prefix-arg))
  (unless just-activate (require 'bytecomp))
  (unless (file-directory-p user-lisp-directory)
    (error "No such directory: %S" user-lisp-directory))
  (unless autoload-file
    (setq autoload-file (expand-file-name ".user-lisp-autoloads.el"
                                          user-lisp-directory)))
  (let* ((ignored (concat "\\`" (regexp-opt user-lisp-ignored-directories) "\\'"))
         (pred (lambda (dir)
                 (not (string-match-p ignored (file-name-nondirectory dir)))))
         (dir (expand-file-name user-lisp-directory))
         (backup-inhibited t)
         (dirs (list dir)))
    (add-to-list 'load-path (directory-file-name dir))
    (dolist (file (directory-files-recursively dir "" t pred))
      (cond ((and (file-regular-p file)
                  (string-suffix-p ".el" file))
             (unless just-activate
               (with-demoted-errors "Error while compiling: %S"
                 (byte-recompile-file file force 0)
                 (when (native-comp-available-p)
                   (native-compile-async file)))))
            ((and (file-directory-p file)
                  (not (string-match-p ignored (file-name-nondirectory file))))
             (add-to-list 'load-path (directory-file-name file))
             (push file dirs))))
    (unless just-activate
      (loaddefs-generate dirs autoload-file nil nil nil force))
    (when (file-exists-p autoload-file)
      (load autoload-file nil t))))
