;;; helheim-straight.el           -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;
;; Install and setup Straight.el package manager.
;;
;;; Code:

(setopt straight-check-for-modifications '(check-on-save find-when-checking)
        ;; Prepend name with space to hide straight buffer in Ibuffer.
        straight-process-buffer " *straight-process*")

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        (or (bound-and-true-p straight-base-dir)
                                            user-emacs-directory)))
      (bootstrap-version 7)
      (url "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil t))

(straight-use-package 'dash)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'blackout)

(defalias 'elpaca-wait #'ignore)

(defalias 'helheim-update     #'straight-pull-package)
(defalias 'helheim-update-all #'straight-pull-all)

;;; .
(provide 'helheim-straight)
;;; helheim-straight.el ends here
