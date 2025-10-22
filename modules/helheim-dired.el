;;; helheim-dired.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -l                   :: use a long listing format
;; -a, --all            :: do not ignore entries starting with "."
;; -A, --almost-all     :: do not list implied "." and ".."
;; -h, --human-readable :: print sizes like 1K 234M 2G
;; -F, --classify       :: append indicator (one of /=>@|) to entries
;; -v                   :: natural sort of (version) numbers within text
(setq dired-listing-switches "-lAhF -v --group-directories-first")
;; (setq dired-free-space nil)

(setq dired-kill-when-opening-new-dired-buffer t
      dired-dwim-target t  ; Propose a target for intelligent moving/copying
      delete-by-moving-to-trash t
      dired-deletion-confirmer 'y-or-n-p
      dired-recursive-deletes 'top ;; 'always
      dired-recursive-copies 'always
      dired-vc-rename-file t
      dired-create-destination-dirs 'ask
      auto-revert-remote-files nil
      dired-auto-revert-buffer #'dired-buffer-stale-p
      dired-no-confirm t
      dired-clean-confirm-killing-deleted-buffers nil
      dired-maybe-use-globstar t)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; dired-omit-mode
(setq dired-omit-verbose nil
      dired-omit-files (concat "\\`[.]\\'"))

(use-package dired-filter
  :ensure t
  :custom
  (dired-filter-verbose nil))

(use-package ls-lisp
  :custom
  (ls-lisp-verbosity nil)
  (ls-lisp-dirs-first t))

;;; dired filter

(use-package dired-filter
  :ensure t
  :after dired
  :config
  (setopt dired-filter-group-saved-groups
          '(("default"
             ("Directories"
              (directory))
             ("Archives"
              (extension "zip" "rar" "gz" "bz2" "tar"))
             ("Pictures"
              (or (extension "jfif" "JPG")
                  (mode . 'image-mode)))
             ("Videos"
              (extension "mp4" "mkv" "flv" "mpg" "avi" "webm"))
             ;; ("LaTeX"
             ;;  (extension "tex" "bib"))
             ;; ("Org"
             ;;  (extension . "org"))
             ("PDF"
              (extension . "pdf"))))))

;;; Commands

(defalias '+dired-copy-file-name #'dired-copy-filename-as-kill)

(defun +dired-copy-file-path ()
  "Copy full path to the file into kill ring."
  (interactive)
  (dired-copy-filename-as-kill 0))

(defun +dired-do-flagged-delete-permanently ()
  "Delete files permanently instead of trashing them"
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (let ((delete-by-moving-to-trash nil))
    (dired-do-flagged-delete)))

(defalias '+dired-delete-permanently #'+dired-do-flagged-delete-permanently)

(defun +dired-global-omit-mode (&optional arg)
  "Toggle `dired-omit-mode' globally."
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg)
                       'toggle)))
  (if (cond ((eq arg 'toggle) (not dired-omit-mode))
            ((and (numberp arg) (< arg 1)) nil)
            (t t))
      (progn
        (dired-omit-mode +1)
        (add-hook 'dired-mode-hook #'dired-omit-mode))
    (dired-omit-mode -1)
    (remove-hook 'dired-mode-hook #'dired-omit-mode)))

(defun +dired-filter-global-group-mode (&optional arg)
  "Toggle `dired-filter-group-mode' in all buffers."
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg)
                       'toggle)))
  (if (cond ((eq arg 'toggle) (not dired-filter-group-mode))
            ((and (numberp arg) (< arg 1)) nil)
            (t t))
      (progn
        (dired-filter-group-mode +1)
        (add-hook 'dired-mode-hook #'dired-filter-group-mode))
    (dired-filter-group-mode -1)
    (remove-hook 'dired-mode-hook #'dired-filter-group-mode)))

(provide 'helheim-dired)
;;; helheim-dired.el ends here
