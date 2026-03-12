;;; helheim-dired.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(require 'dash)

(use-package casual :straight t :defer t)

;; (use-package async
;;   :straight t
;;   :after dired
;;   :blackout dired-async-mode
;;   (dired-async-mode)) ; Do dired actions asynchronously.

(use-package dired
  :defer t
  :custom
  ;; -l                   :: use a long listing format
  ;; -a, --all            :: do not ignore entries starting with "."
  ;; -A, --almost-all     :: do not list implied "." and ".."
  ;; -h, --human-readable :: print sizes like 1K 234M 2G
  ;; -F, --classify       :: append indicator (one of /=>@|) to entries
  ;; -v                   :: natural sort of (version) numbers within text
  (dired-listing-switches "-lAhF -v --group-directories-first")
  ;; (dired-free-space nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t) ;; Propose a target for intelligent moving/copying
  (dired-mouse-drag-files t) ;; 'move
  (delete-by-moving-to-trash t)
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-vc-rename-file t)
  (dired-create-destination-dirs 'ask)
  (dired-do-revert-buffer t)
  (auto-revert-remote-files nil)
  (dired-auto-revert-buffer #'dired-directory-changed-p) ; #'dired-buffer-stale-p
  (dired-no-confirm t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-maybe-use-globstar t)
  (dired-omit-verbose t)
  (dired-omit-files "\\`[.]?#\\|\\`[.].+")
  (dired-hide-details-hide-symlink-targets nil)
  ;; (dired-hide-details-hide-absolute-location t) ;; from Emacs 31
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (put 'dired-jump 'repeat-map nil)
  (load "helheim-dired-lib")
  (load "helheim-dired-keys"))

(use-package wdired
  :defer t
  :custom
  (wdired-use-dired-vertical-movement 'sometimes)
  ;; (wdired-allow-to-change-permissions t) ; 'advanced
  )

(use-package diredfl
  :straight t
  :after dired
  :config (diredfl-global-mode))

(use-package nerd-icons-dired
  :straight t
  :blackout t
  :hook
  (dired-mode-hook . nerd-icons-dired-mode)
  :config
  (advice-add 'wdired-change-to-wdired-mode :before (lambda () (nerd-icons-dired-mode -1)))
  (advice-add 'wdired-change-to-dired-mode  :after  (lambda () (nerd-icons-dired-mode +1))))

(use-package dired-narrow :straight t :defer t)
(use-package dired-subtree :straight t :defer t)

(use-package dired-copy-paste
  :straight (dired-copy-paste :host github :repo "jsilve24/dired-copy-paste")
  :commands (dired-copy-paste-do-copy
             dired-copy-paste-do-cut
             dired-copy-paste-do-paste))

(use-package fd-dired :straight t :defer t)

(use-package dired-filter
  :straight t
  :after dired
  :custom
  (dired-filter-verbose nil)
  (dired-filter-prefix nil)
  (dired-filter-mark-prefix nil)
  :config
  (setq dired-filter-group-saved-groups
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

;; `ls-lisp' package
(setq ls-lisp-verbosity nil
      ls-lisp-dirs-first t)

;;;; Convert local minor-modes to global ones

(defmacro helheim-dired-convert-to-global-minor-mode (mode)
  (declare (debug t))
  `(define-advice ,mode (:after (&rest _) helheim)
     (if ,mode
         (add-hook 'dired-mode-hook #',mode)
       (remove-hook 'dired-mode-hook #',mode))))

(helheim-dired-convert-to-global-minor-mode dired-hide-details-mode)
(helheim-dired-convert-to-global-minor-mode dired-omit-mode)
(helheim-dired-convert-to-global-minor-mode dired-filter-group-mode)

;;;; image-dired

(use-package image-dired
  :defer t
  :custom
  ;; Use Thumbnail Managing Standard
  ;;
  ;; Thumbnails size:
  ;; - standard           128 pixels
  ;; - standard-large     256 pixels
  ;; - standard-x-large   512 pixels
  ;; - standard-xx-large
  (image-dired-thumbnail-storage 'standard)
  (image-dired-marking-shows-next nil)
  :config
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote image-dired-thumbnail-buffer)
                 (display-buffer-reuse-window display-buffer-pop-up-window))))

;;; .
(provide 'helheim-dired)
;;; helheim-dired.el ends here
