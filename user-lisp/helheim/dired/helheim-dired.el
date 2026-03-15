;;; helheim-dired.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(require 'dash)

(leaf casual :straight t)

;; (leaf async
;;   :straight t
;;   :after dired
;;   :blackout dired-async-mode
;;   (dired-async-mode)) ; Do dired actions asynchronously.

(leaf dired
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . dired-omit-mode)
  :config
  (setopt delete-by-moving-to-trash t
          auto-revert-remote-files nil)
  (setopt dired-kill-when-opening-new-dired-buffer t
          ;; dired-free-space nil
          dired-dwim-target t ;; Propose a target for intelligent moving/copying
          dired-mouse-drag-files t ;; 'move
          dired-deletion-confirmer 'y-or-n-p
          dired-recursive-deletes 'always
          dired-recursive-copies 'always
          dired-vc-rename-file t
          dired-create-destination-dirs 'ask
          dired-do-revert-buffer t
          dired-auto-revert-buffer #'dired-directory-changed-p ; #'dired-buffer-stale-p
          dired-no-confirm t
          dired-clean-confirm-killing-deleted-buffers nil
          dired-maybe-use-globstar t
          dired-omit-verbose t
          dired-omit-files "\\`[.]?#\\|\\`[.].+"
          dired-hide-details-hide-symlink-targets nil
          dired-hide-details-hide-absolute-location t) ;; Emacs 31
  :defer-config
  ;; PERF: `dired-listing-switches' is autoloaded, so setup in `:defer-config'
  ;;   section to avoid loading Dired at Emacs startup.
  ;; ---------------------------------------------------------------------
  ;; -l                   :: use a long listing format
  ;; -a, --all            :: do not ignore entries starting with "."
  ;; -A, --almost-all     :: do not list implied "." and ".."
  ;; -h, --human-readable :: print sizes like 1K 234M 2G
  ;; -F, --classify       :: append indicator (one of /=>@|) to entries
  ;; -v                   :: natural sort of (version) numbers within text
  (setopt dired-listing-switches "-lAhF -v --group-directories-first")
  (put 'dired-jump 'repeat-map nil)
  (load "helheim-dired-lib" nil t)
  (load "helheim-dired-keys" nil t))

(leaf wdired
  :custom
  (wdired-use-dired-vertical-movement . 'sometimes)
  ;; (wdired-allow-to-change-permissions . t) ; 'advanced
  )

(leaf diredfl
  :straight t
  :after dired
  :global-minor-mode diredfl-global-mode)

(leaf nerd-icons-dired
  :straight t
  :blackout t
  :hook
  (dired-mode-hook . nerd-icons-dired-mode)
  :config
  (advice-add 'wdired-change-to-wdired-mode :before (lambda () (nerd-icons-dired-mode -1)))
  (advice-add 'wdired-change-to-dired-mode  :after  (lambda () (nerd-icons-dired-mode +1))))

(leaf dired-narrow  :straight t)
(leaf dired-subtree :straight t)

(leaf dired-copy-paste
  :straight (dired-copy-paste :host github :repo "jsilve24/dired-copy-paste")
  :commands (dired-copy-paste-do-copy
             dired-copy-paste-do-cut
             dired-copy-paste-do-paste))

(leaf fd-dired :straight t)

(leaf dired-filter
  :straight t
  :after dired
  :require t
  :init
  ;; These variables must be set before `dired-filter' is loaded.
  (setopt dired-filter-prefix nil
          dired-filter-verbose nil
          dired-filter-mark-prefix nil)
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
              (extension . "pdf")))))
  (fset 'dired-filter-map dired-filter-map))

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

(leaf image-dired
  :custom
  ;; Use Thumbnail Managing Standard
  ;;
  ;; Thumbnails size:
  ;; - standard           128 pixels
  ;; - standard-large     256 pixels
  ;; - standard-x-large   512 pixels
  ;; - standard-xx-large
  (image-dired-thumbnail-storage . 'standard)
  (image-dired-marking-shows-next . nil)
  :defer-config
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote image-dired-thumbnail-buffer)
                 (display-buffer-reuse-window display-buffer-pop-up-window))))

;;; .
(provide 'helheim-dired)
;;; helheim-dired.el ends here
