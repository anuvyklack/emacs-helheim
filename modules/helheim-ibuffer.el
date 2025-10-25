;;; helheim-ibuffer.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(elpaca ibuffer-projectile)

;; (setq ibuffer-formats
;;       '(( mark modified read-only locked
;;           " " (name 55 55 :left :elide)
;;           " " (size 8 -1 :right)
;;           " " (mode 18 18 :left :elide) " " filename-and-process)
;;         ( mark " " (name 16 -1) " " filename)))

(use-package ibuffer-project
  :ensure t
  :after ibuffer
  :custom
  (ibuffer-project-use-cache t)
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups
                    (ibuffer-project-generate-filter-groups))
              (unless (eq ibuffer-sorting-mode 'project-file-relative)
                (ibuffer-do-sort-by-project-file-relative))))
  (setq ibuffer-formats
        '(( mark modified read-only locked
            " " (name 18 18 :left :elide)
            " " (mode 16 16 :left :elide)
            " " project-file-relative))))

;;; Custom columns

;; Icons column
(define-ibuffer-column icon
  ( :name "  ")
  (let ((icon (if (and (buffer-file-name) (nerd-icons-auto-mode-match?))
                  (nerd-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                (nerd-icons-icon-for-mode major-mode :v-adjust -0.05))))
    (if (symbolp icon)
        (setq icon (nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-dsilver :height 0.8 :v-adjust 0.0))
      icon)))

;; Human readable size column
(define-ibuffer-column size
  ( :name "Size"
    :inline t
    :header-mouse-map ibuffer-size-header-map)
  (file-size-human-readable (buffer-size)))

;;; ibuffer-vc

(leaf ibuffer-vc
  ;; :elpaca t
  :elpaca (ibuffer-vc :repo "~/.config/emacs/modules")
  :hook
  (ibuffer-hook . (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic))))
  :config
  ;; Render filenames relative to project root
  (define-ibuffer-column my-vc-root-relative-filename-or-process
    ( :name "Filename/Process"
      :header-mouse-map ibuffer-filename/process-header-map
      :summarizer (lambda (strings)
                    (setq strings (delete "" strings))
                    (let ((procs (--count (get-text-property 1 'ibuffer-process it)
                                          strings))
                          (files (length strings)))
                      (concat (pcase files
                                (0 "No files")
                                (1 "1 file")
                                (_ (format "%d files" files)))
                              ", "
                              (pcase files
                                (0 "no processes")
                                (1 "1 process")
                                (_ (format "%d processes" procs)))))))
    (let ((filename (ibuffer-make-column-filename buffer mark))
          proc root-dir)
      (cond ((setq proc (get-buffer-process buffer))
             (concat (propertize (format "(%s %s)" proc (process-status proc))
                                 'font-lock-face 'italic
                                 'ibuffer-process proc)
                     (if (length> filename 0)
                         (format " %s" filename)
                       "")))
            ((setq root-dir (cdr (ibuffer-vc-root buffer)))
             (file-relative-name filename root-dir))
            (t (abbreviate-file-name filename))))))

;;; ibuffer-project

(require 'ibuffer)
(require 'ibuf-ext)

;; (require 'project)
(declare-function project-root "project" (project))

(defun helheim-ibuffer-group-by-root (buffer)
  (with-current-buffer buffer
    (if-let ((project (project-current)))
        (project-root project)
      default-directory)))

(defun helheim-ibuffer-group-sort-alphabetically (groups _level)
  (sort groups :in-place t :key #'car))

(define-ibuffer-filter project
    "Limit current view to buffers related to project matching QUALIFIER."
  ( :description "project"
    :reader (read-regexp "Filter by project: "))
  (-when-let ((root . type) (ibuffer-project-root buf))
    (if (stringp qualifier)
        (string-match-p qualifier root)
      (equal qualifier type))))

(defun +open-project- ())

(provide 'helheim-ibuffer)
;;; helheim-ibuffer.el ends here
