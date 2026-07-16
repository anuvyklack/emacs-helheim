;;; helheim-ibuffer.el            -*- lexical-binding: t; no-byte-compile: t -*-
;;; Customization

(defcustom helheim-ibuffer-not-project-buffer-filters
  `((mode . (help-mode helpful-mode debugger-mode backtrace-mode))
    (name . ,(eval-when-compile
               (rx string-start
                   (or "*Ibuffer*" "*Messages*" "*Warnings*" "*scratch*")
                   string-end))))
  "TODO"
  :type 'list
  :group 'helheim)

(defcustom helheim-ibuffer-format-project-group-name
  (lambda (directory)
    (format "%s : %s" ;; "%s  %s"
            (file-name-nondirectory (directory-file-name directory))
            directory))
  "TODO"
  :type 'function
  :group 'helheim)

;;; Config

(setup ibuffer
  (setopt ibuffer-truncate-lines t
          ibuffer-show-empty-filter-groups nil ; Don't show emtpy filter groups
          ;; ibuffer-display-summary nil
          ;; ibuffer-movement-cycle nil
          ibuffer-old-time 2 ;; hours
          ibuffer-directory-abbrev-alist `((,abbreviated-home-dir . "~/"))
          ibuffer-default-sorting-mode 'filename/process
          ;; ibuffer-read-only-char ?%
          ;; ibuffer-modified-char  ?*
          ;; ibuffer-marked-char    ?>
          ;; ibuffer-locked-char    ?L
          ;; ibuffer-deletion-char  ?D
          )
  (:hook ibuffer-mode-hook ibuffer-auto-mode) ; Automatically update Ibuffer.
  ;; Project filter groups
  (progn
    (setq ibuffer-maybe-show-predicates '(helheim-ibuffer-maybe-show-p))
    (advice-add 'ibuffer-generate-filter-groups
                :before #'helheim-ibuffer-update-project-filter-groups)
    ;; <leader> b b
    (define-advice ibuffer-jump (:after (&optional _other-window) helheim)
      (ibuffer-switch-to-saved-filter-groups "Project"))
    ;; <leader> p C-b
    (setq project-buffers-viewer 'helheim-ibuffer-project-buffers))
  (:after-load
    (load "helheim-ibuffer-lib" nil t)
    (load "helheim-ibuffer-keys" nil t)
    ;;
    (require 'mule-util)
    (setopt ibuffer-eliding-string (truncate-string-ellipsis))
    ;;
    (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode)))

;; Ibuffer layout
(setopt ibuffer-formats
        `(( mark modified read-only locked
            " " (icon 2 2 :left :elide)
            ,(propertize " " 'display `(space :align-to 8))
            (helheim-name 26 26 :left :elide)
            "  " ;; 2 spaces
            ;; (mode 18 18 :left :elide)
            ;; " "
            project-relative-filename-or-process)
          ( mark modified read-only locked
            " " (icon 2 2 :left :elide)
            ,(propertize " " 'display `(space :align-to 8))
            helheim-name)))

;;; .
(provide 'helheim-ibuffer)
;;; helheim-ibuffer.el ends here
