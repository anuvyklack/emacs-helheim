;;; helheim-minibuffer.el         -*- lexical-binding: t; no-byte-compile: t -*-
;;; Code:
;;; Keybindings

(setup vertico
  (:global-bind :state (normal emacs)
    "C-c '"   '("vertico last session" . vertico-repeat)
    "C-c \""  '("vertico select session" . vertico-repeat-select))
  (:keymap minibuffer-local-map
    (:bind "M-a" 'marginalia-cycle)))

;;; Config

(setup vertico
  (:install t)
  (:after-init vertico-mode)
  (:hook minibuffer-setup-hook vertico-repeat-save)
  (:setopt vertico-resize 'grow-only ;; Grow and shrink the Vertico minibuffer
           vertico-count 15  ;; How many candidates to show
           vertico-scroll-margin 2
           vertico-cycle nil)
  ;; Prompt indicator for `completing-read-multiple'.
  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args
                (lambda (args)
                  (cons (format "[CRM%s] %s"
                                (string-replace "[ \t]*" "" crm-separator)
                                (car args))
                        (cdr args))))))

(setup vertico-directory
  (:after vertico)
  (:require t)
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (:hook rfn-eshadow-update-overlay-hook vertico-directory-tidy)
  (:keymap vertico-directory-map
    (:bind "C-h" 'vertico-directory-up)))

(setup marginalia
  (:install t)
  (marginalia-mode))

(setup nerd-icons-completion
  (:install t)
  (:after marginalia)
  ;; Icons make no sense when they are all the same and only add distraction.
  (:setopt nerd-icons-completion-category-icons nil
           nerd-icons-completion-icon-size 0.95)
  (:hook marginalia-mode-hook nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode))

(provide 'helheim-minibuffer)
;;; helheim-minibuffer.el ends here
