;;; helheim-minibuffer.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Code:
(require 'hel-core)

;;; Keybindings

(setup vertico
  (:global-bind :state '(normal motion)
    "C-c '"   '("vertico repeat" . vertico-repeat)
    "C-c \""  '("select vertico session" . vertico-repeat-select))
  (:with-keymap minibuffer-local-map
    (:bind
      "M-a" 'marginalia-cycle))
  (:after-load
    (:with-keymap vertico-map
      (:bind :state 'normal
        "y"     'vertico-save ;; Copy current candidate to kill ring.
        "j"     'vertico-next
        "k"     'vertico-previous
        "g g"   'vertico-first
        "G"     'vertico-last)
      (:bind
        "C-j"   'vertico-next
        "C-k"   'vertico-previous
        "C-S-j" 'vertico-next-group
        "C-S-k" 'vertico-previous-group

        "C-l"   'vertico-insert
        "C-h"   'vertico-directory-up

        ;; Scrolling in Insert state.
        "C-f"   'vertico-scroll-up
        "C-b"   'vertico-scroll-down

        ;; Rebind "}" / "{" and "]p" / "[p" keys
        [remap hel-forward-paragraph]      'vertico-next-group
        [remap hel-backward-paragraph]     'vertico-previous-group
        [remap hel-forward-paragraph-end]  'vertico-next-group
        [remap hel-backward-paragraph-end] 'vertico-previous-group

        ;; Rebind "C-f" / "C-b" and "C-d" / "C-u" scrolling keys
        [remap hel-smooth-scroll-down]      'vertico-scroll-up
        [remap hel-smooth-scroll-up]        'vertico-scroll-down
        [remap hel-smooth-scroll-page-down] 'vertico-scroll-up
        [remap hel-smooth-scroll-page-up]   'vertico-scroll-down))))

;;; Config

(setup vertico
  (:install t)
  (:after-init vertico-mode)
  (:hook minibuffer-setup-hook vertico-repeat-save)
  (setopt vertico-resize 'grow-only ;; Grow and shrink the Vertico minibuffer
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
  (:with-keymap vertico-directory-map
    (:bind "C-h" 'vertico-directory-up)))

(setup marginalia
  (:install t)
  (marginalia-mode))

(setup nerd-icons-completion
  (:install t)
  (:after marginalia)
  ;; Icons make no sense when they are all the same and only add distraction.
  (setopt nerd-icons-completion-category-icons nil
          nerd-icons-completion-icon-size 0.95)
  (:hook marginalia-mode-hook nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode))

(provide 'helheim-minibuffer)
;;; helheim-minibuffer.el ends here
