;;; helheim-outline-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(require 'hel-macros)
(require 'hel-common)
(require 'hel-core)

;;; Keybindings

(setup outline
  (:after-load
    (:with-keymap (outline-mode-map
                   outline-minor-mode-map)
      (:bind :state 'normal
        "m h"   'helheim-outline-mark-subtree ; "h" is for heading
        "m i h" 'helheim-outline-mark-subtree
        "m o"   'helheim-outline-mark-subtree ; "o" is for outline
        "m i o" 'helheim-outline-mark-subtree)
      (:bind :state '(normal motion)
        "z <tab>"     'outline-cycle
        "z <backtab>" 'outline-cycle-buffer
        "z <return>"  'outline-insert-heading
        "z j"   'outline-next-visible-heading
        "z k"   'outline-previous-visible-heading
        "z C-j" 'outline-forward-same-level
        "z C-k" 'outline-backward-same-level
        "z u"   'helheim-outline-up-heading
        "z o"   'helheim-outline-open
        "z c"   'outline-hide-subtree
        "z r"   'outline-show-all
        "z m"   'outline-hide-sublevels
        "z 2"   'helheim-outline-show-2-sublevels
        "z p"   'helheim-outline-hide-other ; "p" for path
        "z O"   'outline-show-branches
        "z <"   'outline-promote
        "z >"   'outline-demote
        "z M-h" 'outline-promote
        "z M-l" 'outline-demote
        "z M-j" 'outline-move-subtree-down
        "z M-k" 'outline-move-subtree-up
        "z / s" 'outline-show-by-heading-regexp
        "z / h" 'outline-hide-by-heading-regexp))

    (:with-keymap outline-mode-prefix-map
      (:unbind "/"))

    (setq outline-navigation-repeat-map
          (define-keymap
            "u"   'outline-up-heading
            "j"   'outline-next-visible-heading
            "k"   'outline-previous-visible-heading
            "C-j" 'outline-forward-same-level
            "C-k" 'outline-backward-same-level))
    (setq outline-editing-repeat-map
          (define-keymap
            "<"   'outline-promote
            ">"   'outline-demote
            "M-h" 'outline-promote
            "M-l" 'outline-demote
            "M-j" 'outline-move-subtree-down
            "M-k" 'outline-move-subtree-up))))

;;; Config

(setup outli
  (:install outli :host github :repo "jdtsmith/outli")
  (:blackout outline-mode
             outline-minor-mode)
  (:hook emacs-lisp-mode-hook outli-mode)
  (with-eval-after-load 'consult-imenu
    (cl-pushnew '(?h "Headings" font-lock-comment-face)
                (-> consult-imenu-config
                    (map-elt 'emacs-lisp-mode)
                    (map-elt :types))
                :test #'equal)))

(hel-define-advice outline-up-heading (:before (&rest _) push-mark)
  (hel-push-point))

;;; Commands

(defun helheim-outline-up-heading (count &optional invisible-ok)
  "Move up in the outline hierarchy to the parent heading."
  (interactive "p")
  (hel-delete-all-fake-cursors)
  (deactivate-mark)
  (hel-push-point)
  (if (outline-on-heading-p invisible-ok)
      (outline-up-heading count invisible-ok)
    (outline-back-to-heading invisible-ok)
    (cl-decf count)
    (unless (zerop count)
      (outline-up-heading count invisible-ok))))

(put 'helheim-outline-up-heading 'repeat-map 'outline-navigation-repeat-map)

(defun helheim-outline-open ()
  (interactive)
  (outline-show-entry)
  (outline-show-children))

(defun helheim-outline-hide-other ()
  (interactive)
  (outline-hide-other)
  (outline-show-branches))

(defun helheim-outline-show-2-sublevels ()
  "Remain 2 top levels of headings visible."
  (interactive)
  (outline-hide-sublevels 2))

(defun helheim-outline-mark-subtree ()
  "Mark the current subtree in an outlined document."
  (interactive)
  (hel-push-point)
  (if (outline-on-heading-p)
      ;; we are already looking at a heading
      (forward-line 0)
    ;; else go back to previous heading
    (outline-previous-visible-heading 1))
  (hel-set-region (point)
                  (progn (outline-end-of-subtree)
                         (unless (eobp) (forward-char))
                         (point))
                  -1)
  (hel-reveal-point-when-on-top))

(provide 'helheim-outline-mode)
;;; helheim-outline-mode.el ends here
