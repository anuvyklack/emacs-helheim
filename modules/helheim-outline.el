;;; helheim-outline.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'helix-macros)
(require 'helix-common)
(require 'helix-core)

;;; Keybindings

(with-eval-after-load 'outline
  (dolist (keymap (list outline-mode-map outline-minor-mode-map))
    (helix-keymap-set keymap :state 'normal
      ;; "m h"   'helheim-outline-mark-subtree ; "h" is for heading
      ;; "m i h" 'helheim-outline-mark-subtree
      "m o"   'helheim-outline-mark-subtree ; "o" is for outline
      "m i o" 'helheim-outline-mark-subtree)
    (helix-keymap-set keymap :state '(normal motion)
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
      "z M-k" 'outline-move-subtree-up))

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
          "M-k" 'outline-move-subtree-up)))

;;; Config

(use-package outli
  :ensure (outli :host github :repo "jdtsmith/outli")
  :blackout (outline-mode outline-minor-mode)
  :hook (emacs-lisp-mode-hook . outli-mode))

(helix-define-advice outline-up-heading (:before (&rest _) push-mark)
  (helix-push-point))

;; (setq outline-level #'helheim-lisp-outline-level)
;;
;; (defun helheim-lisp-outline-level ()
;;   "Return outline level for comment at point.
;; Replacement for `lisp-outline-level'."
;;   (if (match-beginning 1)
;;       (- (match-end 1)
;;          (match-beginning 1))
;;     0))

;;; Commands

(defun helheim-outline-up-heading (count &optional invisible-ok)
  "Move up in the outline hierarchy to the parent heading."
  (interactive "p")
  (helix-delete-all-fake-cursors)
  (deactivate-mark)
  (helix-push-point)
  (if (outline-on-heading-p invisible-ok)
      (outline-up-heading count invisible-ok)
    (outline-back-to-heading invisible-ok)
    (outline-up-heading (1- count) invisible-ok)))

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
  (helix-push-point)
  (if (outline-on-heading-p)
      ;; we are already looking at a heading
      (forward-line 0)
    ;; else go back to previous heading
    (outline-previous-visible-heading 1))
  (helix-set-region (point)
                    (progn (outline-end-of-subtree)
                           (unless (eobp) (forward-char))
                           (point))
                    -1 :adjust)
  (helix-reveal-point-when-on-top))

(provide 'helheim-outline)
;;; helheim-outline.el ends here
