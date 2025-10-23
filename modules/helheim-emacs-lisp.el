;;; helheim-emacs-lisp.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'helix-paredit)

;;; Keybindings

(helix-keymap-set emacs-lisp-mode-map :state 'normal
  ;; Vim uses `K' but it is occupied in Helix. `M' is near `K' and free.
  "M"       'helpful-at-point
  "g d"     '("Find definition" . +elisp-find-definitions)
  "C-w g d" '("Find definition other window" . +elisp-find-definitions-other-window))

;; <leader> e
(helix-keymap-set emacs-lisp-mode-map
  "C-c e" (cons "eval elisp"
                (define-keymap
                  "e" 'pp-eval-last-sexp
                  ;; "e" 'elisp-eval-region-or-buffer ; "C-c C-e"
                  "r" 'eval-region
                  "b" 'eval-buffer
                  "f" 'eval-defun
                  ;; "m" 'macrostep-expand
                  "m" 'emacs-lisp-macroexpand
                  "p" 'pp-macroexpand-last-sexp)))

(helix-keymap-set lisp-data-mode-map :state 'normal
  "M" 'helpful-at-point)

;;; Config

;; ;; Treat `-' char as part of the word on `w', `e', `b', motions.
;; (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
;; (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)

(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local tab-width 8)))
(add-hook 'emacs-lisp-mode-hook #'helix-paredit-mode)
(add-hook 'lisp-data-mode-hook  #'helix-paredit-mode)

(use-package elisp-demos
  :ensure t
  :after helpful
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update      :after #'elisp-demos-advice-helpful-update))

;; Extra highlighting
(use-package highlight-defined
  :ensure t
  ;; :custom (highlight-defined-face-use-itself . t)
  :hook
  (emacs-lisp-mode-hook . highlight-defined-mode)
  (help-mode-hook . highlight-defined-mode))

;; `elisp-refs-function'
;; `elisp-refs-macro'
;; `elisp-refs-symbol'
;; `elisp-refs-special'
;; `elisp-refs-variable'
(use-package elisp-refs
  :ensure t
  :config
  (helix-keymap-set elisp-refs-mode-map
    "C-j" 'elisp-refs-next-match
    "C-k" 'elisp-refs-prev-match
    "n"   'elisp-refs-next-match
    "N"   'elisp-refs-prev-match)

  (dolist (cmd '(elisp-refs-visit-match
                 elisp-refs-next-match
                 elisp-refs-prev-match))
    (helix-advice-add cmd :around #'helix-jump-command-a)))

;;;; Xref tweaks

(elpaca elisp-def)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (remove-hook 'xref-backend-functions #'elisp--xref-backend :local)))

(defun +elisp-find-definitions ()
  "Try `elisp-def', on fail try other xref backends."
  (interactive)
  (deactivate-mark)
  (or (ignore-errors (call-interactively #'elisp-def))
      (call-interactively #'xref-find-definitions)))

(defun +elisp-find-definitions-other-window ()
  (interactive)
  (other-window-prefix)
  (+elisp-find-definitions))

(dolist (cmd '(+elisp-find-definitions
               +elisp-find-definitions-other-window))
  (helix-advice-add cmd :around #'helix-jump-command-a))

(provide 'helheim-emacs-lisp)
;;; helheim-emacs-lisp.el ends here
