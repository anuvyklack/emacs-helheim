;;; helheim-eglot.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; TODO:
;; - yasnippet
;; - eglot-booster
;; - eglot-momentary-inlay-hints
;;
;;; Keybindings

(hel-keymap-set mode-specific-map
  "l" (cons "LSP" (hel-keymap-set (or (keymap-lookup mode-specific-map "l")
                                      (make-sparse-keymap))
                    "RET" 'eglot)))

(with-eval-after-load 'eglot
  (hel-keymap-set eglot-mode-map :state 'normal
    "K"         'eldoc-box-help-at-point
    "M"         'eldoc-box-help-at-point
    "g d"       '("Definition" . xref-find-definitions)
    "g r"       '("References" . xref-find-references)
    "g D"       '("Declaration" . eglot-find-declaration)
    "g t"       '("Type definition" . eglot-find-typeDefinition)
    "g i"       '("Implementations" . eglot-find-implementation)
    "<f2>"      'eglot-rename
    ;;
    "C-c t h"   'eglot-inlay-hints-mode
    "C-c t s"   'eglot-semantic-tokens-mode
    ;;
    "C-c l" (cons "LSP"
                  (define-keymap
                    "RET" '("LSP reconnect" . eglot-reconnect)
                    "Q"   '("LSP shutdown" . eglot-shutdown)
                    "r"   '("Rename" . eglot-rename)
                    "f"   '("Format" . eglot-format)
                    "="   '("Format" . eglot-format)
                    "a"   '("Code actions" . eglot-code-actions)
                    "o"   '("Organize imports" . eglot-code-action-organize-imports)
                    "q"   '("Quickfix" . eglot-code-action-quickfix)
                    "e"   '("Refactor Extract" . eglot-code-action-extract)
                    "i"   '("Rewrite Inline" . eglot-code-action-inline)
                    "R"   '("Refactor Rewrite" eglot-code-action-rewrite)
                    "t"   '("Type hierarchy" . eglot-show-type-hierarchy)
                    "c"   '("Call hierarchy" . eglot-show-call-hierarchy)))))

(with-eval-after-load 'flymake
  (hel-keymap-set flymake-mode-map :state 'normal
    "] d"      '("Next diagnostic" . flymake-goto-next-error)
    "[ d"      '("Prev diagnostic" . flymake-goto-prev-error)
    "C-c l d"  '("Diagnostic" . flymake-show-buffer-diagnostics)
    "C-c l D"  '("Project diagnostic" . flymake-show-project-diagnostics))
  ;; Flymake buffer
  (hel-keymap-set flymake-diagnostics-buffer-mode-map
    :unset "C-m"
    "RET"      'flymake-goto-diagnostic
    "S-RET"    'flymake-show-diagnostic
    "M-RET"    'flymake-show-diagnostic
    "o"        'flymake-show-diagnostic
    "g o"      'flymake-show-diagnostic))

;;; Config

;; For on hover documentation formatting.
(require 'helheim-markdown)

(use-package flymake
  :ensure t
  :hook (prog-mode-hook . flymake-mode)
  :custom
  (flymake-mode-line-lighter nil))

(elpaca jsonrpc)

(use-package eglot
  :ensure t
  :defer t
  :hook (eglot-managed-mode-hook . hel-update-active-keymaps)
  :custom
  (eglot-sync-connect 1)
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits '((t . maybe-diff)))
  ;; Margin indicator may increase line height due to glyph display
  ;; failures or emoji font height differences.
  (eglot-code-action-indications '(eldoc-hint))
  (eglot-code-action-indicator "") ;; 💡   󰌵 󱠂 󱠃
  (eglot-extend-to-xref t)
  :config
  ;; PERF: Disable the eglot-events-buffer, so Emacs doesn't churn GC and
  ;;   CPU cycles on pretty-printing the events buffer in the background
  ;;   (once it reaches max size).
  (unless init-file-debug
    (cl-callf plist-put eglot-events-buffer-config :size 0)))

(use-package eldoc-box
  :ensure t
  :defer t
  :custom
  (eldoc-box-self-insert-command-list '(self-insert-command)))

(use-package consult-eglot
  :ensure t
  :after eglot
  :config
  (hel-keymap-set eglot-mode-map
    "<remap> <xref-find-apropos>" 'consult-eglot-symbols))

(use-package sideline
  :ensure t
  :blackout t
  :hook (flymake-mode-hook . sideline-mode)
  :custom
  (sideline-format-right "  %s")
  (sideline-backends-right-skip-current-line nil)
  (sideline-display-backend-name t))

(use-package sideline-flymake
  :ensure t
  :after sideline
  :custom
  (sideline-flymake-display-mode 'line) ;; 'line or 'point
  :config
  (add-to-list 'sideline-backends-right 'sideline-flymake))

(use-package sideline-eglot
  :ensure t
  :after sideline
  :custom
  (sideline-eglot-code-actions-prefix "󰌵 ") ;; 💡   󰌵 󱠂 󱠃
  :config
  (add-to-list 'sideline-backends-right 'sideline-eglot))

(use-package breadcrumb
  :ensure t
  :hook ((c-mode-hook
          c++-mode-hook
          c-ts-mode-hook
          c++-ts-mode-hook) . breadcrumb-local-mode))

;;; .
(provide 'helheim-eglot)
;;; helheim-eglot.el ends here
