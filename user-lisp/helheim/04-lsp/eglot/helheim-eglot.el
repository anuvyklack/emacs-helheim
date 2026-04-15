;;; helheim-eglot.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; TODO:
;; - yasnippet
;; - eglot-booster
;; - eglot-momentary-inlay-hints
;;
;;; Keybindings

(setup eglot
  (:with-keymap (helheim-leader-map)
    (:bind "l" (cons "LSP" (hel-keymap-set (helheim-leader-prefix-map "l")
                             "RET" 'eglot))))
  (:after-load
    (:with-keymap eglot-mode-map
      (:bind :state 'normal
        "<f2>" 'eglot-rename
        "K"    'eldoc-box-help-at-point
        "M"    'eldoc-box-help-at-point
        "g d"  '("Definition" . xref-find-definitions)
        "g r"  '("References" . xref-find-references)
        "g D"  '("Declaration" . eglot-find-declaration)
        "g t"  '("Type definition" . eglot-find-typeDefinition)
        "g i"  '("Implementations" . eglot-find-implementation)))
    (:with-keymap (helheim-leader-map eglot-mode-map)
      (:bind
        "t" (cons "toggle"
                  (define-keymap
                    "h" 'eglot-inlay-hints-mode
                    "s" 'eglot-semantic-tokens-mode))
        "l" (cons "LSP"
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
                    "c"   '("Call hierarchy" . eglot-show-call-hierarchy)))))))

(setup flymake
  (:after-load
    (:with-keymap flymake-mode-map
      (:bind :state 'normal
        "] d"      '("Next diagnostic" . flymake-goto-next-error)
        "[ d"      '("Prev diagnostic" . flymake-goto-prev-error)
        "C-c l d"  '("Diagnostic" . flymake-show-buffer-diagnostics)
        "C-c l D"  '("Project diagnostic" . flymake-show-project-diagnostics)))
    ;; Flymake buffer
    (:with-keymap flymake-diagnostics-buffer-mode-map
      ;; (:unbind "C-m")
      (:bind
        "RET"      'flymake-goto-diagnostic
        "S-RET"    'flymake-show-diagnostic
        "M-RET"    'flymake-show-diagnostic
        "o"        'flymake-show-diagnostic
        "g o"      'flymake-show-diagnostic))))

;;; Config


(setup flymake
  (:built-in)
  (:hook prog-mode-hook flymake-mode)
  (setopt flymake-mode-line-lighter nil))

;; (setup flymake-popon
;;   (:install t)
;;   (:hook flymake-mode-hook flymake-popon-mode))

(setup jsonrpc (:built-in))

(setup eglot
  (:built-in)
  (:require helheim-markdown) ;; For on hover documentation formatting.
  (:hook eglot-managed-mode-hook hel-update-active-keymaps)
  (setopt eglot-sync-connect 1
          eglot-autoshutdown t
          eglot-confirm-server-edits '((t . maybe-diff))
          ;; Margin indicator may increase line height due to glyph display
          ;; failures or emoji font height differences.
          eglot-code-action-indications '(eldoc-hint)
          eglot-code-action-indicator "" ;; 💡   󰌵 󱠂 󱠃
          eglot-extend-to-xref t)
  (:after-load
    ;; PERF: Disable the eglot-events-buffer, so Emacs doesn't churn GC and
    ;;   CPU cycles on pretty-printing the events buffer in the background
    ;;   (once it reaches max size).
    (unless init-file-debug
      (cl-callf plist-put eglot-events-buffer-config :size 0))))

(setup eldoc-box
  (:install t)
  (setopt eldoc-box-self-insert-command-list '(self-insert-command)))

(setup consult-eglot
  (:install t)
  (:after eglot)
  (:with-keymap eglot-mode-map
    (:bind "<remap> <xref-find-apropos>" 'consult-eglot-symbols)))

(setup sideline
  (:install t)
  (:blackout t)
  (:hook flymake-mode-hook sideline-mode)
  (setopt sideline-format-right "  %s"
          sideline-backends-right-skip-current-line nil
          sideline-display-backend-name t))

(setup sideline-flymake
  (:install t)
  (:after sideline)
  (setopt sideline-flymake-display-mode 'line) ;; 'line or 'point
  (:hook sideline-backends-right sideline-flymake)
  ;; (add-to-list 'sideline-backends-right 'sideline-flymake)
  )

(setup sideline-eglot
  (:install t)
  (:after sideline)
  (setopt sideline-eglot-code-actions-prefix "󰌵 ") ;; 💡   󰌵 󱠂 󱠃
  (:hook sideline-backends-right sideline-eglot)
  ;; (add-to-list 'sideline-backends-right 'sideline-eglot)
  )

(setup breadcrumb
  (:install t)
  (:hook (c-mode-hook
          c++-mode-hook
          c-ts-mode-hook
          c++-ts-mode-hook) breadcrumb-local-mode))

;;; .
(provide 'helheim-eglot)
;;; helheim-eglot.el ends here
