;;; helheim-embark.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Keybindings

(require 'hel-core)

(hel-keymap-global-set
  ;; "C-<return>" 'embark-act
  "C-<m>" 'embark-act
  "M-m"   'embark-dwim)

;; On QWERTY layout c, v, b keys are next to each other.
(hel-keymap-set minibuffer-local-map
  "C-c C-c" 'embark-export
  "C-c C-v" 'embark-collect
  "C-c C-b" 'embark-become)

;;; Config

(use-package embark-consult :straight t :defer t)

(use-package embark
  :straight t
  :defer t
  :custom
  (which-key-use-C-h-commands nil)
  (prefix-help-command 'embark-prefix-help-command)
  :config
  (require 'embark-consult)
  ;; Hide the modeline of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; .
(provide 'helheim-embark)
;;; helheim-embark.el ends here
