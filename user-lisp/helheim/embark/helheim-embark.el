;;; helheim-embark.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Keybindings

(require 'hel-core)

(hel-keymap-global-set
  "C-<m>" 'embark-act
  "M-m"   'embark-dwim)

;; On QWERTY layout c, v, b keys are next to each other.
(hel-keymap-set minibuffer-local-map
  "C-c C-c" 'embark-export
  "C-c C-v" 'embark-collect
  "C-c C-b" 'embark-become)

;;; Config

(leaf embark
  :straight t
  :init
  (setopt which-key-use-C-h-commands nil
          prefix-help-command 'embark-prefix-help-command)
  :config
  ;; Hide the modeline of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :defer-config
  (load "helheim-embark-keys" nil t))

(leaf embark-consult
  :straight t
  :after embark
  :require t)

;;; .
(provide 'helheim-embark)
;;; helheim-embark.el ends here
