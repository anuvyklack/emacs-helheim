;;; helheim-embark.el             -*- lexical-binding: t; no-byte-compile: t -*-
;;; Keybindings

(require 'hel)

(hel-keymap-global-set
  "C-<m>" 'embark-act
  "M-m"   'embark-dwim)

;; On QWERTY layout c, v, b keys are next to each other.
(hel-keymap-set minibuffer-local-map
  "C-c C-c" 'embark-export
  "C-c C-v" 'embark-collect
  "C-c C-b" 'embark-become)

;;; Config

(setup embark
  (:install t)
  ;; (:setopt which-key-use-C-h-commands nil)
  ;; (setq prefix-help-command 'embark-prefix-help-command)
  ;; Display Emabark menus with Which-key.
  (:setopt embark-indicators '(+embark-which-key-indicator
                               embark-highlight-indicator
                               embark-isearch-highlight-indicator))
  ;; Hide the modeline of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (:after-load
    (load "helheim-embark-lib" nil t)))

(setup embark-consult
  (:install t)
  (:after embark)
  (:require t))

;;; .
(provide 'helheim-embark)
;;; helheim-embark.el ends here
