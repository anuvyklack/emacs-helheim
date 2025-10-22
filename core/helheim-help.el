;;; helheim-help.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'helix-core)

;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)

(use-package help
  :hook
  (help-mode-hook . helheim-disable-hl-line-mode)
  :custom
  (help-enable-autoload nil)
  (help-enable-completion-autoload nil)
  (help-enable-symbol-autoload nil)
  (help-window-select t) ; Focus new help windows when opened
  :config
  (helix-keymap-set help-map
    "h"   nil   ; unbind `view-hello-file'
    "C-c" nil)) ; unbind `describe-copying'

(use-package helpful
  :ensure t
  :defer t
  :hook
  ;; (helpful-mode-hook . outline-minor-mode)
  (helpful-mode-hook . helheim-disable-hl-line-mode)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol))
  :config
  ;; Open links to functions, variables and symbols in helpful buffer in the
  ;; same window.
  (add-to-list 'display-buffer-alist
               '((derived-mode . helpful-mode)
                 (display-buffer-reuse-mode-window display-buffer-pop-up-window)
                 (mode . helpful-mode)
                 (body-function . select-window))))

;;; Keybindings

;; Vim uses `K' but it is occupied in Helix. `M' is near `K' and it is free.
(dolist (keymap (list emacs-lisp-mode-map lisp-data-mode-map))
  (helix-keymap-set keymap :state 'normal
    "M" 'helpful-at-point))

(helix-keymap-set help-map
  "F" 'describe-face
  "M" 'describe-keymap
  "s" 'helpful-symbol
  ;; Rebind `b' key from `describe-bindings' to prefix with more binding
  ;; related commands.
  "b" (cons "bindings"
            (define-keymap
              "b" 'describe-bindings
              "B" 'embark-bindings ;; alternative for `describe-bindings'
              "i" 'which-key-show-minor-mode-keymap
              "m" 'which-key-show-major-mode
              "t" 'which-key-show-top-level
              "f" 'which-key-show-full-keymap
              "k" 'which-key-show-keymap)))

(provide 'helheim-help)
;;; helheim-help.el ends here
