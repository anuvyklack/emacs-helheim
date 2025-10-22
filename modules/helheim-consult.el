;;; helheim-consult.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'helix-core)

(use-package consult
  :ensure t
  :defer t
  :custom
  (consult-narrow-key "<")
  (consult-fd-args '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
                     "--color=never"
                     ;; https://github.com/sharkdp/fd/issues/839
                     "--full-path --absolute-path"
                     "--hidden --exclude .git"))
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Aggressive asynchronous that yield instantaneous results. (suitable for
  ;; high-performance systems.) Note: Minad, the author of Consult, does not
  ;; recommend aggressive values.
  ;; Read: https://github.com/minad/consult/discussions/951
  ;;
  ;; However, the author of minimal-emacs.d uses these parameters to achieve
  ;; immediate feedback from Consult.
  ;; (setq consult-async-input-debounce 0.02
  ;;       consult-async-input-throttle 0.05
  ;;       consult-async-refresh-delay 0.02)
  (setq consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-async-refresh-delay  0.15)

  ;; Enable automatic preview at point in the *Completions* buffer.
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  ;; Configure the register formatting and preview. This improves the register.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add 'register-preview :override #'consult-register-window)

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.2 any))

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.5 any)))

;;; Keybindings

(helix-keymap-global-set :state '(normal motion)
  "C-/" 'consult-line       ; `/' is bound to search
  "C-?" 'consult-line-multi) ; `C-S-/'

(helix-keymap-global-set :state 'normal
  "g i" 'consult-imenu
  "g I" 'consult-imenu-multi
  "g o" 'consult-outline
  "g m" 'consult-mark
  "g M" 'consult-global-mark
  "g e" 'consult-compile-error
  "g <return>" 'consult-goto-line)

(helix-keymap-set minibuffer-local-map
  "C-r" 'consult-history) ; like in shell

(helix-keymap-set search-map
  "f" 'consult-find
  "l" 'consult-locate
  "g" 'consult-grep
  "v" 'consult-git-grep
  "r" 'consult-ripgrep
  "/" 'consult-ripgrep
  "k" 'consult-keep-lines
  "u" 'consult-focus-lines)

(helix-keymap-set project-prefix-map
  "b" 'consult-project-buffer)

;; C-x bindings are in `ctl-x-map'
(helix-keymap-global-set
  "C-x b"   'consult-buffer
  "C-x 4 b" 'consult-buffer-other-window
  "C-x 5 b" 'consult-buffer-other-frame
  "C-x t b" 'consult-buffer-other-tab
  "C-x r b" 'consult-bookmark)

(helix-keymap-global-set
  "<remap> <repeat-complex-command>"        'consult-complex-command
  "<remap> <recentf-open>"                  'consult-recent-file
  "<remap> <recentf-open-files>"            'consult-recent-file
  "<remap> <bookmark-jump>"                 'consult-bookmark
  "<remap> <goto-line>"                     'consult-goto-line
  "<remap> <imenu>"                         'consult-imenu
  "<remap> <Info-search>"                   'consult-info
  "<remap> <load-theme>"                    'consult-theme
  "<remap> <switch-to-buffer>"              'consult-buffer
  "<remap> <switch-to-buffer-other-window>" 'consult-buffer-other-window
  "<remap> <switch-to-buffer-other-frame>"  'consult-buffer-other-frame
  "<remap> <yank-pop>"                      'consult-yank-pop
  "<remap> <locate>"                        'consult-locate)

(provide 'helheim-consult)
;;; helheim-consult.el ends here
