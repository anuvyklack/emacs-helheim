;;; helheim-agent-shell.el -*- lexical-binding: t; no-byte-compile: t -*-

(setup agent-shell
  (:install t)
  (:setopt agent-shell-show-welcome-message t
           agent-shell-header-style 'graphical
           agent-shell-session-strategy 'prompt
           agent-shell-show-context-usage-indicator 'detailed
           ;; agent-shell-prefer-viewport-interaction t
           agent-shell-write-inhibit-minor-modes '(aggressive-indent-mode)
           agent-shell-transcript-file-path-function 'agent-shell--default-transcript-file-path)
  (hel-keymap-set (helheim-leader-prefix-map "a")
    "RET" 'agent-shell
    "n"   '("new agent-shell" . agent-shell-new-shell)
    "w"   '("new worktree agent-shell" . agent-shell-new-worktree-shell))
  (:after-load
    (load "helheim-agent-shell-lib" nil t)
    (hel-keymap-set (helheim-leader-prefix-map "a")
      "s" 'agent-shell-send-dwim)
    (hel-keymap-set (helheim-leader-map agent-shell-mode-map)
      "RET" 'dired-jump)
    (hel-keymap-set agent-shell-mode-map :state 'normal
      "z '" 'agent-shell-prompt-compose)))

(setup hel-agent-shell
  (:install hel-agent-shell :host github :repo "anuvyklack/hel-agent-shell")
  (:after agent-shell)
  (:require t))

;;; .
(provide 'helheim-agent-shell)
;;; helheim-agent-shell.el ends here
