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
  (:global-bind
    "C-c a RET" 'agent-shell
    "C-c a n"   '("new agent-shell" . agent-shell-new-shell)
    "C-c a w"   '("new worktree agent-shell" . agent-shell-new-worktree-shell))
  (:after-load
    (load "helheim-agent-shell-lib" nil t)
    (:global-bind
      "C-c a s" 'agent-shell-send-dwim)
    (:with-keymap agent-shell-mode-map
      (:bind :state 'normal
        "z '" 'agent-shell-prompt-compose)
      (:bind
        "C-c RET" 'dired-jump))))

(setup hel-agent-shell
  (:install hel-agent-shell :host github :repo "anuvyklack/hel-agent-shell")
  (:after agent-shell)
  (:require t))

;;; .
(provide 'helheim-agent-shell)
;;; helheim-agent-shell.el ends here
