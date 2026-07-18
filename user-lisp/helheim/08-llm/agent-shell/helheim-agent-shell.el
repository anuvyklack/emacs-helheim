;;; helheim-agent-shell.el -*- lexical-binding: t; no-byte-compile: t -*-

(setup agent-shell
  (:install t)
  (:global-bind
    "C-c a RET" 'agent-shell
    "C-c a n"   '("new agent-shell" . agent-shell-new-shell)
    "C-c a w"   '("new worktree agent-shell" . agent-shell-new-worktree-shell))
  (:after-load
    (:global-bind
      "C-c a s" 'agent-shell-send-dwim)
    (:keymap agent-shell-mode-map
      (:bind :state normal
        ", ," 'agent-shell-prompt-compose
        "z '" 'agent-shell-prompt-compose)
      (:bind
        "C-c RET" 'dired-jump))))

;;;; Display buffer logic

;; Open the initial `agent-shell' buffer in another window. All subsequent
;; buffers are opened in the same window.
(setq agent-shell-display-action nil)
(add-to-list 'display-buffer-alist
             '((or (major-mode . agent-shell-mode)
                   (major-mode . agent-shell-viewport-view-mode)
                   (major-mode . agent-shell-viewport-edit-mode))
               (display-buffer-reuse-mode-window
                display-buffer-pop-up-window)
               (mode . (agent-shell-mode
                        agent-shell-viewport-view-mode
                        agent-shell-viewport-edit-mode))))

;;; .
(provide 'helheim-agent-shell)
;;; helheim-agent-shell.el ends here
