;;; helheim-mcp-server.el         -*- lexical-binding: t; no-byte-compile: t -*-

(setup mcp-server
  (:install mcp-server :host github :repo "rhblind/emacs-mcp-server"
            :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  (:setopt mcp-server-security-prompt-for-permissions t)
  (with-eval-after-load 'org
    (:setopt mcp-server-emacs-tools-org-allowed-roots (list org-directory)
             mcp-server-emacs-tools-org-auto-save t))
  (:defer
    (mcp-server-start)))

;;;; Do not ask about killing mcp-server process on exit Emacs

(advice-add 'mcp-server-transport-unix--start :after
            '+mcp-server-transport-unix--do-not-query-on-exit)

(advice-add 'mcp-server-transport-tcp--start :after
            '+mcp-server-transport-tcp--do-not-query-on-exit)

(defun +mcp-server-transport-unix--do-not-query-on-exit (&rest _)
  (-some-> mcp-server-transport-unix--server-process
    (set-process-query-on-exit-flag nil)))

(defun +mcp-server-transport-tcp--do-not-query-on-exit (&rest _)
  (-some-> mcp-server-transport-tcp--server-process
    (set-process-query-on-exit-flag nil)))

;;; .
(provide 'helheim-mcp-server)
;;; helheim-mcp-server.el ends here
