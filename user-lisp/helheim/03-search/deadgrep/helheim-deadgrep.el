;;; helheim-deadgrep.el           -*- lexical-binding: t; no-byte-compile: t -*-
;;; Config

;; (require 'hel)

(setup deadgrep
  (:install t)
  ;; <leader> ss — deadgrep entry point
  (:with-keymap search-map
    (:bind "s" 'deadgrep))
  ;; (:hook deadgrep-mode-hook next-error-follow-minor-mode)
  )

(add-hook 'deadgrep-mode-hook
          (defun helheim-deadgrep-mode-h ()
            ;; TODO: upstream this
            (setq-local revert-buffer-function (lambda (&rest _)
                                                 (deadgrep-restart)))))

;; TODO: upstream this
(dolist (fun '(deadgrep
               deadgrep-search-term))
  (advice-add fun :after 'helheim-deadgrep-set-list-buffers-directory-a))

(defun helheim-deadgrep-set-list-buffers-directory-a (&rest _)
  "Set `list-buffers-directory' for search query so it displays nicely in Ibuffer."
  (setq-local list-buffers-directory
              (format "query: %s" deadgrep--search-term)))

(hel-advice-add 'deadgrep-mode :before #'hel-deactivate-mark-a)
(hel-advice-add 'deadgrep-mode :before #'hel-delete-all-fake-cursors)

(dolist (cmd '(deadgrep-visit-result
               deadgrep-visit-result-other-window))
  (hel-advice-add cmd :around #'hel-jump-command-a))

;;; Keybindings

;; Keybindings in Deadgrep buffer
(setup deadgrep
  (:after-load
    (load "helheim-deadgrep-lib" nil t)
    (:with-keymap deadgrep-mode-map
      (:unbind "g")
      (:bind
        "i"   'deadgrep-edit-mode

        "a"   'deadgrep-incremental ; "a" for amend
        "g r" 'deadgrep-restart     ; also "C-w r"

        "RET" 'deadgrep-visit-result-other-window

        "o"   '+deadgrep-show-result-other-window
        "C-o" '+deadgrep-show-result-other-window

        "n"   'deadgrep-forward-match
        "N"   'deadgrep-backward-match

        "C-j" '+deadgrep-forward-match-show-other-window
        "C-k" '+deadgrep-backward-match-show-other-window

        "}"   'deadgrep-forward-filename
        "{"   'deadgrep-backward-filename
        "] p" 'deadgrep-forward-filename
        "[ p" 'deadgrep-backward-filename

        "z j" 'deadgrep-forward-filename
        "z k" 'deadgrep-backward-filename
        "z u" 'deadgrep-parent-directory))
    (:with-keymap deadgrep-edit-mode-map
      (:bind :state 'normal
        "<escape>" 'deadgrep-mode
        "z x" 'deadgrep-mode
        "Z Z" 'deadgrep-mode
        "RET" 'deadgrep-visit-result-other-window

        ;; Commands bound to these keys have no sense for Deadgrep.
        "o"   'undefined
        "O"   'undefined
        "J"   'undefined))))

;;; .
(provide 'helheim-deadgrep)
;;; helheim-deadgrep.el ends here
