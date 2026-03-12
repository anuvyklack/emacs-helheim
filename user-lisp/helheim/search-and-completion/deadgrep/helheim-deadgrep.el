;;; helheim-deadgrep.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Config

(require 'hel-macros)
(require 'hel-core)

;; <leader> ss — deadgrep entry point
(hel-keymap-set search-map
  "s" 'deadgrep)

(leaf deadgrep
  :straight t
  ;; :hook
  ;; (deadgrep-mode-hook . next-error-follow-minor-mode)
  :defer-config
  (load "helheim-deadgrep-keys"))

(add-hook 'deadgrep-mode-hook
          (defun helheim-deadgrep-mode-h ()
            ;; TODO: upstream this
            (setq-local revert-buffer-function (lambda (&rest _)
                                                 (deadgrep-restart)))))

;; TODO: upstream this
(dolist (fun '(deadgrep deadgrep-search-term))
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

;;; .
(provide 'helheim-deadgrep)
;;; helheim-deadgrep.el ends here
