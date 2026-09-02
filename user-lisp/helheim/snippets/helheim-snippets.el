;;; helheim-snippets.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; Keybindings

(setup tempel
  (:global-bind
    ;; The "/" trigger reaches templates from the corfu popup.  These keys
    ;; reach them without a slash, and from anywhere in a word.
    "M-+"     'tempel-complete
    ;; The leader "i" prefix.  `helheim-org' gave the key up because its own
    ;; insert map is still reachable at the org local leader, ", i".
    "C-c i i" '("template" . tempel-insert)
    "C-c i c" '("complete template" . tempel-complete)))

;;; Config

(setup tempel
  (:install t)
  ;; The "templates" file by default is under `user-emacs-directory', which
  ;; Helheim points at var/ -- generated files only.
  (:setopt tempel-path (expand-file-name "templates" helheim-root-directory))
  (:hook (prog-mode-hook
          text-mode-hook
          conf-mode-hook
          lsp-completion-mode-hook)
         (defun helheim-tempel-setup-capf ()
           "Put both template Capfs at the head of the buffer-local Capf list."
           ;; Type "/" to see the list of snippets.
           (setq-local corfu-auto-trigger "/")
           (add-hook 'completion-at-point-functions #'helheim-tempel-capf -100 t)
           ;; Also add snippets to the general completion list.
           (add-hook 'completion-at-point-functions #'tempel-complete -90 t)))
  ;; Modes outside those families still reach the template Capf through
  ;; the global value, behind whatever Capf they install for themselves.
  (:hook completion-at-point-functions helheim-tempel-capf))

;;; .
(provide 'helheim-snippets)
;;; helheim-snippets.el ends here
