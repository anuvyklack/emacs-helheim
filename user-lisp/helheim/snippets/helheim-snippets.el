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
  ;; The tempel default is "templates" under `user-emacs-directory', which
  ;; Helheim points at var/ -- generated files only, not tracked. Keep the
  ;; template file in the repo root next to init.el.
  (:setopt tempel-path (expand-file-name "templates" helheim-root-directory))
  ;; Some major modes like `emacs-lisp-mode' and `lsp-completion-mode' add
  ;; exclusive Capfs to `completion-at-point-functions' buffer-locally.
  ;; An exclusive Capf that returns bounds ends the chain, even when it finds no
  ;; candidates, so anything behind it never runs. Depth -100 sorts the template
  ;; Capf in front. Ordinary completion is untouched: without a leading slash
  ;; `helheim-tempel-capf' returns nil straight away.
  (:hook (prog-mode-hook
          text-mode-hook
          conf-mode-hook
          lsp-completion-mode-hook)
         (defun helheim-tempel-setup-capf ()
           "Put `helheim-tempel-capf' at the head of the buffer-local Capf list."
           ;; Use "/" as the template trigger. The relaxed minimum applies
           ;; to every Capf, not only the template one, so a "/" that follows
           ;; a non-word character also offers `cape-file' root completion.
           (setq-local corfu-auto-trigger "/")
           (add-hook 'completion-at-point-functions #'helheim-tempel-capf
                     -100 t)))
  ;; Modes outside those families still reach the template Capf through
  ;; the global value, behind whatever Capf they install for themselves.
  (:hook completion-at-point-functions helheim-tempel-capf))

;;; .
(provide 'helheim-snippets)
;;; helheim-snippets.el ends here
