;;; helheim-emacs-lisp.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Code:

(require 'hel)

;;; Keybindings

(setup elisp-mode
  (:with-keymap (emacs-lisp-mode-map
                 lisp-interaction-mode-map)
    (:bind
      "C-c e"   '("eval" . helheim-elisp-eval-map)) ;; <leader>
    (:bind :state 'normal
      ", e"     '("eval" . helheim-elisp-eval-map) ;; <localleader>
      "K"       'helpful-at-point ;; after Vim
      "M"       'helpful-at-point
      "g d"     '("Find definition" . helheim-elisp-find-definitions)
      "C-w g d" '("Find definition other window" . helheim-elisp-find-definitions-other-window))))

(defvar-keymap helheim-elisp-eval-map
  :prefix 'helheim-elisp-eval-map
  "e"   'pp-eval-last-sexp
  "f"   'eval-defun
  "r"   'elisp-eval-region-or-buffer ; "C-c C-e"
  "b"   'elisp-eval-region-or-buffer
  "B"   'elisp-byte-compile-buffer
  ;; "m"   'macrostep-expand
  "m"   'emacs-lisp-macroexpand
  "p"   'pp-macroexpand-last-sexp
  "RET" 'eval-print-last-sexp)

(setup lisp-mode
  (:with-keymap lisp-data-mode-map
    (:bind :state 'normal
      "K"   'helpful-at-point
      "M"   'helpful-at-point)))

(hel-keymap-global-set "<remap> <eval-expression>" 'pp-eval-expression)

;;; Config

(setup hel-paredit
  (:install paredit)
  (:require t)
  (:hook (emacs-lisp-mode-hook
          lisp-data-mode-hook) hel-paredit-mode))

;; ;; Treat `-' char as part of the word on `w', `e', `b', motions.
;; (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
;; (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)

(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local tab-width 8)))

(add-to-list 'display-buffer-alist
             `(,(rx string-start
                    (or "*Pp Eval Output*" "*Pp Macroexpand Output*")
                    string-end)
               (display-buffer-use-some-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (body-function . select-window)))

(setup elisp-demos
  (:install t)
  (:after helpful)
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update      :after #'elisp-demos-advice-helpful-update))

;; Extra highlighting
(setup highlight-defined
  (:install t)
  ;; (setopt highlight-defined-face-use-itself t)
  (:hook (emacs-lisp-mode-hook help-mode-hook)))

;; `elisp-refs-function'
;; `elisp-refs-macro'
;; `elisp-refs-symbol'
;; `elisp-refs-special'
;; `elisp-refs-variable'
(setup elisp-refs
  (:install t)
  (:after-load
    (:with-keymap elisp-refs-mode-map
      (:bind
        "C-j" 'elisp-refs-next-match
        "C-k" 'elisp-refs-prev-match
        "n"   'elisp-refs-next-match
        "N"   'elisp-refs-prev-match)))
  ;;
  (dolist (cmd '(elisp-refs-visit-match
                 elisp-refs-next-match
                 elisp-refs-prev-match))
    (hel-advice-add cmd :around #'hel-jump-command-a)))

(setopt edebug-print-length 10
        edebug-print-level 3)

;;;; Go to definition

(setup elisp-def (:install t))

(defun helheim-elisp-find-definitions ()
  "Try `elisp-def', on fail try other xref backends."
  (interactive)
  (deactivate-mark)
  (or (ignore-errors (call-interactively 'elisp-def))
      (call-interactively 'helheim-xref-find-definitions))
  (setq disable-point-adjustment t))

(defun helheim-elisp-find-definitions-other-window ()
  (interactive)
  (other-window-prefix)
  (helheim-elisp-find-definitions))

(dolist (cmd '(helheim-elisp-find-definitions
               helheim-elisp-find-definitions-other-window))
  (hel-advice-add cmd :around #'hel-jump-command-a))

;;; .
(provide 'helheim-emacs-lisp)
;;; helheim-emacs-lisp.el ends here
