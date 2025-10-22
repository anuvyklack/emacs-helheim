;;; helheim-xref.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'helheim-utils)

(use-package xref
  :defer t
  :custom
  (xref-search-program 'ripgrep) ; or 'ugrep
  (xref-auto-jump-to-first-definition 'show)
  (xref-prompt-for-identifier nil)
  (xref-history-storage #'xref-window-local-history)
  ;; Enable completion in the minibuffer instead of the definitions buffer.
  ;; You can use `embark-export' to export minibuffer content to xref buffer.
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  ;; (xref-show-definitions-function #'xref-show-definitions-buffer-at-bottom)
  :config
  (advice-add 'xref-find-definitions :around '+xref-try-all-backends-a)
  (advice-add 'xref-find-references  :around '+xref-try-all-backends-a))

(use-package dumb-jump
  :ensure t
  :after xref
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package nerd-icons-xref
  :ensure t
  :after xref
  :config (nerd-icons-xref-mode))

(defun +xref-try-all-backends-a (orig-fun &rest args)
  "Try all `xref-backend-functions' in row until first succeed."
  (let* ((jumped nil)
         (xref-after-jump-hook (cons (lambda () (setq jumped t))
                                     xref-after-jump-hook)))
    (cl-dolist (backend (+hook-values 'xref-backend-functions))
      (ignore-error user-error
        (let ((xref-backend-functions (list backend)))
          (apply orig-fun args)))
      (when jumped (cl-return)))))

(provide 'helheim-xref)
;;; helheim-xref.el ends here
