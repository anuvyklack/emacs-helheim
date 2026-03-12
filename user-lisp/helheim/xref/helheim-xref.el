;;; helheim-xref.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(leaf xref
  ;; :straight t
  :custom
  (xref-search-program . 'ripgrep) ; or 'ugrep
  (xref-auto-jump-to-first-definition . 'show)
  (xref-prompt-for-identifier . nil)
  (xref-history-storage . #'xref-window-local-history)
  ;; ;; Enable completion in the minibuffer instead of the definitions buffer.
  ;; ;; You can use `embark-export' to export minibuffer content to xref buffer.
  ;; (xref-show-xrefs-function . #'xref-show-definitions-completing-read)
  ;; (xref-show-definitions-function . #'xref-show-definitions-completing-read)
  ;; ;; (xref-show-definitions-function . #'xref-show-definitions-buffer-at-bottom)
  :config
  ;; Open xref buffer in another window
  (add-to-list 'display-buffer-alist
               '((major-mode . xref--xref-buffer-mode)
                 (display-buffer-use-some-window display-buffer-pop-up-window)
                 (inhibit-same-window . t)
                 (body-function . select-window))))

(leaf dumb-jump
  :straight t
  :after xref
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (remove-hook 'xref-backend-functions #'etags--xref-backend))

(leaf nerd-icons-xref
  :straight t
  :after xref
  :global-minor-mode nerd-icons-xref-mode)

;;; Make Xref try all backends untill first one succeed

(hel-keymap-global-set
  "<remap> <xref-find-references>"  #'helheim-xref-find-references
  "<remap> <xref-find-definitions>" #'helheim-xref-find-definitions
  "<remap> <xref-find-definitions-other-window>" #'helheim-xref-find-definitions-other-window
  "<remap> <xref-find-definitions-other-frame>" #'helheim-xref-find-definitions-other-frame)

(provide 'helheim-xref)
;;; helheim-xref.el ends here
