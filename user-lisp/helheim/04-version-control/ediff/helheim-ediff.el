;;; helheim-ediff.el              -*- lexical-binding: t; no-byte-compile: t -*-

(setup ediff
  (:hook ediff-keymap-setup-hook helheim-ediff-setup-keys)
  (setopt ediff-diff-options "-w" ;; turn off whitespace checking
          ediff-split-window-function #'split-window-horizontally
          ediff-window-setup-function #'ediff-setup-windows-plain
          ;; ediff-keep-variants nil
          )
  (hel-set-initial-state 'ediff-mode 'emacs))

;;; Restore windows configuration after quitting ediff

(let (wconf) ; Private variable shared by two functions.
  ;;
  (defun helheim-ediff--save-window-configuration ()
    (setq wconf (current-window-configuration)))
  ;;
  (defun helheim-ediff--restore-window-configuration ()
    (when (window-configuration-p wconf)
      (set-window-configuration wconf))))

(add-hook 'ediff-before-setup-hook #'helheim-ediff--save-window-configuration)
(add-hook 'ediff-quit-hook    #'helheim-ediff--restore-window-configuration 90)
(add-hook 'ediff-suspend-hook #'helheim-ediff--restore-window-configuration 90)

;;; .
(provide 'helheim-ediff)
;;; helheim-ediff.el ends here
