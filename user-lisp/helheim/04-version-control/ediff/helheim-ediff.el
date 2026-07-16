;;; helheim-ediff.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; Code:
;;;; Restore windows configuration after quitting ediff

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
