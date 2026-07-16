;;; helheim-flycheck.el -*- lexical-binding: t; no-byte-compile: t -*-

(setup flycheck
  (:install t)
  (:command flycheck-list-errors
            flycheck-buffer)
  (:setopt flycheck-emacs-lisp-load-path 'inherit
           flycheck-idle-change-delay 1.0
           flycheck-buffer-switch-check-intermediate-buffers t
           flycheck-display-errors-delay 0.25 ;; default 0.9
           flycheck-highlighting-mode 'symbols
           flycheck-keymap-prefix (kbd "C-c !"))
  ;;
  ;; Pass flycheck diagnostics through Eldoc.
  (setq flycheck-display-errors-function nil)
  (:hook flycheck-mode-hook +flycheck-setup-eldoc-h)
  ;;
  ;; Reveal and pulse the errors region on jump to it.
  (hel-advice-add 'flycheck-next-error :around 'hel-jump-command-a)
  (advice-add 'flycheck-next-error :after '+flycheck-reveal-error-at-point)
  (add-hook 'flycheck-error-list-after-jump-hook '+flycheck-reveal-error-at-point)
  (add-hook 'flycheck-error-list-after-jump-hook 'recenter)
  ;;
  ;; Display flycheck buffer in the bottom side-window.
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Flycheck errors*" string-end)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  ;;
  (:after-init global-flycheck-mode)
  (:after-load
    (load "helheim-flycheck-lib" nil t)
    ;; Rerunning checks on every newline is a mote excessive.
    (delq 'new-line flycheck-check-syntax-automatically)
    ;;
    (:keymap flycheck-mode-map
      (:bind :state normal
        "] d"  '("Next diagnostic" . flycheck-next-error)
        "[ d"  '("Prev diagnostic" . flycheck-previous-error))
      (:bind
        "C-c d d"   '("diagnostic buffer" . flycheck-list-errors)
        "C-c d SPC" 'flycheck-compile
        "C-c d c"   '("start checking" . flycheck-buffer)
        "C-c d C"   '("forget all errors" . flycheck-clear)
        "C-c d x"   '("disable checker" . flycheck-disable-checker)
        "C-c d ?"   '("describe checker" . flycheck-describe-checker)
        "C-c d s"   '("select checker" . flycheck-select-checker)
        "C-c d y"   '("copy errors" . flycheck-copy-errors-as-kill)
        "C-c d e"   '("explain error at point" . flycheck-explain-error-at-point)
        "C-c d h"   '("display error at point" . flycheck-display-error-at-point)
        "C-c d H"   'display-local-help
        "C-c d i"   'flycheck-manual
        "C-c d v"   'flycheck-verify-setup
        "C-c d V"   'flycheck-version))
    (:keymap flycheck-error-list-mode-map
      (:bind
        "j"   'flycheck-error-list-next-error
        "k"   'flycheck-error-list-previous-error
        "RET" 'flycheck-error-list-goto-error
        "g"   'flycheck-error-list-check-source
        "e"   'flycheck-error-list-explain-error
        "f"   'flycheck-error-list-set-filter
        "F"   'flycheck-error-list-reset-filter))))

;;; .
(provide 'helheim-flycheck)
;;; helheim-flycheck.el ends here
