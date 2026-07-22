;;; helheim-modeline              -*- lexical-binding: t; no-byte-compile: t -*-

(setup doom-modeline
  (:install t)
  ;; Varables should be set before package is loaded.
  (:setopt doom-modeline-buffer-file-name-style 'relative-from-project ;; 'auto
           doom-modeline-major-mode-icon nil
           doom-modeline-position-column-line-format '("%c:%l")
           doom-modeline-total-line-number nil
           ;; Right segment
           doom-modeline-project-name nil
           doom-modeline-minor-modes t
           ;; Only show file encoding if it's non-UTF-8 and different line
           ;; endings than the current OSes preference
           doom-modeline-buffer-encoding 'nondefault
           ;; Error | Warning | Note
           doom-modeline-check-icon t
           doom-modeline-check 'auto ;; 'simple
           ;; Word count and in which major modes to display it.
           doom-modeline-enable-word-count t
           doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode)
           ;; Display the GitHub notifications. Requires `ghub' package.
           doom-modeline-github nil
           ;; Gnus notifications.
           doom-modeline-gnus t
           doom-modeline-gnus-timer 15
           doom-modeline-gnus-excluded-groups nil) ; '("dummy.group")
  (:require t)
  ;; Don't show tab number in modeline.
  (doom-modeline-remove-segment 'workspace-name 'all)
  (doom-modeline-mode))

(when-let* ((font (face-font 'fixed-pitch)))
  (set-face-font 'mode-line            font)
  (set-face-font 'mode-line-active     font)
  (set-face-font 'mode-line-inactive   font)
  (set-face-font 'header-line          font)
  (when (<= 31 emacs-major-version)
    (set-face-font 'header-line-inactive font)))

;;; .
(provide 'helheim-modeline)
;;; helheim-modeline.el ends here
