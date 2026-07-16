;;; helheim-diff-hl.el            -*- lexical-binding: t; no-byte-compile: t -*-
;;; Config

(setup diff-hl
  (:install t)
  (:command diff-hl-stage-current-hunk
            diff-hl-revert-hunk
            diff-hl-next-hunk
            diff-hl-previous-hunk)
  (setopt diff-hl-show-staged-changes nil
          diff-hl-show-hunk-function 'diff-hl-show-hunk-inline
          diff-hl-show-hunk-inline-smart-lines nil)
  (:after-init global-diff-hl-mode)
  (:hook dired-mode-hook diff-hl-dired-mode)
  (:hook magit-post-refresh-hook diff-hl-magit-post-refresh)
  ;; (:hook dired-mode-hook diff-hl-dired-mode-unless-remote)
  ;; (:hook vc-dir-mode-hook turn-on-diff-hl-mode)
  ;; (:hook diff-hl-mode-hook diff-hl-flydiff-mode)
  (:hook diff-hl-mode-hook diff-hl-show-hunk-mouse-mode)
  (:after-load
    ;; Suppress default repeat-map assigment.
    (setq diff-hl-repeat-exceptions '(diff-hl-revert-hunk
                                      diff-hl-previous-hunk
                                      diff-hl-next-hunk
                                      diff-hl-show-hunk
                                      diff-hl-show-hunk-previous
                                      diff-hl-show-hunk-next
                                      diff-hl-stage-dwim))))

;;; Keybindings

;; Entry points
(setup diff-hl
  (:after-load
    (:keymap diff-hl-mode-map
      (:bind :state normal
        ;; "] ]" 'diff-hl-next-hunk
        ;; "[ [" 'diff-hl-previous-hunk
        ;; "[ {" 'diff-hl-show-hunk-previous
        ;; "] }" 'diff-hl-show-hunk-next
        "] v" '("next hunk" . diff-hl-next-hunk)
        "[ v" '("previous hunk" . diff-hl-previous-hunk)
        "] V" '("view next hunk" . diff-hl-show-hunk-next)
        "[ V" '("view prev hunk" . diff-hl-show-hunk-previous))
      (:bind
        "C-c v ]" '("view next hunk" . diff-hl-show-hunk-next)
        "C-c v [" '("view prev hunk" . diff-hl-show-hunk-previous)
        "C-c v v" '("view hunk" . diff-hl-show-hunk)
        "C-c v s" '("stage hunk" . diff-hl-stage-dwim)
        "C-c v r" '("revert hunk" . diff-hl-revert-hunk)
        "C-c v =" '("goto hunk" . diff-hl-diff-goto-hunk)))))

(setup diff-hl-show-hunk
  (:after-load
    (:keymap diff-hl-show-hunk-map
      (:bind :state emacs
        "["   'diff-hl-show-hunk-previous
        "]"   'diff-hl-show-hunk-next)
      (:bind
        "C-j" 'diff-hl-show-hunk-next
        "C-k" 'diff-hl-show-hunk-previous
        "y"   'diff-hl-show-hunk-copy-original-text
        "s"   'diff-hl-show-hunk-stage-hunk))))

(setup diff-hl-show-hunk-inline
  (:after-load
    (:keymap diff-hl-show-hunk-inline-transient-mode-map
      (:bind :state emacs
        "j"   'diff-hl-show-hunk-inline--popup-down
        "k"   'diff-hl-show-hunk-inline--popup-up
        "C-b" 'diff-hl-show-hunk-inline--popup-pageup
        "C-f" 'diff-hl-show-hunk-inline--popup-pagedown
        "C-u" 'diff-hl-show-hunk-inline--popup-pageup
        "C-d" 'diff-hl-show-hunk-inline--popup-pagedown))))

(define-advice diff-hl-show-hunk-inline (:after (&rest _) helheim)
  (setq diff-hl-show-hunk-inline--current-footer
        (if diff-hl-show-staged-changes
            "(q)Quit  (])Next  ([)Previous  (r)Revert  (y)Copy original"
          "(q)Quit  (])Next  ([)Previous  (s)Stage  (r)Revert  (y)Copy original"))
  (diff-hl-show-hunk-inline-scroll-to 0))

;;; .
(provide 'helheim-diff-hl)
;;; helheim-diff-hl.el ends here
