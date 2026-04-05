;;; helheim-diff-hl.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Config

(setup diff-hl
  (:install t)
  (:autoload diff-hl-stage-current-hunk
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
    (:with-keymap diff-hl-mode-map
      (:bind :state 'normal
        ;; "] ]" 'diff-hl-next-hunk
        ;; "[ [" 'diff-hl-previous-hunk
        ;; "[ {" 'diff-hl-show-hunk-previous
        ;; "] }" 'diff-hl-show-hunk-next
        "] v" 'diff-hl-show-hunk-next
        "[ v" 'diff-hl-show-hunk-previous)
      (:bind
        "C-c v ]" '("Next hunk" . diff-hl-next-hunk)
        "C-c v [" '("Prev hunk" . diff-hl-previous-hunk)
        "C-c v V" '("View hunk" . diff-hl-show-hunk)
        "C-c v }" '("View next hunk" . diff-hl-show-hunk-next)
        "C-c v {" '("View prev hunk" . diff-hl-show-hunk-previous)
        "C-c v s" '("Stage hunk" . diff-hl-stage-dwim)
        "C-c v r" '("Revert hunk" . diff-hl-revert-hunk)
        "C-c v =" '("Goto hunk" . diff-hl-diff-goto-hunk)))))

(setup diff-hl-show-hunk
  (:after-load
    (:with-keymap diff-hl-show-hunk-map
      (:bind :state 'motion
        "["   'diff-hl-show-hunk-previous
        "]"   'diff-hl-show-hunk-next)
      (:bind
        "C-j" 'diff-hl-show-hunk-next
        "C-k" 'diff-hl-show-hunk-previous
        "y"   'diff-hl-show-hunk-copy-original-text
        "s"   'diff-hl-show-hunk-stage-hunk))))

(setup diff-hl-show-hunk-inline
  (:after-load
    (:with-keymap diff-hl-show-hunk-inline-transient-mode-map
      (:bind :state 'motion
        "j"   'diff-hl-show-hunk-inline--popup-up
        "k"   'diff-hl-show-hunk-inline--popup-down
        "C-b" 'diff-hl-show-hunk-inline--popup-pagedown
        "C-f" 'diff-hl-show-hunk-inline--popup-pageup
        "C-u" 'diff-hl-show-hunk-inline--popup-pagedown
        "C-d" 'diff-hl-show-hunk-inline--popup-pageup))))

(define-advice diff-hl-show-hunk-inline (:after (&rest _) helheim)
  (setq diff-hl-show-hunk-inline--current-footer
        (if diff-hl-show-staged-changes
            "(q)Quit  (])Next  ([)Previous  (r)Revert  (y)Copy original"
          "(q)Quit  (])Next  ([)Previous  (s)Stage  (r)Revert  (y)Copy original"))
  (diff-hl-show-hunk-inline-scroll-to 0))

;;; .
(provide 'helheim-diff-hl)
;;; helheim-diff-hl.el ends here
