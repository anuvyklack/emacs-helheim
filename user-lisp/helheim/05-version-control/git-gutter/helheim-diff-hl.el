;;; helheim-diff-hl.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Keybindings
;;;; Entry points

(with-eval-after-load 'diff-hl
  (hel-keymap-set diff-hl-mode-map :state 'normal
    ;; "] ]" 'diff-hl-next-hunk
    ;; "[ [" 'diff-hl-previous-hunk
    ;; "[ {" 'diff-hl-show-hunk-previous
    ;; "] }" 'diff-hl-show-hunk-next
    "] v" 'diff-hl-show-hunk-next
    "[ v" 'diff-hl-show-hunk-previous)
  (hel-keymap-set diff-hl-mode-map
    "C-c v ]" '("Next hunk" . diff-hl-next-hunk)
    "C-c v [" '("Prev hunk" . diff-hl-previous-hunk)
    "C-c v V" '("View hunk" . diff-hl-show-hunk)
    "C-c v }" '("View next hunk" . diff-hl-show-hunk-next)
    "C-c v {" '("View prev hunk" . diff-hl-show-hunk-previous)
    "C-c v s" '("Stage hunk" . diff-hl-stage-dwim)
    "C-c v r" '("Revert hunk" . diff-hl-revert-hunk)
    "C-c v =" '("Goto hunk" . diff-hl-diff-goto-hunk)))

;;; Config

(leaf diff-hl
  :straight t
  :commands (diff-hl-stage-current-hunk
             diff-hl-revert-hunk
             diff-hl-next-hunk
             diff-hl-previous-hunk)
  :custom
  (diff-hl-show-staged-changes . nil)
  (diff-hl-show-hunk-function . 'diff-hl-show-hunk-inline)
  (diff-hl-show-hunk-inline-smart-lines . nil)
  :hook
  (after-init-hook . global-diff-hl-mode)
  (dired-mode-hook . diff-hl-dired-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  ;; (dired-mode-hook . diff-hl-dired-mode-unless-remote)
  ;; (vc-dir-mode-hook . turn-on-diff-hl-mode)
  ;; (diff-hl-mode-hook . diff-hl-flydiff-mode)
  (diff-hl-mode-hook . diff-hl-show-hunk-mouse-mode)
  :defer-config
  ;; Suppress default repeat-map assigment.
  (setq diff-hl-repeat-exceptions '(diff-hl-revert-hunk
                                    diff-hl-previous-hunk
                                    diff-hl-next-hunk
                                    diff-hl-show-hunk
                                    diff-hl-show-hunk-previous
                                    diff-hl-show-hunk-next
                                    diff-hl-stage-dwim)))

(with-eval-after-load 'diff-hl-show-hunk
  (hel-keymap-set diff-hl-show-hunk-map :state 'motion
    "["   'diff-hl-show-hunk-previous
    "]"   'diff-hl-show-hunk-next)
  (hel-keymap-set diff-hl-show-hunk-map
    "C-j" 'diff-hl-show-hunk-next
    "C-k" 'diff-hl-show-hunk-previous
    "y"   'diff-hl-show-hunk-copy-original-text
    "s"   'diff-hl-show-hunk-stage-hunk))

(leaf diff-hl-show-hunk-inline
  :defer-config
  (hel-keymap-set diff-hl-show-hunk-inline-transient-mode-map :state 'motion
    "j"   'diff-hl-show-hunk-inline--popup-up
    "k"   'diff-hl-show-hunk-inline--popup-down
    "C-b" 'diff-hl-show-hunk-inline--popup-pagedown
    "C-f" 'diff-hl-show-hunk-inline--popup-pageup
    "C-u" 'diff-hl-show-hunk-inline--popup-pagedown
    "C-d" 'diff-hl-show-hunk-inline--popup-pageup)
  :config
  (define-advice diff-hl-show-hunk-inline (:after (&rest _) helheim)
    (setq diff-hl-show-hunk-inline--current-footer
          (if diff-hl-show-staged-changes
              "(q)Quit  (])Next  ([)Previous  (r)Revert  (y)Copy original"
            "(q)Quit  (])Next  ([)Previous  (s)Stage  (r)Revert  (y)Copy original"))
    (diff-hl-show-hunk-inline-scroll-to 0)))

;;; .
(provide 'helheim-diff-hl)
;;; helheim-diff-hl.el ends here
