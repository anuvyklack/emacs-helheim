;;; helheim-ediff-keys.el -*- lexical-binding: t -*-
;;; Commentary:
;;
;; All credit goes to the `evil-collection' package.
;;
;;; Keybindings

(eval-when-compile (require 'cl-lib))
(require 'hel-core)
(require 'ediff)

;;;###autoload
(defun helheim-ediff-setup-keys ()
  "Setup Ediff keys."
  (hel-keymap-set ediff-mode-map :state 'motion
    "d"   'ediff-jump-to-difference
    "H"   'ediff-toggle-hilit
    "j"   'ediff-next-difference
    "k"   'ediff-previous-difference
    "N"   'ediff-previous-difference
    "C-d" 'helheim-ediff-scroll-down
    "C-u" 'helheim-ediff-scroll-up
    "C-e" 'helheim-ediff-scroll-down-1
    "C-y" 'helheim-ediff-scroll-up-1
    "g g" 'helheim-ediff-first-difference
    "G"   'helheim-ediff-last-difference
    "C-z" 'ediff-suspend
    "z l" 'helheim-ediff-scroll-right
    "z h" 'helheim-ediff-scroll-left
    "u a" 'ediff-restore-diff
    "u b" 'ediff-restore-diff
    "u c" 'ediff-restore-diff)
  (unless (or ediff-3way-comparison-job
              (eq ediff-split-window-function 'split-window-vertically))
    (hel-keymap-set ediff-mode-map :state 'motion
      "l" 'ediff-copy-A-to-B
      "h" 'ediff-copy-B-to-A))
  ;; (hel-update-active-keymaps)
  (hel-motion-state))

;;;; Help message

(let ((messages '(ediff-long-help-message-compare2
                  ediff-long-help-message-compare3
                  ediff-long-help-message-narrow2
                  ediff-long-help-message-word-mode
                  ediff-long-help-message-merge
                  ediff-long-help-message-head
                  ediff-long-help-message-tail))
      (replace '(;;("^" . "  ")
                 ("p,DEL -previous diff " . "  k,N -previous diff ")
                 ("n,SPC -next diff     " . "  j,n -next diff     ")
                 ("    j -jump to diff  " . "    d -jump to diff  ")
                 ("    h -highlighting  " . "    H -highlighting  ")
                 ("  v/V -scroll up/dn  " . "C-u/d -scroll up/dn  ")
                 ("  </> -scroll lt/rt  " . "zh/zl -scroll lt/rt  ")
                 ("  z/q -suspend/quit"   . "C-z/q -suspend/quit"))))
  (dolist (msg messages)
    (cl-loop for (from . to) in replace
             do (setf (symbol-value msg)
                      (replace-regexp-in-string from to (symbol-value msg))))))

;;; Commands

(defun helheim-ediff-scroll-left (&optional arg)
  "Scroll left."
  (interactive "P")
  (let ((last-command-event ?>))
    (ediff-scroll-horizontally arg)))

(defun helheim-ediff-scroll-right (&optional arg)
  "Scroll right."
  (interactive "P")
  (let ((last-command-event ?<))
    (ediff-scroll-horizontally arg)))

(defun helheim-ediff-scroll-up (&optional arg)
  "Scroll up by half of a page."
  (interactive "P")
  (let ((last-command-event ?V))
    (ediff-scroll-vertically arg)))

(defun helheim-ediff-scroll-down (&optional arg)
  "Scroll down by half of a page."
  (interactive "P")
  (let ((last-command-event ?v))
    (ediff-scroll-vertically arg)))

(defun helheim-ediff-scroll-down-1 ()
  "Scroll down by a line."
  (interactive)
  (let ((last-command-event ?v))
    (ediff-scroll-vertically 1)))

(defun helheim-ediff-scroll-up-1 ()
  "Scroll down by a line."
  (interactive)
  (let ((last-command-event ?V))
    (ediff-scroll-vertically 1)))

(defun helheim-ediff-first-difference ()
  "Jump to first difference."
  (interactive)
  (ediff-jump-to-difference 1))

(defun helheim-ediff-last-difference ()
  "Jump to last difference."
  (interactive)
  (ediff-jump-to-difference ediff-number-of-differences))

;;; .
(provide 'helheim-ediff '(keys))
;;; helheim-ediff-keys.el ends here
