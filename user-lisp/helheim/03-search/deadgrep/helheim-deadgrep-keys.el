;;; helheim-deadgrep-keys.el -*- lexical-binding: t -*-

(eval-when-compile
  (require 'hel-macros)
  (require 'hel-core))
(require 'deadgrep)

;;; Keybindings

(hel-keymap-set deadgrep-mode-map
  :unset "g"

  "i"   'deadgrep-edit-mode

  "a"   'deadgrep-incremental ; "a" for amend
  "g r" 'deadgrep-restart     ; also "C-w r"

  "RET" 'deadgrep-visit-result-other-window

  "o"   '+deadgrep-show-result-other-window
  "C-o" '+deadgrep-show-result-other-window

  "n"   'deadgrep-forward-match
  "N"   'deadgrep-backward-match

  "C-j" '+deadgrep-forward-match-show-other-window
  "C-k" '+deadgrep-backward-match-show-other-window

  "}"   'deadgrep-forward-filename
  "{"   'deadgrep-backward-filename
  "] p" 'deadgrep-forward-filename
  "[ p" 'deadgrep-backward-filename

  "z j" 'deadgrep-forward-filename
  "z k" 'deadgrep-backward-filename
  "z u" 'deadgrep-parent-directory)

(hel-keymap-set deadgrep-edit-mode-map :state 'normal
  "<escape>" 'deadgrep-mode
  "z x" 'deadgrep-mode
  "Z Z" 'deadgrep-mode
  "RET" 'deadgrep-visit-result-other-window

  ;; Commands bound to these keys have no sense for Deadgrep.
  "o"   'undefined
  "O"   'undefined
  "J"   'undefined)

;;; Commands

(defun +deadgrep-show-result-other-window ()
  "Show search result at point in another window."
  (interactive)
  (unless next-error-follow-minor-mode
    (hel-recenter-point-on-jump
      (save-selected-window
        (deadgrep-visit-result-other-window)
        (deactivate-mark)))))

(defun +deadgrep-forward-match-show-other-window ()
  "Move point to next search result and show it in another window."
  (interactive)
  (deadgrep-forward-match)
  (+deadgrep-show-result-other-window))

(defun +deadgrep-backward-match-show-other-window ()
  "Move point to previous search result and show it in another window."
  (interactive)
  (deadgrep-backward-match)
  (+deadgrep-show-result-other-window))

;;; .
(provide 'helheim-deadgrep '(keys))
;;; helheim-deadgrep-keys.el ends here
