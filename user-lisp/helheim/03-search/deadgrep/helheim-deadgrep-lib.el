;;; helheim-deadgrep-lib.el -*- lexical-binding: t -*-

(eval-when-compile (require 'hel-common))

(declare-function deadgrep-visit-result-other-window "deadgrep")
(declare-function deadgrep-forward-match "deadgrep")
(declare-function deadgrep-backward-match "deadgrep")

;; o or C-o
(defun +deadgrep-show-result-other-window ()
  "Show search result at point in another window."
  (interactive)
  (unless next-error-follow-minor-mode
    (hel-recenter-point-on-jump
      (save-selected-window
        (deadgrep-visit-result-other-window)
        (deactivate-mark)))))

;; C-j
(defun +deadgrep-forward-match-show-other-window ()
  "Move point to next search result and show it in another window."
  (interactive)
  (deadgrep-forward-match)
  (+deadgrep-show-result-other-window))

;; C-k
(defun +deadgrep-backward-match-show-other-window ()
  "Move point to previous search result and show it in another window."
  (interactive)
  (deadgrep-backward-match)
  (+deadgrep-show-result-other-window))

;;; .
(provide 'helheim-deadgrep '(lib))
;;; helheim-deadgrep-lib.el ends here
