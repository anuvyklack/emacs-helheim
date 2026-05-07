;;; helheim-agent-shell-lib.el                        -*- lexical-binding: t -*-

(eval-when-compile (require 'dash))
(require 'agent-shell-ui)

(define-advice agent-shell-ui-backward-block (:override () helheim)
  "Jump to the beginning of current block, then on previous block."
  (interactive)
  (when-let*
      ((start-point (point))
       (block-start (when (get-text-property (point) 'agent-shell-ui-state)
                      (-some-> (agent-shell-ui--block-range :position (point))
                        (map-elt :start))))
       (target (if (/= (line-number-at-pos block-start)
                       (line-number-at-pos start-point))
                   block-start
                 (goto-char block-start)
                 (when-let* ((prev (text-property-search-backward
                                    'agent-shell-ui-state nil
                                    (lambda (_old-val new-val)
                                      (if new-val (map-elt new-val :navigatable)))
                                    t)))
                   (prop-match-beginning prev)))))
    (deactivate-mark)
    (goto-char target)
    target))

;;; .
(provide 'helheim-agent-shell '(lib))
;;; helheim-agent-shell-lib.el ends here
