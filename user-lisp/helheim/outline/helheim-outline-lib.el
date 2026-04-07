;;; helheim-outline-lib.el -*- lexical-binding: t -*-

(require 'outline)
(require 'hel)

(hel-define-advice outline-up-heading (:before (&rest _) push-mark)
  (hel-push-point))

(defun helheim-outline-up-heading (count &optional invisible-ok)
  "Move up in the outline hierarchy to the parent heading."
  (interactive "p")
  (hel-delete-all-fake-cursors)
  (deactivate-mark)
  (hel-push-point)
  (if (outline-on-heading-p invisible-ok)
      (outline-up-heading count invisible-ok)
    (outline-back-to-heading invisible-ok)
    (cl-decf count)
    (unless (zerop count)
      (outline-up-heading count invisible-ok))))

(put 'helheim-outline-up-heading 'repeat-map 'outline-navigation-repeat-map)

(defun helheim-outline-open ()
  (interactive)
  (outline-show-entry)
  (outline-show-children))

(defun helheim-outline-hide-other ()
  (interactive)
  (outline-hide-other)
  (outline-show-branches))

(defun helheim-outline-show-2-sublevels ()
  "Remain 2 top levels of headings visible."
  (interactive)
  (outline-hide-sublevels 2))

(defun helheim-outline-mark-subtree ()
  "Mark the current subtree in an outlined document."
  (interactive)
  (hel-push-point)
  (if (outline-on-heading-p)
      ;; we are already looking at a heading
      (forward-line 0)
    ;; else go back to previous heading
    (outline-previous-visible-heading 1))
  (hel-set-region (point)
                  (progn (outline-end-of-subtree)
                         (unless (eobp) (forward-char))
                         (point))
                  -1)
  (hel-reveal-point-when-on-top))

;;; .
(provide 'helheim-outline '(lib))
;;; helheim-outline-lib.el ends here
