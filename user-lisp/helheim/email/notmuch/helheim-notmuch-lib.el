;;; helheim-notmuch-lib.el                            -*- lexical-binding: t -*-
;;; Code:

(require 'hel)
(require 'notmuch)

;; v
(defun helheim-notmuch-toggle-selection ()
  "Toggle selection."
  (interactive nil notmuch-search-mode notmuch-tree-mode)
  (if (use-region-p)
      (deactivate-mark)
    (hel-expand-line-selection 1)))

;; j
(defun helheim-notmuch-search-next-line (count)
  (interactive "p" notmuch-search-mode)
  (if (region-active-p)
      (hel-expand-line-selection count)
    (next-line count)))

;; k
(defun helheim-notmuch-search-previous-line (count)
  (interactive "p" notmuch-search-mode)
  (if (region-active-p)
      (hel-expand-line-selection (- count))
    (previous-line count)))

;; j
(defun helheim-notmuch-tree-next-line ()
  (interactive nil notmuch-tree-mode)
  (if (region-active-p)
      (hel-expand-line-selection 1)
    (notmuch-tree-next-message)))

;; k
(defun helheim-notmuch-tree-previous-line ()
  (interactive nil notmuch-tree-mode)
  (if (region-active-p)
      (hel-expand-line-selection -1)
    (notmuch-tree-prev-message)))

;; RET
(defun helheim-notmuch-show-RET ()
  "Push button, compose a message to e-mail address or open URL at point."
  (interactive)
  (if (button-at (point))
      (push-button)
    (goto-address-at-point)
    ;; (browse-url-at-point)
    ))

;; zo
(defun +notmuch-show-unfold-message ()
  "Open current message."
  (interactive)
  (notmuch-show-message-visible (notmuch-show-get-message-properties) t)
  ;; (force-window-update)
  )

;; zc
(defun +notmuch-show-fold-message ()
  "Open current message."
  (interactive)
  (notmuch-show-message-visible (notmuch-show-get-message-properties) nil)
  ;; (force-window-update)
  )

;; zr
(defun +notmuch-show-open-all-messages ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (progn
             (notmuch-show-message-visible (notmuch-show-get-message-properties) t)
             (notmuch-show-goto-message-next)))))

;; zm
(defun +notmuch-show-close-all-messages ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (progn
             (notmuch-show-message-visible (notmuch-show-get-message-properties) nil)
             (notmuch-show-goto-message-next)))))

;;;; Show mode

;; notmuch-show-toggle-part-invisibility
;; (forward-button 1)

(defun helheim-notmuch-next-button-or-open-message ()
  (interactive)
  (setf (point) (min (save-excursion
                       (forward-button 1 nil nil t)
                       (point))
                     (save-excursion
                       (notmuch-show-next-open-message)
                       (point)))))

(defun helheim-notmuch-previous-button-or-open-message ()
  (interactive)
  (setf (point) (max (save-excursion
                       (backward-button 1 nil nil t)
                       (point))
                     (save-excursion
                       (notmuch-show-previous-open-message)
                       (point)))))

;;;; Delete

(defun +notmuch-search-delete ()
  "Mark message as deleted."
  (interactive)
  (notmuch-search-tag (notmuch-tag-change-list
                       (-concat +notmuch-delete-tags notmuch-archive-tags)))
  (notmuch-tree-next-message))

(defun +notmuch-tree-delete ()
  "Mark message as deleted."
  (interactive)
  (notmuch-tree-tag (notmuch-tag-change-list
                     (-concat +notmuch-delete-tags notmuch-archive-tags)))
  (notmuch-tree-next-message))

(defun +notmuch-tree-delete-thread ()
  "Mark whole thread as deleted."
  (interactive)
  (notmuch-tree-tag-thread (notmuch-tag-change-list
                            (-concat +notmuch-delete-tags notmuch-archive-tags))))

(defun +notmuch-show-delete ()
  "Mark message as deleted."
  (interactive)
  (notmuch-show-tag (notmuch-tag-change-list
                     (-concat +notmuch-delete-tags notmuch-archive-tags))))

(defun +notmuch-show-delete-thread ()
  "Mark whole thread as deleted."
  (interactive)
  (notmuch-show-tag-all (notmuch-tag-change-list
                         (-concat +notmuch-delete-tags notmuch-archive-tags))))

;;;; Flagged

(defun +notmuch-search-flagged ()
  (interactive)
  (notmuch-search-tag '("+flagged"))
  (notmuch-tree-next-message))

(defun +notmuch-tree-flagged ()
  (interactive)
  (notmuch-tree-tag '("+flagged"))
  (notmuch-tree-next-message))

(defun +notmuch-show-flagged ()
  (interactive)
  (notmuch-show-tag '("+flagged"))
  (notmuch-show-next-thread-show))

;;;; Compose

;;;###autoload
(defun +notmuch-compose ()
  "Compose new mail"
  (interactive)
  (notmuch-mua-mail
   nil nil
   `((From . ,(message-make-from
               (notmuch-user-name)
               (completing-read "From: " (notmuch-user-emails)))))))

;;; .
(provide 'helheim-notmuch '(lib))
;;; helheim-notmuch-lib.el ends here
