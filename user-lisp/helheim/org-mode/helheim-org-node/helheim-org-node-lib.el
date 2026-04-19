;;; helheim-org-node-lib.el -*- lexical-binding: t -*-

(require 'org)
(require 'org-mem)

(defun helheim-org-node-append-tags (node title)
  "Append NODE\\='s tags to TITLE."
  (list title
        ""
        (if-let* ((tags (org-mem-entry-tags node)))
            (propertize (concat "   :" (string-join tags ":") ":")
                        'face 'org-node-tag)
          "")))

(defun helheim-org-node-filter-p (node)
  "Hide NODE if it has or inherits an :IGNORE: or :ROAM_EXCLUDE: properties."
  (not (or (org-mem-property-with-inheritance "IGNORE" node)
           (org-mem-property-with-inheritance "ROAM_EXCLUDE" node))))

;;; Backlinks drawers

(defun helheim-org-node-backlink-format (id desc &optional _time)
  "Format as list item: \"- [[id:ID][Node title]]\".
ID and DESC are link id and description, TIME a Lisp time value."
  (concat "- " (org-link-make-string (concat "id:" id)
                                     (org-link-display-format desc))))

;;; Backlinks buffer

(defun helheim-org-node-context--add-empty-line-at-eob ()
  "Add empty line at the end of a section to separate it from the following one."
  (goto-char (point-max))
  (insert "\n"))

(defun helheim-org-node-backlinks-buffer ()
  "Show backlinks buffer for the node at point.
Org-node native command is `org-node-context-dwim'."
  (interactive)
  (require 'org-node-context)
  (when (derived-mode-p 'org-mode)
    (let ((buffer (get-buffer-create org-node-context-main-buffer)))
      (org-node-context--refresh buffer (org-entry-get-with-inheritance "ID"))
      (progn (set-buffer buffer)
             (goto-char (point-min)))
      (display-buffer buffer))))

(defun helheim-open-in-another-window-a (orig-fun &rest args)
  "Open backlinks buffer in another window."
  ;; Set `display-buffer-overriding-action' only if it wasn't set before
  ;; us by `same-window-prefix' or `other-window-prefix' or any other.
  (if (equal display-buffer-overriding-action '(nil . nil))
      (let ((display-buffer-overriding-action
             '(nil
               (inhibit-same-window . t))))
        (apply orig-fun args))
    ;; else
    (apply orig-fun args)))

;;; Commands

(defun helheimg-org-node-create-ignored-node ()
  "Add ID to node, and say Org-node to ignore it."
  (interactive)
  (call-interactively 'org-node-nodeify-entry)
  (org-set-property "ROAM_EXCLUDE" "t"))

;;; .
(provide 'helheim-org-node '(lib))
;;; helheim-org-node-lib.el ends here
