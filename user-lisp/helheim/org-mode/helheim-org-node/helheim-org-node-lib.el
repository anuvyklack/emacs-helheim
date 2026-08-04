;;; helheim-org-node-lib.el                           -*- lexical-binding: t -*-

(require 'dash)
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

(defun helheim-org-node-inhibit-auto-fill-a (orig-fun &rest args)
  "Fix for https://github.com/meedstrom/org-node/issues/180"
  (let ((auto-fill-function nil))
    (apply orig-fun args)))

;;; Backlinks buffer

(defun helheim-org-node-context--add-empty-line-at-eob ()
  "Add empty line at the end of a section to separate it from the following one."
  (goto-char (point-max))
  (insert "\n"))

(defvar org-node-context-main-buffer)
(declare-function org-node-context--refresh "org-node-context")

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

;;; Auto generate agenda files list

(defcustom helheim-org-mem-agenda-ignored-file-p
  'helheim-org-mem-agenda-ignored-file-default-p
  "The predicate that should return non-nil if FILE should not be included
into `org-agenda-files'."
  :type 'function
  :group 'helheim)

(defcustom helheim-org-mem-agenda-entry-p
  'helheim-org-mem-agenda-entry-default-p
  "The predicate that should return non-nil if file containing ENTRY should
be included into `org-agenda-files'."
  :type 'function
  :group 'helheim)

(defun helheim-org-mem-agenda-ignored-file-default-p (file)
  "Return non-nil if FILE should not be included into `org-agenda-files'.
Exclude files with:

- word \"archive\" somewhere in their path like: \"*.org_archive\" or
  \"*archive/2025.org\".

- #+category: ignore"
  (or (string-search "archive" file)
      (-> (alist-get "CATEGORY" (org-mem-file-keywords file) nil nil #'string=)
          (-first-item)
          (string= "ignore"))))

(defun helheim-org-mem-agenda-entry-default-p (entry)
  "Return non-nil if file containing ENTRY should be included into
`org-agenda-files'."
  (or (org-mem-entry-active-timestamps entry)
      (and (not (org-mem-entry-closed entry))
           (or (and (org-mem-entry-todo-state entry)
                    (not (member (org-mem-entry-todo-state entry)
                                 '("MAYBE" "READ" "DONE"))))
               (org-mem-entry-scheduled entry)
               (org-mem-entry-deadline entry)))))

(defun helheim-set-agenda-files (&rest _)
  "Auto generate `org-agenda-files' list."
  (setq org-agenda-files
        (-filter (lambda (file)
                   (and (not (funcall helheim-org-mem-agenda-ignored-file-p file))
                        (-find helheim-org-mem-agenda-entry-p
                               (org-mem-entries-in file))))
                 (org-mem-all-files))))

;;; Commands

(defun helheimg-org-node-create-ignored-node ()
  "Add ID to node, and say Org-node to ignore it."
  (interactive)
  (call-interactively 'org-node-nodeify-entry)
  (org-set-property "ROAM_EXCLUDE" "t"))

;;; .
(provide 'helheim-org-node '(lib))
;;; helheim-org-node-lib.el ends here
