;;; helheim-org-lib.el                                -*- lexical-binding: t -*-

(eval-when-compile (require 'cl-macs))
(require 'dash)
(require 'org)
(require 'hel)

;;; Add padding to headings

(defun helheim-org--add-padding-to-headings ()
  "Replace the 2nd element in `org-font-lock-extra-keywords' responsible for
heading fontification."
  (setf (nth 1 org-font-lock-extra-keywords)
        `(,(if org-fontify-whole-heading-line
               "^\\(\\**\\)\\(\\*\\)\\( \\)\\(.*\n?\\)"
             "^\\(\\**\\)\\(\\*\\)\\( \\)\\(.*\\)")
          (1 (helheimg-org--level-face 1))
          (2 (helheimg-org--level-face 2))
          (3 (helheimg-org--level-face 3))
          (4 (helheimg-org--level-face 4)))))

(defun helheimg-org--level-face (n)
  "Get the right face for match N in font-lock matching of headlines."
  (let* ((org-l0 (- (match-end 3) (match-beginning 1) 1))
         (org-l (if org-odd-levels-only (1+ (/ org-l0 2)) org-l0))
         (face (if org-cycle-level-faces
                   (nth (% (1- org-l) org-n-level-faces) org-level-faces)
                 (nth (1- (min org-l org-n-level-faces)) org-level-faces))))
    (cond ((eq n 1) (if org-hide-leading-stars 'org-hide face))
          ((eq n 2) face)
          ((eq n 3) 'helheim-org-heading-padding-face)
          (t (unless org-level-color-stars-only face)))))

;;; org-attach

(defun helheim-org-attach-id-ts-folder-format (id)
  "Translate an ID based on a ISO8601 timestamp to a folder-path.
Place the attachment folder into year folder: 2025/20251104T161807"
  (if (< 4 (length id))
      (format "%s/%s"
              (substring id 0 4)
              id)))

(defvar org-attach-commands)
(declare-function org-attach-dir "org-attach" (&optional create-if-not-exists-p no-fs-check))

(defun helheim-org-attach ()
  "The dispatcher for attachment commands.
Like `org-attach' but tuned for Emacs Helheim."
  (interactive)
  (let (marker)
    (when (eq major-mode 'org-agenda-mode)
      (setq marker (or (get-text-property (point) 'org-hd-marker)
                       (get-text-property (point) 'org-marker)))
      (unless marker
        (error "No item in current line")))
    (org-with-point-at marker
      (let ((dir (org-attach-dir nil :no-fs-check)))
        (cl-assert dir nil "Cannot derive attachment directory from ID")
        (deactivate-mark)
        (if (not (featurep 'org-inlinetask))
            (org-back-to-heading-or-point-min t)
          ;; else
          (if (org-inlinetask-in-task-p)
              (org-inlinetask-goto-beginning)
            ;; else
            (org-with-limited-levels
             (org-back-to-heading-or-point-min t))))
        (let (key)
          (save-excursion
            (save-window-excursion
              (unless org-attach-expert
                (helheimg--org-attach-buffer dir)
                (org-fit-window-to-buffer (get-buffer-window "*Org Attach*")))
              (unwind-protect
                  (progn
                    (message "Select command: [%s]"
                             (concat (mapcar #'caar org-attach-commands)))
                    (while (and (setq key (read-char-exclusive))
                                (memq key '(?\C-f ?\C-b)))
	              (org-scroll (alist-get key '((?\C-f . ?\C-n)
                                                   (?\C-b . ?\C-p))))))
                (-some->> (get-buffer-window "*Org Attach*" t)
                  (quit-window :kill))
	        (-some-> (get-buffer "*Org Attach*")
                  (kill-buffer)))))
          (if-let* ((command (-some (lambda (entry)
				      (and (memq key (nth 0 entry))
                                           (nth 1 entry)))
			            org-attach-commands))
                    ((commandp command)))
	      (command-execute command)
	    (error "No such attachment command: %c" key)))))))

(defun helheimg--org-attach-buffer (dir)
  (switch-to-buffer-other-window "*Org Attach*")
  (erase-buffer)
  (setq cursor-type nil)
  (setq header-line-format (format "Use %s and %s for scrolling"
                                   (propertize "C-f" 'face 'help-key-binding)
                                   (propertize "C-b" 'face 'help-key-binding)))
  (insert
   (concat "Attachment folder:\n\n"
           (propertize (abbreviate-file-name dir)
                       'face 'font-lock-string-face)
           "\n\n  "
           (if (file-directory-p dir)
               (propertize "Exist" 'face 'success)
             (propertize "Does not exist" 'face 'warning))
           "\n\n"
           "Select an Attachment Command:\n\n"
           (mapconcat (lambda (entry)
	                (pcase entry
		          (`((,key . ,_) ,_ ,docstring)
		           (format "%s       %s"
                                   (-> (char-to-string key)
                                       (propertize 'face 'help-key-binding))
                                   (replace-regexp-in-string "\n\\([\t ]*\\)"
                                                             "        "
                                                             docstring nil nil 1)))
		          (_
		           (user-error "Invalid `org-attach-commands' item: %S"
			               entry))))
	              org-attach-commands
	              "\n")))
  (goto-char (point-min)))

;;; org-cliplink

(declare-function org-cliplink "org-cliplink")

;; Based on https://xenodium.com/emacs-dwim-do-what-i-mean/
(defun helheim-org-insert-link ()
  "Like `org-insert-link' but with some \"do what i mean\" behavior.
- If URL is in clipboard — use it.
- If selection is active — use it as link description.
- Automatically fetch URL title from its HTML tag.
- Fallback to `org-insert-link'."
  (interactive)
  (let ((point-at-link? (org-in-regexp org-link-any-re 1))
        (clipboard-url (if-let* ((kill-ring)
                                 (kill (current-kill 0))
                                 ((string-match-p "^http" kill)))
                           kill))
        (region-content (if (region-active-p)
                            (buffer-substring-no-properties (region-beginning)
                                                            (region-end)))))
    (cond ((and region-content clipboard-url (not point-at-link?))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-at-link?))
           (org-cliplink))
          (t
           (call-interactively 'org-insert-link))))
  (hel-extend-selection -1))

;;; Convert markdown to org-mode

;;;###autoload
(defun +markdown-to-org-region (start end)
  "Convert region (START, END) from Markdown to Org-mode via pandoc."
  (interactive "r")
  (unless (executable-find "pandoc")
    (user-error "No pandoc executable found"))
  (shell-command-on-region start end
                           "pandoc --from=markdown --to=org"
                           ;; "pandoc --from=markdown --to=org --wrap=preserve"
                           t t))

;;; .
(provide 'helheim-org '(lib))
;;; helheim-org-lib.el ends here
