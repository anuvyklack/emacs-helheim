;;; helheim-org.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Keybindings

(setup org
  (with-eval-after-load 'org-keys
    (:with-keymap org-mode-map
      (:bind :state 'normal
        "z '" 'org-edit-special
        "z ," 'org-insert-structure-template
        "z /" 'org-sparse-tree
        ;; "z n" 'org-narrow-to-subtree
        "g i" 'consult-org-heading
        "g x" 'org-open-at-point
        "g n" 'org-next-link
        "g N" 'org-previous-link
        "] l" 'org-next-link
        "[ l" 'org-previous-link
        ","   (define-keymap
                "a" 'org-attach
                "o" 'org-open-at-point)))
    ;; <leader>
    (let ((org-leader (or (keymap-lookup org-mode-map "C-c")
                          (make-sparse-keymap))))
      (:with-keymap org-leader
        (:unbind
          "'"  ;; `org-edit-special' — moved to "z'"
          ","  ;; `org-priority' — moved to "z,"
          "/") ;; `org-sparse-tree' — moved to "z/"
        (:bind
          "RET" 'dired-jump ;; rebind `org-ctrl-c-ret', which is also on "z RET"
          "a"   'org-attach
          "t i" 'org-toggle-inline-images
          "t l" 'org-toggle-link-display
          "t f" 'org-table-toggle-formula-debugger
          "t o" 'org-table-toggle-coordinate-overlays
          "i"   (cons "insert"
                      (define-keymap
                        "l" '("insert link" . org-insert-link)
                        "m" 'yank-media
                        "d" 'org-deadline
                        "s" 'org-schedule
                        "t" 'org-time-stamp
                        "T" 'org-time-stamp-inactive
                        "Q" 'org-set-tags-command))
          "l"   (cons "links"
                      (define-keymap
                        "l" 'org-insert-link
                        "i" 'org-insert-last-stored-link ;; "i" for insert
                        "s" 'org-store-link
                        "a" 'org-insert-all-links
                        "m" 'yank-media)))))))

(setup dired
  (:after-load
    (:with-keymap dired-mode-map
      (:bind "C-c a" 'org-attach-dired-to-subtree))))

;;; Config

(setup org
  (:install t)
  ;; (:built-in)
  (setopt org-insert-heading-respect-content nil
          org-M-RET-may-split-line '((default . t)
                                     (item . nil))
          org-return-follows-link t
          org-special-ctrl-a/e t
          org-pretty-entities t))

(setup ol
  (:after-load
    ;; Open org links in the same window. Use "C-c &" to back to the link.
    (setf (alist-get 'file org-link-frame-setup) #'find-file)))

(setup org-indent
  (:blackout t))

(setup hel-org
  (:after org)
  (:require t))

(setup org-eldoc
  (:install org-contrib)
  (:after org)
  (setopt org-eldoc-breadcrumb-separator " → ")
  (:after-load
    ;; Show target for link at point. Emacs has `help-at-pt-display-when-idle',
    ;; but its timer competes with Eldoc for the echo area, so for those who use
    ;; Eldoc in Emacs 31 `eldoc-help-at-pt' option was added.
    (if (version<= "31" emacs-version)
        (setopt eldoc-help-at-pt t) ;; since Emacs 31
      (define-advice org-eldoc-documentation-function (:before-until (&rest _) helheim)
        "Display link target in echo area when cursor/mouse is over it."
        (if-let ((url (thing-at-point 'url t)))
            (format "LINK: %s" url))))
    ;; HACK Fix #2972: infinite recursion when eldoc kicks in 'org' or 'python'
    ;;   src blocks.
    ;; TODO Should be reported upstream!
    (puthash "org" #'ignore org-eldoc-local-functions-cache)
    (puthash "plantuml" #'ignore org-eldoc-local-functions-cache)
    (puthash "python" #'python-eldoc-function org-eldoc-local-functions-cache)))

;;;; Add padding to headings

(defcustom helheim-org-heading-padding 1.6
  "Add extra padding to Org mode headings to make them more spacious.
This is done by increasing the height of the space character between the stars
that denotes the heading level and the heading text. See help for
`set-face-attribute' -> `:height' for the meaning of the value.

If nil — extra padding will be disabled.

Must be set with `setopt' function!"
  :type 'number
  :group 'helheim
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (not value)
             (remove-hook 'org-font-lock-set-keywords-hook
                          'helheim-org--add-padding-to-headings)
           ;; else
           (-doto (make-face 'helheim-org-heading-padding-face)
             (set-face-attribute nil :height value))
           (add-hook 'org-font-lock-set-keywords-hook
                     'helheim-org--add-padding-to-headings))))

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

;;;; Prettify symbols mode

(setup org
  ;; You may delete the hooks you don't like with:
  ;;   (remove-hook 'org-mode-hook 'helheim-org-prettify-todo-keywords)
  (:hook org-mode-hook (prettify-symbols-mode
                        helheim-org-prettify-todo-keywords
                        helheim-org-prettify-blocks))
  (setq-default org-todo-keywords
                '((sequence "SOMEDAY" "TODO" "IN-PROGRESS" "WAIT" "|"
                            "DONE" "ARCHIVED" "CANCELLED")
                  (sequence "READ" "IN-PROGRESS" "|" "DONE"))))

(defun helheim-org-prettify-todo-keywords ()
  "Beautify org mode \"todo\" keywords using `prettify-symbols-mode'."
  (cl-callf append prettify-symbols-alist
    '(("SOMEDAY"     . ?󰒅) ; 󰔌
      ("TODO"        . ?󰄱) ; 󰝣
      ("IN-PROGRESS" . ?󰡖) ; 󱗝 󰜄 󰤌
      ("WAIT"        . ?)
      ("DONE"        . ?󰄵) ; 󰱒
      ("ARCHIVED"    . ?󱈎)
      ("CANCELLED"   . ?󰅘)
      ("READ"        . ?󰃃))))

(defun helheim-org-prettify-blocks ()
  "Beautify org mode block keywords using `prettify-symbols-mode'."
  (cl-callf append prettify-symbols-alist
    (eval-when-compile
      (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
              '(("#+begin_src"     . ?)
                ("#+end_src"       . ?)
                ("#+begin_example" . ?)
                ("#+end_example"   . ?)
                ("#+begin_quote"   . ?)
                ("#+end_quote"     . ?))))))

;;;; ID format

;; Use ISO 8601 timestamp.
(setopt org-id-method 'ts
        org-id-ts-format helheim-id-format
        org-id-link-to-org-use-id 'create-if-interactive)

;;;; org-attach

;; FIX: Link to attachment can't be oppend before `org-attach' is loaded,
;;   and `org-open-at-point' loads it only for headings, but not for links.
(add-to-list 'org-modules 'org-attach)

(setup org-attach
  (:after org)
  (setopt org-attach-id-dir (expand-file-name "org-attach/" org-directory)
          org-attach-method 'mv ;; move
          org-attach-store-link-p 'attached
          org-attach-preferred-new-method 'id
          org-attach-use-inheritance nil
          org-attach-dir-relative t
          org-attach-sync-delete-empty-dir t
          org-attach-id-to-path-function-list '(helheim-org-attach-id-ts-folder-format
                                                org-attach-id-uuid-folder-format
                                                identity)
          org-attach-auto-tag "ATTACH")
  (add-to-list 'org-tags-exclude-from-inheritance org-attach-auto-tag)
  (with-eval-after-load 'org-keys
    (:with-keymap org-mode-map
      (:bind [remap org-attach] 'helheim-org-attach))))

(defun helheim-org-attach-id-ts-folder-format (id)
  "Translate an ID based on a ISO8601 timestamp to a folder-path.
Place the attachment folder into year folder: 2025/20251104T161807"
  (if (< 4 (length id))
      (format "%s/%s"
              (substring id 0 4)
              id)))

(setq org-attach-commands
      '(((?a ?\C-a) org-attach-attach
         "Select a file and attach it to the task, using `org-attach-method'.")
        ((?c ?\C-c) org-attach-attach-cp
         "Attach a file using copy method.")
        ((?m ?\C-m) org-attach-attach-mv
         "Attach a file using move method.")
        ((?l ?\C-l) org-attach-attach-ln
         "Attach a file using link method.")
        ((?y ?\C-y) org-attach-attach-lns
         "Attach a file using symbolic-link method.")
        ((?u ?\C-u) org-attach-url
         "Attach a file from URL (downloading it).")
        ((?b) org-attach-buffer
         "Select a buffer and attach its contents to the task.")
        ((?n ?\C-n) org-attach-new
         "Create a new attachment, as an Emacs buffer.")
        ((?z ?\C-z) org-attach-sync
         "Synchronize the current node with its attachment\n directory, in case \
you added attachments yourself.\n")
        ((?o ?\C-o) org-attach-open
         "Open current node's attachments.")
        ((?O) org-attach-open-in-emacs
         "Like \"o\", but force opening in Emacs.")
        ((?f ?\C-f) org-attach-reveal-in-emacs
         "Open current node's attachment directory in Dired.  Create if missing.")
        ((?F) org-attach-reveal
         "Like \"f\", but try to open in system file manager.\n")
        ((?d ?\C-d) org-attach-delete-one
         "Delete one attachment, you will be prompted for a file name.")
        ((?D) org-attach-delete-all
         "Delete all of a node's attachments.  A safer way is\n to open the \
directory in dired and delete from there.\n")
        ((?s ?\C-s) org-attach-set-directory
         "Set a specific attachment directory for this entry. Sets DIR property.")
        ((?S ?\C-S) org-attach-unset-directory
         "Unset the attachment directory for this entry.  Removes DIR property.")
        ((?q) (lambda () (interactive) (message "Abort")) "Abort.")))

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
   (concat
    "Attachment folder:\n\n"
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

;;;; images

(setopt org-startup-with-inline-images t
        org-cycle-inline-images-display t
        org-image-actual-width '(300))

;;;; org-cliplink

(setup org-cliplink
  (:install t)
  (setopt org-cliplink-max-length nil
          org-cliplink-ellipsis "…")
  (with-eval-after-load 'org-keys
    (:with-keymap org-mode-map
      (:bind [remap org-insert-link] 'helheim-org-insert-link))))

;; Based on https://xenodium.com/emacs-dwim-do-what-i-mean/
(defun helheim-org-insert-link ()
  "Like `org-insert-link' but with some \"do what i mean\" behavior.
- If URL is in clipboard — use it.
- If selection is active — use it as link description.
- Automatically fetch URL title from its HTML tag.
- Fallback to `org-insert-link'."
  (interactive)
  (let ((point-at-link (org-in-regexp org-link-any-re 1))
        (clipboard-url (if (string-match-p "^http" (current-kill 0))
                           (current-kill 0)))
        (region-content (if (region-active-p)
                            (buffer-substring-no-properties (region-beginning)
                                                            (region-end)))))
    (cond ((and region-content clipboard-url (not point-at-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-at-link))
           (org-cliplink))
          (t
           (call-interactively 'org-insert-link))))
  (hel-extend-selection -1))

;;; .
(provide 'helheim-org)
;;; helheim-org.el ends here
