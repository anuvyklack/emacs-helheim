;;; helheim-org.el -*- lexical-binding: t; no-byte-compile: t -*-
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
        ;; <local-leader>
        "," (define-keymap
              "RET" 'org-ctrl-c-ret ;; also on "z RET"
              "'"   'org-edit-special
              ","   'org-priority
              "/"   'org-sparse-tree
              "#"   'org-update-statistics-cookies
              "a"   'org-attach
              "i"   (cons "insert" 'helheim-org-insert-map)
              "l"   (cons "links" 'helheim-org-link-map)
              "o"   'org-open-at-point
              "p"   'yank-media
              "t"   'org-todo)))
    ;; <leader>
    (:with-keymap (helheim-leader-map org-mode-map)
      (:unbind
        "'"  ;; `org-edit-special' — moved to ,' and z'
        ","  ;; `org-priority'     — moved to ,' and z,
        "/") ;; `org-sparse-tree'  — moved to ,' and z/
      (:bind
        "RET" 'dired-jump ;; rebind `org-ctrl-c-ret' which is moved to ", RET"
        "a"  'org-attach
        "i"  (cons "insert" 'helheim-org-insert-map)
        "l"  (cons "links" 'helheim-org-link-map)
        "t"  (cons "toggle"
                   (define-keymap
                     "i" '("Inline images" . org-toggle-inline-images)
                     "l" '("Links display" . org-toggle-link-display)
                     "f" '("Table formula debugger" . org-table-toggle-formula-debugger)
                     "o" '("Table coordinate overlays" . org-table-toggle-coordinate-overlays)))))
    ;; <leader> l or <local-leader> l
    (defvar-keymap helheim-org-link-map
      :prefix 'helheim-org-link-map
      "l" '("Insert link" . org-insert-link)
      "i" '("Insert last stored link" . org-insert-last-stored-link) ;; "i" for insert
      "s" '("Store link" . org-store-link)
      "a" '("Insert all links" . org-insert-all-links)
      "u" '("Update id locations" . org-id-update-id-locations))
    ;; <leader> i or <local-leader> i
    (defvar-keymap helheim-org-insert-map
      :prefix 'helheim-org-insert-map
      "l" '("Link" . org-insert-link)
      "m" 'yank-media
      "d" '("Deadline" . org-deadline)
      "s" '("Schedule" . org-schedule)
      "t" '("Time stamp" . org-time-stamp)
      "T" '("Time stamp inactive" . org-time-stamp-inactive)
      "Q" '("Set tags" . org-set-tags-command))))

(setup dired
  (:after-load
    (:with-keymap (helheim-leader-map dired-mode-map)
      (:bind "a" 'org-attach-dired-to-subtree))))

;;; Config

(setup org
  (:install t)
  ;; (:built-in)
  (setopt org-insert-heading-respect-content nil
          org-M-RET-may-split-line '((default . t)
                                     (item . nil))
          org-return-follows-link t
          org-special-ctrl-a/e t
          org-pretty-entities t)
  (:after-load
    (load "helheim-org-lib" nil t)))

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
         (if value
             (let ((face (make-face 'helheim-org-heading-padding-face)))
               (set-face-attribute face nil :height value)
               (add-hook 'org-font-lock-set-keywords-hook
                         'helheim-org--add-padding-to-headings))
           ;; else
           (remove-hook 'org-font-lock-set-keywords-hook
                        'helheim-org--add-padding-to-headings))))

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

;;; .
(provide 'helheim-org)
;;; helheim-org.el ends here
