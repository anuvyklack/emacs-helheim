;;; helheim-org-keys.el           -*- lexical-binding: t; no-byte-compile: t -*-

(setup org
  (which-key-add-key-based-replacements
    "C-c n"  "notes")
  ;; Keys available everywhere
  (:global-bind
    "C-c n a"  'org-agenda
    "C-c n c"  'org-capture
    ;; "C-c n C"  'org-capture-goto-target ; TODO: autoload
    ;; "C-c n j"  (cons "journal"
    ;;                  (define-keymap
    ;;                    "j" 'org-journal-new-entry
    ;;                    "J" 'org-journal-new-scheduled-entry
    ;;                    "s" 'org-journal-search-forever))
    "C-c n S"  'org-store-link
    "C-c n t"  'org-todo-list)
  ;; Keys active in org-mode buffers
  (with-eval-after-load 'org-keys
    ;; `org-mode-map'
    (:bind :state 'normal
      "z '"  'org-edit-special
      "z ,"  'org-insert-structure-template
      "z /"  'org-sparse-tree
      ;; "z n"  'org-narrow-to-subtree
      "g p"  'consult-org-heading
      "g x"  'org-open-at-point
      "g n"  'org-next-link
      "g N"  'org-previous-link
      "] l"  'org-next-link
      "[ l"  'org-previous-link
      ;; <local-leader>
      ","    (define-keymap
               "RET" 'org-ctrl-c-ret ;; also on "z RET"
               "'"   'org-edit-special
               ","   'org-priority
               "/"   'org-sparse-tree
               "#"   'org-update-statistics-cookies
               "a"   'org-attach
               "e"   'org-export-dispatch
               "i"   '("insert" . helheim-org-insert-map)
               "l"   '("links" . helheim-org-link-map)
               "o"   'org-open-at-point
               "p"   'yank-media
               "t"   'org-todo))
    ;; <leader>
    (:bind
      "C-c RET"  'dired-jump ;; rebind `org-ctrl-c-ret' which is moved to ", RET"
      "C-c i"    '("insert" . helheim-org-insert-map)
      "C-c l"    '("links"  . helheim-org-link-map)
      ;; Toggle
      "C-c t i"  'org-link-preview
      "C-c t l"  '("show links targets" . org-toggle-link-display)
      "C-c t f"  '("table formula debugger" . org-table-toggle-formula-debugger)
      "C-c t o"  '("table coordinate overlays" . org-table-toggle-coordinate-overlays))
    (:unbind
      "C-c '"    ;; `org-edit-special' — moved to ,' and z'
      "C-c ,"    ;; `org-priority'     — moved to ,,
      "C-c /"))) ;; `org-sparse-tree'  — moved to ,/ and z/

;; <leader> l or <local-leader> l
(defvar-keymap helheim-org-link-map
  :prefix 'helheim-org-link-map
  "l"  '("insert link" . org-insert-link)
  "i"  '("insert last stored link" . org-insert-last-stored-link) ;; "i" for insert
  "s"  '("store link" . org-store-link)
  "a"  '("insert all links" . org-insert-all-links)
  "u"  '("update id locations" . org-id-update-id-locations))

;; <leader> i or <local-leader> i
(defvar-keymap helheim-org-insert-map
  :prefix 'helheim-org-insert-map
  "l"  '("insert link" . org-insert-link)
  "m"  'yank-media
  "d"  '("deadline" . org-deadline)
  "s"  '("schedule" . org-schedule)
  "t"  '("time stamp" . org-time-stamp)
  "T"  '("inactive time stamp" . org-time-stamp-inactive)
  "Q"  '("set tags" . org-set-tags-command))

(setup dired
  (:after-load
    ;; `dired-mode-map'
    (:bind
      ", a" '("org-attach file to node" . org-attach-dired-to-subtree))))

;;; .
(provide 'helheim-org-keys)
;;; helheim-org-keys.el ends here
