;;; helheim-org-keys.el           -*- lexical-binding: t; no-byte-compile: t -*-
;;; Leader keys

(setup org-keys
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
    ;; "C-c n t"  'org-todo-list
    )
  ;; Keys active in org-mode buffers
  (:after-load
    (:keymap org-mode-map
      (:bind :state normal
        "z '"  'org-edit-special
        "z ,"  'org-insert-structure-template
        "z `"  'org-table-edit-field
        "z /"  'org-sparse-tree
        ;; "z n"  'org-narrow-to-subtree
        "g p"  'consult-org-heading
        "g x"  'org-open-at-point
        "g n"  'org-next-link
        "g N"  'org-previous-link
        "] l"  'org-next-link
        "[ l"  'org-previous-link
        ;; <local-leader>
        ", RET" 'org-ctrl-c-ret ;; also on "z RET"
        ", |"   'org-table-create-or-convert-from-region
        ", '"   'org-edit-special
        ", ."   'org-edit-special
        ", ,"   'org-insert-structure-template
        ", /"   'org-sparse-tree
        ", #"   'org-update-statistics-cookies
        ", a"   'org-attach
        ", e"   'org-export-dispatch
        ;; ", o"   'org-open-at-point
        ", t"   '("rotate todo state" . org-todo)
        ", y"   'org-copy-visible
        ;; "v"
        ", l"   '("links" . helheim-org-link-map)
        ", i"   '("insert" . helheim-org-insert-map)
        ", n"   (cons "node"
                      (define-keymap
                        "a"  'org-toggle-archive-tag ;; C-c C-x a
                        "c"  'org-clone-subtree-with-time-shift))
        ", p"   'org-paste-special
        ", x"   (cons "other"
                      (define-keymap
                        "<"  'org-agenda-set-restriction-lock       ;; C-c C-x <
                        ">"  'org-agenda-remove-restriction-lock))) ;; C-c C-x >
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
        "C-c '"   ;; `org-edit-special' — moved to ,' and z'
        "C-c ,"   ;; `org-priority'     — moved to ,i,
        "C-c /")) ;; `org-sparse-tree'  — moved to ,/ and z/
    ;;
    (:keymap org-link-navigation-repeat-map
      (:bind
        "n" 'org-next-link
        "p"  nil
        "N" 'org-previous-link
        "]" 'org-next-link
        "[" 'org-previous-link))))

(defvar-keymap helheim-org-link-map
  :prefix 'helheim-org-link-map
  "l"  '("insert link" . org-insert-link)
  "i"  '("insert last stored link" . org-insert-last-stored-link) ;; "i" for insert
  "s"  '("store link" . org-store-link)
  "a"  '("insert all links" . org-insert-all-links)
  "u"  '("update id locations" . org-id-update-id-locations))

(defvar-keymap helheim-org-insert-map
  :prefix 'helheim-org-insert-map
  ","  'org-priority
  "e"  'org-set-effort
  "l"  '("insert link" . org-insert-link)
  "n"  'org-add-note
  "m"  '("paste media" . yank-media)
  "p"  'org-set-property
  ;; timestamp
  "t"  '("time stamp" . org-time-stamp)
  "T"  '("inactive time stamp" . org-time-stamp-inactive)
  "d"  '("deadline" . org-deadline)
  "s"  '("schedule" . org-schedule)
  ;; tags
  "q"  '("set tags" . org-set-tags-command))

(setup dired
  (:after-load
    (:keymap dired-mode-map
      (:bind
        ", a" '("org-attach file to node" . org-attach-dired-to-subtree)))))

;;; Agenda

(setup org-agenda
  (hel-set-initial-state 'org-agenda-mode 'emacs)
  (:after-load
    (:keymap org-agenda-mode-map
      (:unbind
        "," ;; org-priority — moved to ",,"
        "g" ;; org-agenda-redo-all — moved to "C-w r"
        ;; next / previous
        "n" "p"
        ;; view
        "d" "w" "m" "y")
      (:bind
        ;; "j"   'org-agenda-goto-date
        ;; "k"   'org-agenda-capture
        "j"   'org-agenda-next-line
        "k"   'org-agenda-previous-line

        "c"   'org-agenda-capture       ;; `org-agenda-goto-calendar'
        "C"   'org-agenda-goto-calendar ;; `org-agenda-convert-date'

        "SPC" 'org-agenda-show-and-scroll-up ;; or 'org-agenda-show
        "s"   'org-save-all-org-buffers

        ;; Quit / refresh
        "q"   'org-agenda-quit
        "x"   'org-agenda-exit

        ;; Date navigation
        "l"   'org-agenda-later
        "h"   'org-agenda-earlier
        "."   'org-agenda-goto-today
        ;; "v d" 'org-agenda-day-view
        ;; "v w" 'org-agenda-week-view
        ;; "v m" 'org-agenda-month-view
        ;; "v y" 'org-agenda-year-view

        ", ," 'org-agenda-priority
        ", l" 'org-agenda-log-mode

        ;; Entry actions
        "t"   'org-agenda-todo
        "s"   'org-agenda-schedule
        "T"   'org-agenda-set-tags
        "p"   'org-agenda-priority
        "r"   'org-agenda-refile
        "A"   'org-agenda-archive
        "D"   'org-agenda-deadline
        "n"   'org-agenda-add-note
        ", n" 'org-agenda-add-note

        ;; ;; Filtering
        ;; "f" 'org-agenda-filter-by-tag
        ;; "F" 'org-agenda-filter-remove-all

        ;; Clocking
        "I"   'org-agenda-clock-in
        "O"   'org-agenda-clock-out
        "X"   'org-agenda-clock-cancel
        "g J" 'org-agenda-clock-goto
        "g r" 'org-agenda-redo

        ;; Bulk actions
        "m"  'org-agenda-bulk-mark
        "u"  'org-agenda-bulk-unmark
        "B"  'org-agenda-bulk-action
        ", RET" 'org-agenda-bulk-action
        )
      )))

;;; .
(provide 'helheim-org-keys)
;;; helheim-org-keys.el ends here
