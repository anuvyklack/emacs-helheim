;;; helheim-dired-keys.el         -*- lexical-binding: t; no-byte-compile: t -*-
;;; Dired

(require 'dired)
(require 'dired-x) ;; Load because it unconditionally binds "F" and "V" keys.

;; All original keys are overriden!
(setup dired
  (:keymap dired-mode-map
    (:unbind
      ;; Upper case keys
      "A" "E" "G" "H" "Q" "T" "R" "Y" "W"
      ;; "I"   'dired-do-info
      ;; "N"   'dired-do-man
      ;; Lower case keys
      "b" "g" "w")
    (:bind
      "i"   'dired-toggle-read-only ;; wdired
      "v"   'helheim-dired-toggle-selection
      ;; `search-map'
      "C-c s f"   'fd-dired
      "C-c s n"   'find-name-dired
      "C-c s g"   'fd-grep-dired
      "C-c s /"   'dired-do-find-regexp
      "C-c s r"   'dired-do-find-regexp-and-replace ; overrides `query-replace'
      ;; git
      "C-c v i"   'magit-init
      "C-c v s"   'magit-dired-stage
      "C-c v u"   'magit-dired-unstage
      ;; "C-c v f" is bound to `magit-file-dispatch' transient menu which has
      ;; sence only in a file visiting buffer, so use this prefix to preserve
      ;; muscle memory.
      "C-c v f s" 'magit-dired-stage
      "C-c v f u" 'magit-dired-unstage
      ;; moving
      "h"   'dired-up-directory
      "j"   'helheim-dired-next-line
      "k"   'helheim-dired-previous-line
      "l"   'dired-find-file
      "C-j" 'dired-next-marked-file ;; "M-}"
      "C-k" 'dired-prev-marked-file ;; "M-{"
      "/"   'dired-goto-file        ;; "j"
      "<"   'dired-prev-dirline
      ">"   'dired-next-dirline
      ;; File
      "e"   'dired-do-open ;; "e" for external
      "o"   'dired-find-file-other-window
      "g o" 'dired-find-file-other-window ;; go to other window
      "C-o" 'dired-display-file ;; show in other window
      "O"   'dired-do-find-marked-files
      "a"   'dired-find-alternate-file
      "C"   'dired-do-copy
      "M"   'dired-do-rename ;; move
      "B"   'dired-do-byte-compile
      "L"   'dired-do-load
      "P"   'dired-do-print
      "V"   'dired-view-file ;; originally `dired-do-run-mail'
      "Z"   'dired-do-compress
      ", z" 'dired-do-compress-to
      ;; Change file attributes
      "c"   (define-keymap
              "p" '("change permissions" . dired-do-chmod)
              "o" '("change owner"       . dired-do-chown)
              "g" '("change group"       . dired-do-chgrp)
              "t" '("update timestamp"   . dired-do-touch)
              "i" '("add ID" . helheim-dired-do-add-id))
      ;; Delete
      "d"   'dired-flag-file-deletion
      "D"   'dired-do-delete ;; delete marked (not flagged) files
      ", d" (cons "delete"
                  (define-keymap
                    "g" '("Delete garbage files" . dired-flag-garbage-files)
                    "r" '("Delete by regexp" . dired-flag-files-regexp)
                    "#" '("Delete autosave files" . dired-flag-auto-save-files)
                    "~" '("Delete backup files" . dired-flag-backup-files)
                    "." '("Clean directory" . dired-clean-directory)
                    "/" '("Clean directory" . dired-clean-directory)))
      "x"   'dired-do-flagged-delete
      "X"   '+dired-do-flagged-delete-permanently
      ;; New file or directory
      "n"   (define-keymap
              "f" '("Create file" . find-file)
              "d" '("Create directory" . dired-create-directory)
              "l" '("Create symlink" . dired-do-symlink)
              "L" '("Create relative symlink" . dired-do-relsymlink)
              "h" '("Create hardlink" . dired-do-hardlink))
      "+"   'dired-create-directory
      ;; Yank / Paste
      "p"   'dired-copy-paste-do-paste
      "y"   (define-keymap
              "y" '("Copy to kill-ring" . dired-copy-paste-do-copy)
              "d" '("Cut to kill-ring" . dired-copy-paste-do-cut)
              "n" '("Copy file name" . dired-copy-filename-as-kill)
              "p" '("Copy full path" . +dired-copy-file-path)
              "c" '("Copy" . dired-do-copy)
              "m" '("Move" . dired-do-rename))
      ;; Mark / Unmark
      "m"   'dired-mark
      "u"   'dired-unmark
      "DEL" 'dired-unmark-backward  ;; <backspace>
      "U"   'dired-unmark-all-marks ;; remove all marks from all files
      "~"   'dired-toggle-marks     ;; reverse marks
      "*"   'helheim-dired-mark-map
      ", m" '("mark" . helheim-dired-mark-map)
      "M-DEL" 'dired-unmark-all-files
      ;; regexp commands
      "r"   (define-keymap
              "/" 'dired-mark-files-containing-regexp
              "d" 'dired-flag-files-regexp
              "m" 'dired-do-rename-regexp ;; move files
              "c" 'dired-do-copy-regexp
              "l" 'dired-do-symlink-regexp
              "L" 'dired-do-relsymlink-regexp
              "h" 'dired-do-hardlink-regexp)
      ;; Sort
      "s"   'dired-sort-toggle-or-edit
      "S"   'casual-dired-sort-by-tmenu
      ;; View manipulation
      "K"   'dired-do-kill-lines
      "("   'dired-hide-details-mode
      ")"   'dired-omit-mode
      "z i" 'dired-hide-details-mode
      "z ." 'dired-omit-mode
      ;; The bindings follow a convention where the filters are mapped
      ;; on lower-case letters or punctuation, operators are mapped on
      ;; symbols (such as "!", "|","*" etc.) and group commands are mapped
      ;; on upper-case letters. The exception to this is "p" which is bound
      ;; to `dired-filter-pop', which is a very common operation and warrants
      ;; a quick binding.
      "f"   'dired-filter-map
      "F"   'dired-filter-group-mode
      "C-c t f" 'dired-filter-group-mode ;; like in Ibuffer
      ;; dired narrow
      "z n" 'dired-narrow-fuzzy
      "z N" 'dired-narrow-regexp
      ;; "g" commands
      "g v" '("Restore marks" . dired-undo)
      "g a" 'dired-show-file-type
      "g r" 'dired-do-redisplay
      "g u" 'dired-downcase
      "g U" 'dired-upcase
      "g x" 'browse-url-of-dired-file
      ;; dired-subtree
      "<tab>"     'dired-subtree-toggle
      "<backtab>" 'dired-subtree-cycle
      "%"   'dired-subtree-mark-subtree
      "z j" 'dired-subtree-next-sibling
      "z k" 'dired-subtree-previous-sibling
      "z J" 'dired-subtree-end
      "z K" 'dired-subtree-beginning
      "z u" 'dired-subtree-up
      ;; "z n" 'dired-subtree-narrow
      ;; Subdirs
      ", s" (cons "subdir"
                  (define-keymap
                    "<tab>"     '("Cycle" . dired-hide-subdir)
                    "<backtab>" '("Cycle all" . dired-hide-all)
                    "%" '("Mark all" . dired-mark-subdir-files)
                    "i" '("Insert subdir" . dired-maybe-insert-subdir)
                    "d" '("Remove sudir" . dired-kill-subdir)
                    "l" '("goto subdir" . dired-tree-down)
                    "u" '("up subdir" . dired-tree-up)
                    "j" '("next subdir" . dired-next-subdir)
                    "k" '("prev subdir" . dired-prev-subdir)
                    "/" '("find subdir" . dired-goto-subdir)))
      "I"   'dired-maybe-insert-subdir
      ;; Comparison commands
      "="   'dired-diff
      ;; Execute external command
      "!"   'dired-do-shell-command
      "|"   'dired-do-shell-command
      "&"   'dired-do-async-shell-command
      ;; misc
      ", u" 'dired-undo ;; recover marks, killed lines or subdirs
      ", i" '("add ID" . helheim-dired-do-add-id)
      ", I" 'dired-do-info
      "?"   'casual-dired-tmenu
      "<remap> <vc-next-action>"   'dired-vc-next-action
      ;; encryption and decryption (epa-dired)
      ;; originally on  ":", but it is occupied by `execute-extended-command'
      ";"   (define-keymap
              "d" 'epa-dired-do-decrypt
              "v" 'epa-dired-do-verify
              "s" 'epa-dired-do-sign
              "e" 'epa-dired-do-encrypt)
      ;; thumbnail manipulation (image-dired)
      "t"   (define-keymap
              "RET" 'image-dired-show-all-from-dir
              "t"   'image-dired-display-thumbs ;; display thumbs for marked files
              "i"   'image-dired-dired-toggle-marked-thumbs
              "e"   'image-dired-dired-display-external
              "a"   'image-dired-display-thumbs-append)
      ;; "C-t" (define-keymap
      ;;         "d"   'image-dired-display-thumbs
      ;;         "t"   'image-dired-tag-files
      ;;         "r"   'image-dired-delete-tag
      ;;         "j"   'image-dired-jump-thumbnail-buffer
      ;;         "i"   'image-dired-dired-display-image
      ;;         "x"   'image-dired-dired-display-external
      ;;         "a"   'image-dired-display-thumbs-append
      ;;         "."   'image-dired-display-thumb
      ;;         "c"   'image-dired-dired-comment-files
      ;;         "f"   'image-dired-mark-tagged-files
      ;;         "C-t" 'image-dired-dired-toggle-marked-thumbs
      ;;         "e"   'image-dired-dired-edit-comment-and-tags)
      ;; ;; isearch
      ;; "C-c s a C-s"   'dired-do-isearch
      ;; "C-c s a C-M-s" 'dired-do-isearch-regexp
      ;; "C-c s f C-s"   'dired-isearch-filenames
      ;; "C-c s f C-M-s" 'dired-isearch-filenames-regexp
      )))

(defvar-keymap helheim-dired-mark-map
  :prefix 'helheim-dired-mark-map
  "c" '("Change marks" . dired-change-marks)
  "t" '("Toggle marks" . dired-toggle-marks)
  "u" '("Remove specific mark" . dired-unmark-all-files)
  "N" '("Number of marked files" . dired-number-of-marked-files)
  ;;
  "." '("Mark by extension" . dired-filter-mark-by-extension)
  ;; "." '("Mark by extension" . dired-mark-extension)
  "x" '("Mark executables" . dired-filter-mark-by-executable)
  "*" '("Mark executables" . dired-mark-executables)
  "n" '("Mark by name" . dired-filter-mark-by-name)
  "r" '("Mark by regex" . dired-mark-files-regexp)
  ;; "r" '("Mark by regex" . dired-filter-mark-by-regexp)
  "d" '("Mark directories" . dired-filter-mark-by-directory)
  "/" '("Mark directories" . dired-mark-directories)
  "f" '("Mark files" . dired-filter-mark-by-file)
  "l" '("Mark symlinks" . dired-filter-mark-by-symlink)
  "@" '("Mark symlinks" . dired-mark-symlinks)
  "h" '("Mark dot-files" . dired-filter-mark-by-dot-files) ;; hidden-files
  "i" '("Mark git-ignored" . dired-filter-mark-by-git-ignored)
  "e" '("Mark by predicate" . dired-filter-mark-by-predicate)
  "(" '("Mark by sexp" . dired-mark-sexp)
  "m" '("Mark by major-mode" . dired-filter-mark-by-mode)
  "RET" '("Saved filters" . dired-filter-mark-by-saved-filters)
  ;; "o" 'dired-filter-mark-by-omit
  ;; "g" 'dired-filter-mark-by-garbage
  )

;;; image-dired

(setup image-dired
  (:after-load
    (:keymap image-dired-thumbnail-mode-map
      (:unbind "w")
      (:bind
        "n"   'image-dired-display-next
        "p"   'image-dired-display-previous
        "C-l" 'image-dired-display-next
        "C-h" 'image-dired-display-previous
        "y"   'image-dired-copy-filename-as-kill ;; "w"
        "e"   'image-dired-thumbnail-display-external
        "o"   'image-dired-thumbnail-display-external))))

;;; .
(provide 'helheim-dired '(keys))
;;; helheim-dired-keys.el ends here
