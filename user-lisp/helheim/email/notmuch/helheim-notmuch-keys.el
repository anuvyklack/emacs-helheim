;;; helheim-notmuch-keys.el       -*- lexical-binding: t; no-byte-compile: t -*-

(setup notmuch
  (hel-set-initial-state 'notmuch-hello-mode  'emacs)
  (add-hook 'notmuch-hello-mode-hook #'hel-switch-to-normal-state-in-field-widget)

  (hel-set-initial-state 'notmuch-search-mode 'emacs)
  (hel-set-initial-state 'notmuch-tree-mode   'emacs)
  (hel-set-initial-state 'notmuch-show-mode   'emacs)

  (setq notmuch-tagging-keys
        `(("a" notmuch-archive-tags "Archive")
          ("u" notmuch-show-mark-read-tags "Mark as read")
          ("f" ("+flagged") "Flagged")
          ("d" ,(notmuch-tag-change-list
                 (-concat +notmuch-delete-tags notmuch-archive-tags))
           "Delete")
          ("t" ,(notmuch-tag-change-list
                 (-concat +notmuch-delete-tags notmuch-archive-tags))
           "Trash")
          ("s" ,(notmuch-tag-change-list
                 (-concat +notmuch-spam-tags notmuch-archive-tags))
           "Spam")))

  ;; For `notmuch-tag-jump'
  (setq notmuch-tag-jump-reverse-key "m")

  (set-keymap-parent notmuch-common-keymap special-mode-map)
  (:keymap notmuch-common-keymap
    (:unbind "g" "G" "j" "z" "?")
    (:bind
      "C-c RET" '("Sync email" . notmuch-poll-and-refresh-this-buffer) ;; leader
      ", RET"   '("Sync email" . notmuch-poll-and-refresh-this-buffer) ;; localleader
      "%"     'hel-mark-whole-buffer
      "C-o"   'hel-backward-mark-ring
      "C-<i>" 'hel-forward-mark-ring
      ;;
      "q"     'notmuch-bury-or-kill-this-buffer
      "s"     'notmuch-search
      "t"     'notmuch-search-by-tag
      "S"     'notmuch-search
      "T"     'notmuch-tree
      "U"     'notmuch-unthreaded
      "Z"     'notmuch-tree
      ;;
      "u"     'notmuch-tag-undo
      "x"     'notmuch-refresh-this-buffer
      "c"     '("Compose mail" . notmuch-mua-new-mail)
      ", s"   'notmuch-search
      ", t"   'notmuch-search-by-tag
      ", u"   'notmuch-unthreaded
      ", z"   'notmuch-tree
      ;; "c c"   'compose-mail
      ;; "c C"   'compose-mail-other-frame
      ", c"   '("Compose mail" . notmuch-mua-new-mail)
      ", v"   'notmuch-version
      ;;
      "="     'notmuch-refresh-this-buffer
      "M-="   'notmuch-refresh-all-buffers))

  (:keymap notmuch-hello-mode-map
    (:bind
      "m"     'notmuch-jump-search)) ;; "m" for jump menu

  (:keymap notmuch-search-mode-map
    (:unbind "SPC" "b" "l")
    (:bind
      "RET"   'notmuch-search-show-thread
      "M-RET" 'notmuch-tree-from-search-thread
      ;;
      "j"     'helheim-notmuch-search-next-line
      "k"     'helheim-notmuch-search-previous-line
      "v"     'helheim-notmuch-toggle-selection
      ;;
      "] ]"   'notmuch-search-next-thread
      "[ ["   'notmuch-search-previous-thread
      ;; "n"     'notmuch-search-next-thread
      ;; "p"     'notmuch-search-previous-thread
      "g g"   'notmuch-search-first-thread
      "G"     'notmuch-search-last-thread
      ;;
      "s"     '("Edit search" . notmuch-search-edit-search)
      "e"     '("Edit search" . notmuch-search-edit-search)
      "f"     '("Filter results" . notmuch-search-filter)
      "t"     '("Filter by tag" . notmuch-search-filter-by-tag)
      "o"     '("Inverse order" . notmuch-search-toggle-order)
      "i"     '("Show ignored" . notmuch-search-toggle-hide-excluded) ;; "i" for ignore
      "T"     'notmuch-tree-from-search-current-query
      "U"     'notmuch-unthreaded-from-search-current-query
      "Z"     'notmuch-tree-from-search-current-query
      ;;
      "m"     '("Mark with tag" . notmuch-tag-jump) ;; "m" for mark
      "*"     '("Tag all" . notmuch-search-tag-all)
      "-"     '("Remove tag" . notmuch-search-remove-tag)
      "+"     '("Add tag" . notmuch-search-add-tag)
      "a"     '("Archive" . notmuch-search-archive-thread)
      "d"     '("Delete" .  +notmuch-search-delete)
      "!"     '+notmuch-search-flagged
      "x"     'notmuch-refresh-this-buffer ;; apply tag changes like in Dired
      ;;
      "y"     '("yank" . notmuch-search-stash-map) ;; by "stash" Notmuch means copy
      "c"     '("Compose mail" . notmuch-mua-new-mail)
      "r"     '("Reply" . notmuch-search-reply-to-thread-sender)
      "R"     '("Reply all" . notmuch-search-reply-to-thread)
      ", r"   '("Reply" . notmuch-search-reply-to-thread-sender)
      ", R"   '("Reply all" . notmuch-search-reply-to-thread)))

  (:keymap notmuch-tree-mode-map
    (:unbind "z")
    (:bind
      "RET"   'notmuch-tree-show-message
      ;;
      "j"     'helheim-notmuch-tree-next-line
      "k"     'helheim-notmuch-tree-previous-line
      "C-j"   'notmuch-tree-next-matching-message
      "C-k"   'notmuch-tree-prev-matching-message
      "g j"   'notmuch-tree-next-matching-message
      "g k"   'notmuch-tree-prev-matching-message
      "z j"   'notmuch-tree-next-message
      "z k"   'notmuch-tree-prev-message
      "] ]"   'notmuch-tree-next-thread
      "[ ["   'notmuch-tree-prev-thread
      "g g"   'notmuch-search-first-thread
      "G"     'notmuch-search-last-thread
      ;;
      "o"     '("Inverse order" . notmuch-tree-toggle-order)
      "i"     '("Show ignored" . notmuch-tree-toggle-hide-excluded) ;; "i" for ignore
      "S"     'notmuch-search-from-tree-current-query
      "T"     'notmuch-tree-from-unthreaded-current-query
      "U"     'notmuch-unthreaded-from-tree-current-query
      "Z"     'notmuch-tree-from-unthreaded-current-query
      ;;
      ;; Access notmuch-show functions directly from notmuch-tree buffer
      "|"     'notmuch-show-pipe-message
      "y"     '("yank" . notmuch-show-stash-map) ;; by "stash" Notmuch means copy
      "v"     'notmuch-show-view-all-mime-parts
      "w"     'notmuch-show-save-attachments ;; "w" for write
      ", w"   '("Save attachments" . notmuch-show-save-attachments)
      "b"     'notmuch-show-resend-message
      ;;
      "a"     '("Archive" . notmuch-tree-archive-message-then-next)
      "A"     '("Archive thread" . notmuch-tree-archive-thread-then-next)
      "d"     '("Delete" . +notmuch-tree-delete)
      "D"     '("Delete thread" . +notmuch-tree-delete-thread)
      "m"     '("Mark with tag" . notmuch-tag-jump) ;; "m" for mark
      "-"     '("Remove tag" . notmuch-tree-remove-tag)
      "+"     '("Add tag" . notmuch-tree-add-tag)
      "*"     '("Tag thread" . notmuch-tree-tag-thread)
      "x"     'notmuch-refresh-this-buffer ;; apply tag changes like in Dired
      ;;
      "e"     'notmuch-tree-edit-search
      "f"     'notmuch-tree-filter
      "t"     'notmuch-tree-filter-by-tag
      "r"     '("Reply" . notmuch-tree-reply-sender)
      "R"     '("Reply all" . notmuch-tree-reply)
      "V"     'notmuch-tree-view-raw-message
      ", f"   '("Forward email" . notmuch-tree-forward-message)
      ", r"   '("Reply" . notmuch-tree-reply-sender)
      ", R"   '("Reply all" . notmuch-tree-reply)
      "s"     'notmuch-tree-to-tree
      "E"     'notmuch-tree-resume-message))

  (:keymap notmuch-show-mode-map
    (:unbind "h" "j" "k" "l" "c" "t")
    (:bind
      ;; "M-RET" 'notmuch-show-open-or-close-all
      "RET"   'helheim-notmuch-show-RET
      "C-j"   'helheim-notmuch-next-button-or-open-message
      "C-k"   'helheim-notmuch-previous-button-or-open-message
      ;; "C-j" 'notmuch-show-next-open-message
      ;; "C-k" 'notmuch-show-previous-open-message
      "z j"   '("Next message" . notmuch-show-next-message)
      "z k"   '("Previous message" . notmuch-show-previous-message)
      "z c"   '("Fold message" . +notmuch-show-fold-message)
      "z o"   '("Open message" . +notmuch-show-unfold-message)
      "z r"   '("Open all" . +notmuch-show-open-all-messages)
      "z m"   '("Close all" . +notmuch-show-close-all-messages)
      "] ]"   '("Notmuch next thread" . notmuch-show-next-thread-show)
      "[ ["   '("Notmuch prev thread" . notmuch-show-previous-thread-show)
      "g x"   'goto-address-at-point
      ;;
      "n"     'notmuch-show-next-open-message
      "p"     'notmuch-show-previous-open-message
      "N"     'notmuch-show-next-message
      "P"     'notmuch-show-previous-message
      "M-n"   'notmuch-show-next-thread-show
      "M-p"   'notmuch-show-previous-thread-show
      "C-c C-c" 'notmuch-show-advance-and-archive ;; <leader><leader>
      "DEL"   'notmuch-show-rewind                  ;; <backspace>
      ;;
      "a"     '("Archive" . notmuch-show-archive-message)
      "A"     '("Archive thread" . notmuch-show-archive-thread)
      "d"     '("Delete" . +notmuch-show-delete)
      "D"     '("Delete thread" . +notmuch-show-delete-thread)
      "m"     '("Mark with tag" . notmuch-tag-jump) ;; "m" for mark
      "*"     'notmuch-show-tag-all
      "-"     'notmuch-show-remove-tag
      "+"     'notmuch-show-add-tag
      "x"     'notmuch-refresh-this-buffer ;; apply tag changes like in Dired
      ;;
      "T"     'notmuch-tree-from-show-current-query
      "U"     'notmuch-unthreaded-from-show-current-query
      "Z"     'notmuch-tree-from-show-current-query
      ;; Use forwarding to share information with others; use resending to fix
      ;; delivery issues or resend to the original recipient.
      "f"     '("Forward message" . notmuch-show-forward-message)
      "F"     '("Forward open messages" . notmuch-show-forward-open-messages)
      "b"     'notmuch-show-resend-message
      "r"     '("Reply" . notmuch-show-reply-sender)
      "R"     '("Reply all" . notmuch-show-reply)
      "L"     'notmuch-show-filter-thread
      "|"     'notmuch-show-pipe-message
      "w"     'notmuch-show-save-attachments ;; "w" for write
      "v"     'notmuch-show-view-part
      "V"     'notmuch-show-view-raw-message
      "E"     'notmuch-show-resume-message
      "y"     '("copy" . notmuch-show-stash-map)
      "H"     '("Headers visibility" . notmuch-show-toggle-visibility-headers)
      "!"     'notmuch-show-toggle-elide-non-matching
      "$"     'notmuch-show-toggle-process-crypto
      "."     'notmuch-show-part-map
      "#"     'notmuch-show-print-message
      "<"     'notmuch-show-toggle-thread-indentation
      ">"     'notmuch-show-toggle-thread-indentation
      ", e"   '("Resume message" . notmuch-show-resume-message)
      ", f"   '("Forward message" . notmuch-show-forward-message)
      ", F"   '("Forward open messages" . notmuch-show-forward-open-messages)
      ", b"   'notmuch-show-resend-message
      ", w"   '("Save attachments" . notmuch-show-save-attachments)
      ", r"   '("Reply" . notmuch-show-reply-sender)
      ", R"   '("Reply all" . notmuch-show-reply)
      ", p"   '("Print" . notmuch-show-print-message)
      ", l"   '("URLs" . notmuch-show-browse-urls)
      ", V"   '("View raw message" . notmuch-show-view-raw-message)
      ", %"   '("Choose duplicate" . notmuch-show-choose-duplicate)
      "C-c t h" '("Notmuch headers visibility" . notmuch-show-toggle-visibility-headers)
      "C-c t i" '("Notmuch thread indentation" . notmuch-show-toggle-thread-indentation)))

  (:keymap notmuch-show-stash-map
    (:unbind "?")
    (:bind
      "c"     '("Copy CC field" . notmuch-show-stash-cc)
      "d"     '("Copy date" . notmuch-show-stash-date)
      "F"     '("Copy filename" . notmuch-show-stash-filename)
      "f"     '("Copy from" . notmuch-show-stash-from)
      "i"     '("Copy ID" . notmuch-show-stash-message-id)
      "I"     '("Copy ID stripped" . notmuch-show-stash-message-id-stripped)
      "s"     '("Copy subject" . notmuch-show-stash-subject)
      "T"     '("Copy tags" . notmuch-show-stash-tags)
      "t"     '("Copy to" . notmuch-show-stash-to)
      "l"     'notmuch-show-stash-mlarchive-link
      "L"     'notmuch-show-stash-mlarchive-link-and-go
      "G"     'notmuch-show-stash-git-send-email))

  (:keymap notmuch-show-part-map
    (:unbind "?")
    (:bind
      "s"     '("Save part" . notmuch-show-save-part)
      "v"     '("View part" . notmuch-show-view-part)
      "o"     '("View part interactively" . notmuch-show-interactively-view-part)
      "|"     '("Pipe part" . notmuch-show-pipe-part)
      "m"     '("Choose mime of part" . notmuch-show-choose-mime-of-part))))

;;; .
(provide 'helheim-notmuch '(keys))
;;; helheim-notmuch-keys.el ends here
