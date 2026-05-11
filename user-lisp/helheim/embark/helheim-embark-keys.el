;;; helheim-embark-keys.el        -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;
;; Original Embark keybindings are tailored for Emacs.
;; Lets reconfigure them for Hel.
;;
;; Embark Keymap Hierarchy
;; -----------------------
;; - embark-meta-map
;;   + embark-general-map
;;     + embark-region-map
;;     + embark-file-map
;;     + embark-kill-ring-map
;;     + embark-url-map
;;     + embark-email-map
;;     + embark-library-map
;;     + embark-buffer-map
;;     + embark-tab-map
;;     + embark-identifier-map
;;       + embark-symbol-map
;;         + embark-face-map
;;         + embark-variable-map
;;         + embark-function-map
;;           + embark-command-map
;;     + embark-expression-map
;;       + embark-defun-map
;;     + embark-heading-map
;;     + embark-package-map
;;     + embark-bookmark-map
;;     + embark-flymake-map
;;     + embark-unicode-name-map
;;     + embark-prose-map
;;         + embark-sentence-map
;;         + embark-paragraph-map
;;   + embark-become-help-map
;;   + embark-become-file+buffer-map
;;   + embark-become-shell-command-map
;;   + embark-become-match-map
;;
;; Embark Org extension
;; --------------------
;; - embark-meta-map
;;   - embark-general-map
;;     - embark-heading-map
;;       - embark-org-heading-map
;;     - embark-org-table-cell-map
;;     - embark-org-table-map
;;     - embark-org-link-map
;;     - embark-org-src-block-map
;;     - embark-org-inline-src-block-map
;;     - embark-org-babel-call-map
;;     - embark-org-item-map
;;     - embark-org-plain-list-map
;;     - embark-org-export-in-place-map
;; - embark-org-link-copy-map

;;; Code:

(setup embark
  (:require t)
  (:with-keymap embark-general-map
    (:unbind "w" "DEL")
    (:bind
      "E"   'embark-export
      "S"   'embark-collect ;; for "snapshot"
      "L"   'embark-live
      "B"   'embark-become
      "A"   'embark-act-all
      "SPC" 'embark-select
      "i"   'embark-insert
      "y"   'embark-copy-as-kill ;; "w"
      "q"   'embark-toggle-quit))

  (:with-keymap embark-region-map
    (:unbind
      "u"  ;; `upcase-region'   - "gU" in Hel
      "l"  ;; `downcase-region' - "gu" in Hel
      "t"  ;; `transpose-regions' - very obscure command
      ";"  ;; `comment-or-uncomment-region' - "gc" in hel
      "+"  ;; `append-to-file' - legacy from a bygone era
      "n"  ;; `narrow-to-region' - "zn" in Hel
      "F") ;; `whitespace-cleanup-region' - moved to "w"
    (:bind
      "<left>"  'indent-rigidly
      "<right>" 'indent-rigidly
      "TAB" 'indent-region
      "c"   'capitalize-region
      "|"   'shell-command-on-region
      "e"   'eval-region
      "<"   'embark-eval-replace
      "a"   'align
      "A"   'align-regexp
      "f"   'fill-region
      "p"   'fill-region-as-paragraph
      "$"   'ispell-region
      "="   'count-words-region
      "w"   'whitespace-cleanup-region  ; "F"
      "o"   'org-table-convert-region
      "W"   'write-region
      ;; "k"   'apply-macro-to-region-lines ; "k" for keyboard macro
      "m"   'apply-macro-to-region-lines
      "*"   'calc-grab-region
      ":"   'calc-grab-sum-down
      "_"   'calc-grab-sum-across
      "r"   'reverse-region
      "d"   'delete-duplicate-lines
      "b"   'browse-url-of-region
      "h"   'shr-render-region
      "'"   'expand-region-abbrevs
      "v"   'vc-region-history
      "R"   'repunctuate-sentences
      "s"   'embark-sort-map
      ">"   'embark-encode-map))

  (:with-keymap embark-kill-ring-map
    (:unbind "\\")
    (:bind "d" 'embark-kill-ring-remove)) ;; "\"

  (:with-keymap embark-buffer-map
    (:unbind
      "k" "K"
      "b"  ;; `switch-to-buffer'
      "z") ;; `embark-bury-buffer'
    (:bind
      "RET" 'switch-to-buffer
      "d"   'kill-buffer                   ; "k"
      "D"   'embark-kill-buffer-and-window ; "K
      "o"   'switch-to-buffer-other-window
      "r"   'embark-rename-buffer
      "="   'ediff-buffers
      "|"   'embark-shell-command-on-buffer
      "<"   'insert-buffer
      "x"   'embark-open-externally
      "j"   'embark-dired-jump
      "$"   'eshell))

  (:with-keymap embark-tab-map
    (:unbind "k" "s")
    (:bind
      "RET" 'tab-bar-select-tab-by-name ; "s"
      "r"   'tab-bar-rename-tab-by-name
      "d"   'tab-bar-close-tab-by-name)) ; "k"

  (:with-keymap embark-identifier-map
    (:unbind
      ;; Use `hel-paredit' instead.
      "n"  ;; `embark-next-symbol'
      "p") ;; `embark-previous-symbol'
    (:bind
      "RET" 'xref-find-definitions
      "h"   'display-local-help
      "H"   'embark-toggle-highlight
      "d"   'xref-find-definitions
      "r"   'xref-find-references
      "a"   'xref-find-apropos
      "i"   'info-lookup-symbol ;; "s"
      "'"   'expand-abbrev
      "$"   'ispell-word
      "o"   'occur))

  ;; inherit from `embark-identifier-map'
  (:with-keymap embark-symbol-map
    (:bind
      "RET" 'embark-find-definition
      "h"   'describe-symbol
      "i"   'embark-info-lookup-symbol ;; "s"
      "d"   'embark-find-definition
      "e"   'pp-eval-expression
      "a"   'apropos
      "\\"  'embark-history-remove))

  (:with-keymap embark-expression-map
    (:unbind "k"
      ;; Use `hel-paredit' instead of following commands
      "r"  ;; `raise-sexp'
      "u"  ;; `backward-up-list'
      "n"  ;; `forward-list'
      "p") ;; `backward-list'
    (:bind
      "RET" 'pp-eval-expression
      "e"   'pp-eval-expression
      "<"   'embark-eval-replace
      "m"   'pp-macroexpand-expression
      "TAB" 'indent-region
      ";"   'comment-dwim
      "t"   'transpose-sexps
      "d"   'kill-region)) ;; "k"

  ;; inherit from `embark-expression-map'
  (:with-keymap embark-defun-map
    (:unbind "N") ;; `narrow-to-defun'
    (:bind
      "RET" 'embark-pp-eval-defun
      "e"   'embark-pp-eval-defun
      "c"   'compile-defun
      "D"   'edebug-defun
      "o"   'checkdoc-defun))

  (:with-keymap embark-heading-map
    (:unbind "C-SPC" "n" "p" "f" "b" "^" "v" "+" "-")
    (:bind
      "RET" 'outline-show-subtree
      "TAB" 'outline-cycle
      "o"   'outline-show-subtree             ; "+"
      "c"   'outline-hide-subtree             ; "-"
      "m"   'outline-mark-subtree             ; "C-SPC"
      "j"   'outline-next-visible-heading     ; "n"
      "k"   'outline-previous-visible-heading ; "p"
      "J"   'outline-forward-same-level       ; "f"
      "K"   'outline-backward-same-level      ; "b"
      "u"   'outline-up-heading
      "M-j" 'outline-move-subtree-up    ; "^"
      "M-j" 'outline-move-subtree-down  ; "v"
      ">"   'outline-demote
      "<"   'outline-promote))

  (:with-keymap embark-flymake-map
    (:unbind "n" "p")
    (:bind
      "RET" 'flymake-show-buffer-diagnostics
      "j"   'flymake-goto-next-error   ;; "n"
      "k"   'flymake-goto-prev-error)) ;; "p"

  (:with-keymap embark-unicode-name-map
    (:unbind "I" "W")
    (:bind
      "RET" 'insert-char
      "Y"   'embark-save-unicode-character)) ;; "W"

  (:with-keymap embark-prose-map
    (:unbind
      "F"  ;; `whitespace-cleanup-region'
      "u"  ;; `upcase-region'
      "l") ;; `downcase-region'
    (:bind
      "$"   'ispell-region
      "f"   'fill-region
      "c"   'capitalize-region
      "w"   'whitespace-cleanup-region ;; "F"
      "="   'count-words-region))

  (:with-keymap embark-sentence-map
    (:bind
      ;; inherit from `embark-prose-map'
      "t"   'transpose-sentences
      ")"   'forward-sentence
      "("   'backward-sentence))

  ;; Keymap for *Embark Collect* buffer.
  (:with-keymap embark-collect-mode-map
    (:bind
      ;; `m' and `u' are used for selecting and unselecting in Dired like buffers.
      "m"   'hel-embark-select
      "u"   'hel-embark-select
      "y"   'embark-copy-as-kill)))

(setup embark-consult
  (:after-load
    (:with-keymap embark-consult-rerun-map
      (:unbind "g")
      (:bind "g r" 'embark-rerun-collect-or-export))))

(setup embark-org
  (:after-load
    (:with-keymap embark-org-table-cell-map
      (:unbind
        ;; Use M-j / M-k instead
        "^"  ;; `org-table-move-row-up'
        "v"  ;; `org-table-move-row-down'
        ;; Use M-h / M-l instead
        "<"  ;; `org-table-move-column-left'
        ">"  ;; `org-table-move-column-right'
        "e"  ;; `org-table-edit-field' -- moved to ' to be aligned with z'
        "h") ;; `org-table-insert-hline' -- replaced with "J" / "K" keys
      (:bind
        "'" 'org-table-edit-field
        "v" 'org-table-insert-column
        "j" '+org-table-insert-row-below
        "k" '+org-table-insert-row-above
        "J" '+org-table-insert-hline-below
        "K" '+org-table-insert-hline-above))
    ;;
    (:with-keymap embark-org-table-map
      (:unbind
        "y") ;; moved to "p"
      (:bind
        "p" 'org-table-paste-rectangle))))

;;; .
(provide 'helheim-embark '(keys))
;;; helheim-embark-keys.el ends here
