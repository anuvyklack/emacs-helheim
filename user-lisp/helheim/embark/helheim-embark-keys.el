;;; helheim-embark-keys.el -*- lexical-binding: t; no-byte-compile: t; -*-
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

(require 'embark)

(hel-keymap-set embark-general-map
  :unset '("w" "DEL")
  "E"   'embark-export
  "S"   'embark-collect ; for "snapshot"
  "L"   'embark-live
  "B"   'embark-become
  "A"   'embark-act-all
  "SPC" 'embark-select
  "i"   'embark-insert
  "y"   'embark-copy-as-kill ; "w"
  "q"   'embark-toggle-quit)

(hel-keymap-set embark-region-map
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
  "w"   'whitespace-cleanup-region ; "F"
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
  ">"   'embark-encode-map
  "u"    nil  ; `upcase-region'
  "l"    nil  ; `downcase-region'
  "t"    nil  ; `transpose-regions'
  ";"    nil  ; `comment-or-uncomment-region'
  "+"    nil  ; `append-to-file'
  "n"    nil) ; `hel-narrow-to-region-indirectly'

(hel-keymap-set embark-kill-ring-map
  :unset "\\"
  "d"   'embark-kill-ring-remove) ; "\"

(hel-keymap-set embark-buffer-map
  :unset '("k" "K")
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
  "$"   'eshell
  "b"    nil  ; `switch-to-buffer'
  "z"    nil) ; `embark-bury-buffer'

(hel-keymap-set embark-tab-map
  :unset '("k" "s")
  "RET" 'tab-bar-select-tab-by-name ; "s"
  "r"   'tab-bar-rename-tab-by-name
  "d"   'tab-bar-close-tab-by-name) ; "k"

(hel-keymap-set embark-identifier-map
  "RET" 'xref-find-definitions
  "h"   'display-local-help
  "H"   'embark-toggle-highlight
  "d"   'xref-find-definitions
  "r"   'xref-find-references
  "a"   'xref-find-apropos
  "i"   'info-lookup-symbol ; "s"
  "'"   'expand-abbrev
  "$"   'ispell-word
  "o"   'occur
  ;; Use `hel-paredit' instead.
  "n"    nil  ; `embark-next-symbol'
  "p"    nil) ; `embark-previous-symbol'

(hel-keymap-set embark-symbol-map
  ;; inherit from `embark-identifier-map'
  "RET" 'embark-find-definition
  "h"   'describe-symbol
  "i"   'embark-info-lookup-symbol ; "s"
  "d"   'embark-find-definition
  "e"   'pp-eval-expression
  "a"   'apropos
  "\\"  'embark-history-remove)

(hel-keymap-set embark-expression-map
  :unset "k"
  "RET" 'pp-eval-expression
  "e"   'pp-eval-expression
  "<"   'embark-eval-replace
  "m"   'pp-macroexpand-expression
  "TAB" 'indent-region
  ";"   'comment-dwim
  "t"   'transpose-sexps
  "d"   'kill-region ; "k"
  ;; Use `hel-paredit' instead.
  "r"    nil  ; `raise-sexp'
  "u"    nil  ; `backward-up-list'
  "n"    nil  ; `forward-list'
  "p"    nil) ; `backward-list'

(hel-keymap-set embark-defun-map
  ;; inherit from `embark-expression-map'
  "RET" 'embark-pp-eval-defun
  "e"   'embark-pp-eval-defun
  "c"   'compile-defun
  "D"   'edebug-defun
  "o"   'checkdoc-defun
  "N"    nil) ; `narrow-to-defun'

(hel-keymap-set embark-heading-map
  :unset '("C-SPC" "n" "p" "f" "b" "^" "v" "+" "-")
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
  "M-j" 'outline-move-subtree-up          ; "^"
  "M-j" 'outline-move-subtree-down        ; "v"
  ">"   'outline-demote
  "<"   'outline-promote)

(hel-keymap-set embark-flymake-map
  :unset '("n" "p")
  "RET" 'flymake-show-buffer-diagnostics
  "j"   'flymake-goto-next-error  ; "n"
  "k"   'flymake-goto-prev-error) ; "p"

(hel-keymap-set embark-unicode-name-map
  :unset '("I" "W")
  "RET" 'insert-char
  "Y"   'embark-save-unicode-character) ; "W"

(hel-keymap-set embark-prose-map
  :unset "F"
  "$"   'ispell-region
  "f"   'fill-region
  "c"   'capitalize-region
  "w"   'whitespace-cleanup-region ; "F"
  "="   'count-words-region
  "u"    nil  ; `upcase-region'
  "l"    nil) ; `downcase-region'

(hel-keymap-set embark-sentence-map
  ;; inherit from `embark-prose-map'
  "t"   'transpose-sentences
  ")"   'forward-sentence
  "("   'backward-sentence)

;; Keymap for *Embark Collect* buffer.
(hel-keymap-set embark-collect-mode-map
  ;; `m' and `u' are used for selecting and unselecting in Dired like buffers.
  "m"   'hel-embark-select
  "u"   'hel-embark-select
  "y"   'embark-copy-as-kill)

(hel-keymap-set embark-consult-rerun-map
  :unset "g"
  "g r" 'embark-rerun-collect-or-export)

;;; Commands

(defun hel-embark-select ()
  "Add or remove the target from the current buffer's selection.
You can act on all selected targets at once with `embark-act-all'.
When called from outside `embark-act' this command will select
the first target at point."
  (interactive)
  (embark-select)
  (next-line))

;;; .
(provide 'helheim-embark '(keys))
;;; helheim-embark-keys.el ends here

