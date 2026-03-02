;;; helheim-disable-isearch.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; This module unbinds Isearch keys. Isearch doesn't play well with multiple
;; cursors and `consult-line' is better anyway.
;;
;;; Code:
(require 'hel-core)

(hel-keymap-global-set
  "C-s"   nil  ; `isearch-forward'
  "C-M-s" nil  ; `isearch-forward-regexp'
  "C-r"   nil  ; `isearch-backward'
  "C-M-r" nil) ; `isearch-backward-regexp'
(hel-keymap-set search-map
  "w"     nil  ; `isearch-forward-word'
  "_"     nil  ; `isearch-forward-symbol'
  "."     nil  ; `isearch-forward-symbol-at-point'
  "M-."   nil) ; `isearch-forward-thing-at-point'
(hel-keymap-set help-map
  "C-s"   nil) ; `search-forward-help-for-help'

;; After deleting "M-." from `search-map' there remain an empty keymap:
;; `(27 keymap)' which blocks access to "g" and "m" keys from `hel-leader'.
;; 27 is ASCII code for ESC. This is about how Emacs works: key sequences
;; starts with ESC are accessible via Meta key.
(cl-callf2 assq-delete-all 27 search-map)

(with-eval-after-load 'embark
  (hel-keymap-set embark-general-map
    "C-s" nil   ; `embark-isearch-forward'
    "C-r" nil)  ; `embark-isearch-backward'
  (hel-keymap-set embark-collect-mode-map
    "s"   nil)) ; `isearch-forward'

(provide 'helheim-disable-isearch)
;;; helheim-disable-isearch.el ends here
