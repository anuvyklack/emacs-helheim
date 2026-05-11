;;; helheim-disable-isearch.el    -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;
;; This module unbinds Isearch keys. Isearch doesn't play well with multiple
;; cursors and `consult-line' is better anyway.
;;
;;; Code:

(setup isearch
  (:with-keymap global-map
    (:unbind
      "C-s"     ;; `isearch-forward'
      "C-M-s"   ;; `isearch-forward-regexp'
      "C-r"     ;; `isearch-backward'
      "C-M-r")) ;; `isearch-backward-regexp'
  (:with-keymap search-map
    (:unbind
      "w"       ;; `isearch-forward-word'
      "_"       ;; `isearch-forward-symbol'
      "."       ;; `isearch-forward-symbol-at-point'
      "M-."))   ;; `isearch-forward-thing-at-point'
  (:with-keymap help-map
    (:unbind "C-s")) ;; `search-forward-help-for-help'
  ;; After deleting "M-." from `search-map' there remain an empty keymap:
  ;; `(27 keymap)' which blocks access to "g" and "m" keys from `hel-leader'.
  ;; 27 is ASCII code for ESC. This is about how Emacs works: key sequences
  ;; starts with ESC are accessible via Meta key.
  (cl-callf2 assq-delete-all 27 search-map))

(setup embark
  (:after-load
    (:with-keymap embark-general-map
      (:unbind
        "C-s"   ;; `embark-isearch-forward'
        "C-r")) ;; `embark-isearch-backward'
    (:with-keymap embark-collect-mode-map
      (:unbind "s")))) ;; `isearch-forward'

(provide 'helheim-disable-isearch)
;;; helheim-disable-isearch.el ends here
