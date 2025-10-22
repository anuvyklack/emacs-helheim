;;; helheim-keybindings.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; The idea of this file is to store all keybindings in one place. It relies on
;; `featurep' and enables keybindings conditionally, so you should `require' it
;; last.
;;
;;; Code:
(elpaca imenu-list)

(helix-keymap-global-set :state 'insert
  "C-/" 'hippie-expand)

(helix-keymap-global-set :state 'normal
  "z SPC" 'cycle-spacing
  "z ."   'set-fill-prefix)

(helix-keymap-global-set
  "C-x C-b" 'ibuffer-jump ; override `list-buffers'
  "C-x C-r" 'recentf-open ; override `find-file-read-only'
  "C-x C-d" 'dired-jump)  ; override `list-directory'

;;; <leader>

(helix-keymap-set mode-specific-map
  "RET" 'bookmark-jump
  "," 'switch-to-buffer
  "/" 'consult-ripgrep ; "/" is bound to search in Helix
  "d" 'dired-jump
  "b" (cons "buffer"
            (define-keymap
              "b" 'ibuffer-jump        ; "<leader> bb"
              "n" 'switch-to-buffer    ; next key after "b"
              "s" 'save-buffer
              "w" 'write-file
              "d" 'kill-current-buffer ; also "C-w d"
              "z" 'bury-buffer         ; also "C-w z"
              "g" 'revert-buffer       ; also "C-w r"
              "r" 'rename-buffer
              "x" 'scratch-buffer
              ;; Bookmarks
              "m" 'bookmark-set
              "M" 'bookmark-delete))
  "f" (cons "file/find"
            (define-keymap
              ;; "x" 'xref-find-apropos
              "b" 'switch-to-buffer
              "f" 'find-file
              "F" 'consult-find
              "d" 'dired
              "l" 'locate
              "r" '("Recent files" . recentf-open)
              "w" 'write-file))
  "o" (cons "open"
            (define-keymap
              "t" 'treemacs
              "i" 'imenu-list-smart-toggle))
  "s" (cons "search"  search-map)
  "p" (cons "project" project-prefix-map)
  "v" (cons "version control" vc-prefix-map))

;; <leader> s
(helix-keymap-set search-map
  "i" 'imenu)

;;; Man

(with-eval-after-load 'man
  ;; You can also enable `outline-minor-mode' in a Man buffer, so the keys
  ;; should possibly not interfere with it.
  (helix-keymap-set Man-mode-map :state 'normal
    "z h"   'Man-next-manpage     ; left
    "z l"   'Man-previous-manpage ; right
    "z j"   'Man-next-section     ; up
    "z k"   'Man-previous-section ; down
    "z /"   'Man-goto-section     ; Relative to sections thats why on "z" layer.
    "g r"   'Man-follow-manual-reference ; go to reference
    "C-w r" 'Man-update-manpage)) ; Standard chord for revert.

(provide 'helheim-keybindings)
;;; helheim-keybindings.el ends here

