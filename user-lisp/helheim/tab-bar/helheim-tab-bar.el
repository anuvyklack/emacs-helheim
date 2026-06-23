;;; helheim-tab-bar.el            -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;
;; Each tab represents a set of windows (like in Vim).
;;
;;; Keybindings

;; "C-w"
(hel-keymap-set hel-window-map
  "<tab>"   'helheim-tab-new
  "C-<tab>" 'other-tab-prefix
  "m"       'other-tab-prefix       ; next to "n" which is `other-window-prefix'
  "t" (cons "tab-bar"
            (define-keymap
              ;; "t" helheim-tab-new
              "t" 'tab-duplicate
              "d" 'tab-duplicate
              "n" 'other-tab-prefix
              "g" 'tab-group         ; Add current tab to group.
              ">" 'tab-bar-move-tab
              "<" 'tab-bar-move-tab-backward
              "r" 'tab-rename
              "u" 'tab-undo          ; Restore last closed tab.
              "c" 'tab-close
              "o" 'tab-close-other   ; Close all other tabs.
              "w" 'tab-window-detach ; Move current window to new tab.
              "F" 'tab-detach)))     ; Move current tab to new frame.

(with-eval-after-load 'tab-bar
  (hel-keymap-set tab-bar-mode-map :state '(normal emacs)
    ;; "C-<tab>" and "C-S-<tab>" are bound by deafult.
    "] t" 'tab-next
    "[ t" 'tab-previous)
  ;; Repeat mode
  (setq tab-bar-switch-repeat-map
        (define-keymap
          "]" 'tab-next
          "[" 'tab-previous))
  (setq tab-bar-move-repeat-map
        (define-keymap
          ">" 'tab-bar-move-tab
          "<" 'tab-bar-move-tab-backward)))

;;; Config

(setup tab-bar
  (:setopt tab-bar-format '(tab-bar-format-history
                            tab-bar-format-tabs-groups
                            tab-bar-separator
                            tab-bar-format-add-tab
                            tab-bar-format-align-right
                            tab-bar-format-global)
           ;; tab-bar-new-tab-choice "*dashboard*" ;; Buffer to show in new tab.
           tab-bar-tab-hints nil ;; Show tab numbers.
           tab-bar-close-button-show nil
           ;; - 1 -- Hide tab bar if only 1 tabs open.
           ;; - t -- Always show tab bar.
           tab-bar-show t
           tab-bar-history-limit 20)
  (tab-bar-mode)
  (tab-bar-history-mode))

(defun helheim-tab-new (arg)
  "Create new tab. With \\[universal-argument] detach current window into new tab."
  (interactive "P")
  (if arg (tab-window-detach) (tab-new)))

;;; .
(provide 'helheim-tab-bar)
;;; helheim-tab-bar.el ends here
