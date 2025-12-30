;;; helheim-keybindings.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; If you want to see all key bindings in a keymap, place point (cursor) on it
;; and press "M", or press "<F1> M" and type the name of the keymap.
;;
;;; General

(setup hel-leader
  (:install hel-leader :host github :repo "anuvyklack/hel-leader")
  (:require t))

(setup helheim
  (:global-bind :state 'normal
    "z SPC" 'cycle-spacing
    "z ."   'set-fill-prefix)
  (:global-bind :state 'insert
    "C-/"   'hippie-expand)
  (:global-bind
    [remap keyboard-quit] 'helheim-keyboard-quit ; "C-g"
    ;;
    "C-x C-b" 'ibuffer-jump  ;; override `list-buffers'
    "C-x C-r" 'recentf-open  ;; override `find-file-read-only'
    "C-x C-d" 'dired-jump))  ;; override `list-directory'

;;; <Leader>

(hel-keymap-set (helheim-leader-map)
  "RET" 'dired-jump
  "," 'switch-to-buffer
  "/" 'consult-ripgrep ; "/" is for search in Hel
  "d" 'dired-jump
  "b" (cons "buffer"
            (hel-keymap-set (helheim-leader-prefix-map "b")
              "b" 'ibuffer-jump        ;; "<leader> bb"
              "n" 'switch-to-buffer    ;; next key after "b"
              "s" 'save-buffer
              "w" 'write-file
              "d" 'kill-current-buffer ;; also "C-w d"
              "z" 'bury-buffer         ;; also "C-w z"
              "g" 'revert-buffer       ;; also "C-w r"
              "r" 'rename-buffer
              "x" 'scratch-buffer
              ;; Bookmarks
              "RET" 'bookmark-jump
              "m" 'bookmark-set
              "M" 'bookmark-delete))
  "f" (cons "file"
            (hel-keymap-set (helheim-leader-prefix-map "f")
              "b" 'switch-to-buffer
              "f" 'find-file  ; select file in current dir or create new one
              "/" 'consult-fd ; or `consult-find'
              "d" 'dired
              "l" 'locate
              "r" '("Recent files" . recentf-open)
              "w" 'write-file))
  "o" (cons "open"
            (hel-keymap-set (helheim-leader-prefix-map "o")
              "f" 'treemacs ; from future
              "i" 'imenu-list-smart-toggle))
  "p" (cons "project"
            (hel-keymap-set project-prefix-map
              "RET" 'project-dired
              ","   'project-switch-to-buffer
              "/"   'project-find-regexp
              "B"   'project-list-buffers))
  "t" (cons "toggle"
            (hel-keymap-set (helheim-leader-prefix-map "t")
              "v" 'visual-line-mode
              "w" '+wrap-line-mode
              "r" '("Read only" . read-only-mode) ;; "C-x C-q"
              "$" 'set-selective-display)) ;; "C-x $"
  "s" (cons "search"
            (hel-keymap-set search-map
              "a" 'xref-find-apropos
              "r" 'query-replace
              "R" 'query-replace-regexp))
  "v" (cons "version control" (helheim-leader-prefix-map "v")))

;;; Button

(hel-keymap-set button-buffer-map
  "C-j" 'forward-button
  "C-k" 'backward-button)

;;; Customize

(setup cus-edit
  (hel-set-initial-state 'Custom-mode 'normal)
  (:after-load
    (:with-keymap custom-mode-map
      (:bind :state 'normal
        "z j" 'widget-forward
        "z k" 'widget-backward
        "z u" 'Custom-goto-parent
        "q"   'Custom-buffer-done))))

;;; Elpaca
;; `elpaca-manager' and `elpaca-log' are the main entry points to the UI.

(hel-set-initial-state 'elpaca-info-mode 'normal)

;;; Help

;; <F1>
(setup help
  (:with-keymap help-map
    (:unbind
      ;; Unclatter help-map to make `whick-key' useable.
      "<f1>" "C-h" "?" "<help>" ;; `help-for-help'
      "C-e"  ;; `view-external-packages' — irrelevant or outdated information
      "C-o"  ;; `describe-distribution'
      "C-q"  ;; `help-quick-toggle' — irrelevant to us
      "C-w"  ;; `describe-no-warranty'
      "R"    ;; `info-display-manual' — moved to "RET"
      "C-n") ;; `view-emacs-news' — duplicated on "n"
    (:bind
      "m"   'describe-mode
      "C-d" 'view-emacs-debugging
      "F"   'describe-face
      "M"   'describe-keymap
      "o"   'describe-syntax ;; swap "s" and "o"
      "s"   'helpful-symbol
      "S"   'info-lookup-symbol
      "RET" 'info-display-manual ;; originally `view-order-manuals'
      "d"   'apropos-documentation
      "l"   'view-lossage
      ;; Rebind `b' key from `describe-bindings' to prefix with more binding
      ;; related commands.
      "b" (cons "bindings"
                (define-keymap
                  "b" 'describe-bindings
                  "B" 'embark-bindings ;; alternative for `describe-bindings'
                  "i" 'which-key-show-minor-mode-keymap
                  "m" 'which-key-show-major-mode
                  "t" 'which-key-show-top-level
                  "f" 'which-key-show-full-keymap
                  "k" 'which-key-show-keymap)))))

;;; Info

(setup info
  (hel-set-initial-state 'Info-mode 'normal)
  (hel-advice-add 'Info-next-reference :before #'hel-deactivate-mark-a)
  (hel-advice-add 'Info-prev-reference :before #'hel-deactivate-mark-a)
  (:after-load
    (:with-keymap Info-mode-map
      (:bind :state 'normal
        "C-j"   'Info-next
        "C-k"   'Info-prev
        "z j"   'Info-forward-node
        "z k"   'Info-backward-node
        "z u"   'Info-up
        "z d"   'Info-directory
        "z ~"   'Info-directory ;; "~" is for "home"
        ;;
        "z h"   'Info-history
        "u"     'Info-history-back
        "U"     'Info-history-forward
        "C-<i>" 'Info-history-forward
        "C-o"   'Info-history-back
        ;;
        "g t"   'Info-toc
        "g i"   'Info-index ;; imenu
        "g I"   'Info-virtual-index
        "g m"   'Info-menu
        ;;
        "z i"   'Info-index
        "z I"   'Info-virtual-index
        "C-c s a" 'info-apropos
        ;;
        "M-h"   'Info-help))))

;;; Man

(setup man
  (hel-set-initial-state 'Man-mode 'normal)
  (add-hook 'Man-mode-hook
            (defun helheim-man-mode-h ()
              (setq-local revert-buffer-function (lambda (&rest _)
                                                   (Man-update-manpage)))))
  (:after-load
    ;; User may also enable `outline-minor-mode' in a Man buffer, so the keys
    ;; should possibly not interfere with it.
    (:with-keymap Man-mode-map
      (:bind :state 'normal
        "] ]" 'Man-next-manpage
        "[ [" 'Man-previous-manpage
        "z h" 'Man-next-manpage     ;; left
        "z l" 'Man-previous-manpage ;; right
        "z j" 'Man-next-section     ;; up
        "z k" 'Man-previous-section ;; down
        "z /" 'Man-goto-section ;; On "z" layer becacuse related to sections.
        "g r" 'Man-follow-manual-reference)))) ; go to reference

;;; Magit-section

(setup magit-section
  (:after-load
    (:with-keymap magit-section-mode-map
      (:unbind "M-1" "M-2" "M-3" "M-4" "C-c TAB" "C-<tab>" "M-<tab>")
      (:bind
        "<tab>"     'magit-section-cycle
        "<backtab>" 'magit-section-cycle-global ;; "S-<tab>"
        "C-j" 'magit-section-forward-sibling
        "C-k" 'magit-section-backward-sibling
        "z j" 'magit-section-forward
        "z k" 'magit-section-backward
        "z u" 'magit-section-up
        "z a" 'magit-section-toggle
        "z c" 'magit-section-hide
        "z o" 'magit-section-show
        "z O" 'magit-section-show-children
        "z m" 'magit-section-show-level-1-all
        "z r" 'magit-section-show-level-4-all
        "1"   'magit-section-show-level-1
        "2"   'magit-section-show-level-2
        "3"   'magit-section-show-level-3
        "4"   'magit-section-show-level-4
        "z 1" 'magit-section-show-level-1-all
        "z 2" 'magit-section-show-level-2-all
        "z 3" 'magit-section-show-level-3-all
        "z 4" 'magit-section-show-level-4-all))))

;;; Repeat mode

(put 'other-window 'repeat-map nil) ;; Use "." key instead.

;;; .
(provide 'helheim-keybindings)
;;; helheim-keybindings.el ends here
