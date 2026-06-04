;;; helheim-keybindings.el        -*- lexical-binding: t; no-byte-compile: t -*-
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

(which-key-add-key-based-replacements
  "C-c a"  "ai"
  "C-c b"  "buffer / bookmark"
  "C-c f"  "file"
  "C-c o"  "open"
  "C-c p"  "project"
  "C-c t"  "toggle"
  "C-c s"  "search"
  "C-c v"  "version control")

(hel-keymap-set mode-specific-map
  "RET"   'dired-jump
  ","     'switch-to-buffer
  "/"     'consult-ripgrep ;; "/" is for search in Hel
  "d"     'dired-jump
  ;; Buffer
  "b b"   '("ibuffer" . ibuffer-jump) ;; "<leader> bb"
  "b n"   'switch-to-buffer    ; next key after "b"
  "b s"   'save-buffer
  "b c"   '("copy buffer's file" . bufferfile-copy)
  "b w"   '("write buffer for file" . write-file)
  "b d"   '("delete buffer's file" . bufferfile-delete)
  "b g"   'revert-buffer       ; also "C-w r"
  "b r"   '("rename buffer's file" . bufferfile-rename)
  "b R"   'rename-buffer
  "b x"   'scratch-buffer
  "b z"   'bury-buffer         ; also "C-w z"
  ;; Bookmarks
  "b RET" 'bookmark-jump
  "b m"   'bookmark-set
  "b M"   'bookmark-delete
  ;; File
  "f b"   'switch-to-buffer
  "f f"   'find-file  ;; select file in current dir or create new one
  "f /"   'consult-fd ;; or `consult-find'
  "f d"   'dired
  "f l"   'locate
  "f r"   '("Recent files" . recentf-open)
  "f w"   'write-file
  ;; Open
  ;; "o f"   'treemacs ; for future
  ;; "o i"   'imenu-list-smart-toggle
  ;; Project
  "p"     (hel-keymap-set project-prefix-map
            "RET" 'project-dired
            ","   'project-switch-to-buffer
            "/"   'project-find-regexp
            "B"   'project-list-buffers)
  ;; Toggle
  "t d"   'display-line-numbers-mode
  "t r"   'read-only-mode        ;; "C-x C-q"
  "t v"   'visual-line-mode
  "t w"   '+wrap-line-mode
  "t $"   'set-selective-display ;; "C-x $"
  ;; Search
  "s"     (hel-keymap-set search-map
            "a" 'xref-find-apropos
            "r" 'query-replace
            "R" 'query-replace-regexp))

;;; Buffer

(setup bufferfile
  (:install t)
  (:autoload bufferfile-copy)
  (:setopt bufferfile-verbose t
           bufferfile-use-vc t
           bufferfile-delete-switch-to 'parent-directory))

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
    ;; Unclatter `help-map' to make `whick-key' useable.
    (:unbind
      "<f1>" "C-h" "?" "<help>" ;; `help-for-help'
      "h"    ;; `view-hello-file'
      "C-c"  ;; `describe-copying'
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
      (:bind
        "C-c s a" 'info-apropos)
      (:bind :state 'motion
        "i"     'hel-normal-state)
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
        ;;
        "M-h"   'Info-help
        ;; <local-leader>
        "," (define-keymap
              "t" 'Info-toc
              "i" 'Info-index
              "I" 'Info-virtual-index
              "m" 'Info-menu
              "h" 'Info-history
              "u" 'Info-history-back
              "U" 'Info-history-forward)))))

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

;;; tab-bar

(setup tab-bar
  (:with-keymap tab-bar-history-mode-map
    (:unbind
      "C-c <left>"
      "C-c <right>")))

;;; .
(provide 'helheim-keybindings)
;;; helheim-keybindings.el ends here
