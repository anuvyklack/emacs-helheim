;;; helheim-core.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; Customization

(defgroup helheim nil
  "Helheim specific settings."
  :prefix 'helheim-)

(defcustom helheim-package-manager 'straight
  "Which package manager Helheim will use."
  :type '(radio (symbol :tag "elpaca"      'elpaca)
                (symbol :tag "straight.el" 'straight))
  :group 'helheim)

(defcustom helheim-id-format "%Y%m%dT%H%M%S"
  "The format string of timebased ID as `format-time-string' accepts."
  :type 'string
  :group 'helheim)

(defcustom helheim-file-id-regexp "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)--"
  "File name ID regexp."
  :type 'string
  :group 'helheim)

;;; Package management

(pcase helheim-package-manager
  ('straight (require 'helheim-straight))
  ('elpaca   (require 'helheim-elpaca)))
(require 'dash)
(require 'f)
(require 'helheim-lib)
(require 'helheim-setup)

;; Compiles .el files before they are loaded with `load' or `require'.
(setup compile-angel
  (:install t)
  (:require t)
  (:setopt compile-angel-verbose t)
  (cl-callf nconc compile-angel-excluded-files
    '("/init.el"
      "/custom.el"))
  (blackout 'compile-angel-on-load-mode)
  (compile-angel-on-load-mode))

;; We disabled byte-compilation by setting `user-lisp-auto-scrape' to nil
;; in early-init.el file because it happens too early before dependencies
;; are installed, and with this we also disabled autoload cookies scrapping.
;; So do it manually here.
(loaddefs-generate (f-directories user-lisp-directory nil t)
                   (expand-file-name ".user-lisp-autoloads.el"
                                     user-lisp-directory))

;;; External dependencies

(setup pcre2el (:install t))
(setup wgrep   (:install t))

(setup avy
  (:install t)
  (:setopt avy-keys '( ?a ?s ?d ?f ?g ?h ?j ?k ?l    ;; middle layer
                       ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ;; top layer
                       ?z ?x ?c ?v ?b ?n ?m)         ;; bottom layer
           avy-style 'at-full
           avy-all-windows nil
           avy-all-windows-alt t
           avy-background t
           ;; The unpredictability of such behavior (when enabled) makes it
           ;; a poor default.
           avy-single-candidate-jump nil))

(setup hel
  (:install hel :host github :repo "anuvyklack/hel")
  (hel-mode))

(setup compat
  (:install t))

(setup transient
  (:install t)
  (:setopt transient-common-command-prefix "SPC"
           ;; Pop up transient windows at the bottom of the current window
           ;; instead of entire frame. This is more ergonomic for users with
           ;; large displays or many splits.
           transient-display-buffer-action '(display-buffer-below-selected
                                             (dedicated . t)
                                             (inhibit-same-window . t))
           transient-show-during-minibuffer-read t
           ;; transient-default-level 5
           )
  (:after-load
    ;; Close transient menus with ESC.
    (keymap-set transient-map "<escape>" #'transient-quit-one)))

(setup nerd-icons
  (:install t)
  (:setopt nerd-icons-scale-factor 0.95)
  (:after-load
    ;; Add some icons
    (-each '((fundamental-mode nerd-icons-faicon "nf-fa-file_o" :face nerd-icons-dsilver)
             (deadgrep-mode nerd-icons-faicon "nf-fa-search"))
      (-lambda ((mode . icon-spec))
        (setf (alist-get mode nerd-icons-mode-icon-alist) icon-spec)))))

;; `+wrap-line-mode' dependencies: its file is autoloaded, so we should install
;; its dependences out of it.
(setup adaptive-wrap (:install t))
(setup visual-fill-column (:install t))

;; Required by:
;; - `markdown-mode', or it will install it via package.el if it isn't present
;;    on call of `markdown-edit-code-block'.
;; - `helheim-edit-indirect'
(setup edit-indirect
  (:install t)
  (:after-load
    ;; edit-indirect-mode-map
    (:bind :state 'normal
      "Z Z" 'edit-indirect-commit
      "Z Q" 'edit-indirect-abort)))

(elpaca-wait)

;;; UI
;;;; Misc

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim.
;; Any feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Accept shorter responses: "y" for yes and "n" for no.
(setq use-short-answers t
      read-answer-short 'auto)

(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Position underlines at the descent line instead of the baseline.
(setq x-underline-at-descent-line t)

(setq truncate-string-ellipsis "…")

;; No beeping or blinking
(setq visible-bell nil
      ring-bell-function #'ignore)

;; Disable truncation of printed s-expressions in the message buffer.
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;;;; Mouse

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Context menu on right mouse button click.
(when (and (display-graphic-p)
           (memq 'context-menu helheim-emacs-ui-elements))
  (add-hook 'after-init-hook #'context-menu-mode))

;;;; Cursor

;; I anable blinking cursor, but it may interferes with cursor settings in some
;; minor modes that try to change it buffer-locally (e.g., Treemacs).
(blink-cursor-mode)

;; Do not extend the cursor to fit wide characters.
(setq x-stretch-cursor nil)

;; Show cursor in non-selected window.
(setq-default cursor-in-non-selected-windows nil)

;; Show active region in non-selected window.
(setq highlight-nonselected-windows nil)

;;;; Highlight current line

;; `hl-line-mode' was obsoleted in Emacs 31 by the `window' value of
;; `global-hl-line-sticky-flag' that uses `pre-redisplay-functions'.
;; (bug#80007)

(when (< emacs-major-version 31)
  (defcustom global-hl-line-buffers nil
    "Whether the Global HL-Line mode should be enabled in a buffer.
The predicate is passed as argument to `buffer-match-p', which see."
    :type '(buffer-predicate :tag "Predicate for `buffer-match-p'"))
  ;;
  (advice-add 'global-hl-line-highlight :around
              #'helheim-global-hl-line-highlight-a))

(setup hl-line
  (:setopt global-hl-line-sticky-flag 'window ;; Emacs 31
           ;; I want current line highlighting only in special modes that are in
           ;; Hel motion state, and temporary disable it when region is active.
           ;; In text editing modes disable it, because it interferes with Hel
           ;; selections.
           global-hl-line-buffers '(and (or (derived-mode . special-mode)
                                            ;; For `define-compilation-mode'
                                            (derived-mode . compilation-mode)
                                            ;; Follow major modes doesn't inherit
                                            ;; from `special-mode'.
                                            (major-mode . dired-mode)
                                            (major-mode . notmuch-search-mode)
                                            (major-mode . notmuch-tree-mode)
                                            (major-mode . notmuch-show-mode))
                                        (not (major-mode . image-dired-thumbnail-mode))
                                        ;; According to my measures, compiled
                                        ;; function is two times faster.
                                        helheim--global-hl-line-buffers-p))
  (global-hl-line-mode))

;;;; Display line numbers

(setup display-line-numbers
  (:hook (prog-mode-hook conf-mode-hook))
  (:setopt display-line-numbers-width 3
           display-line-numbers-type t
           display-line-numbers-width-start t
           display-line-numbers-grow-only t
           ;; Show absolute line numbers for narrowed regions to make it easier
           ;; to tell the buffer is narrowed, and where you are, exactly.
           display-line-numbers-widen t))

;;;; Fill-Column indicator

(add-hook 'prog-mode-hook 'helheim-show-fill-column-indicator)

(defun helheim-show-trailing-whitespace ()
  "Highlight trailing whitespaces with `trailing-whitespace' face.
Use `delete-trailing-whitespace' command."
  (setq-local show-trailing-whitespace t))

;; TODO: report this
;; BUG: In `display-fill-column-indicator-mode': two strings are compared with
;;   `eq' instead of `equal' and result is always nil.
(defun helheim-show-fill-column-indicator ()
  "Display `fill-column' indicator."
  (setq-local display-fill-column-indicator t
              display-fill-column-indicator-character ?\u2502))

;;;; Fringes

;; Disable visual indicators in the fringe for buffer boundaries and empty lines.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

(setq-default left-fringe-width  8
              right-fringe-width 8)

;;;; Modeline

;; Show (line,column) indicator in modeline.
(add-hook 'after-init-hook #'line-number-mode)
(add-hook 'after-init-hook #'column-number-mode)

;;;; Windows

;; Prefer vertical splits over horizontal ones.
(setq split-width-threshold 150
      split-height-threshold nil
      window-combination-resize t)

;; Prefer open new buffer in current window, all else being equal.
(setq pop-up-windows nil)
(setq display-buffer-base-action
      '((display-buffer-reuse-mode-window
         display-buffer-in-previous-window
         display-buffer-use-some-window
         display-buffer-pop-up-window)))

;; If I trying to switch buffer in dedicated window, put it somewhere
;; instead of error out.
(setq switch-to-buffer-in-dedicated-window 'pop)

(setq window-resize-pixelwise t)

;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'after-init-hook #'window-divider-mode)

;; (setq even-window-sizes 'height-only)
;; (setq window-sides-vertical nil)
(setq fit-window-to-buffer-horizontally t)
(setq fit-frame-to-buffer t)

;;;; Scrolling

;; ;; Move point to top/bottom of buffer before signaling a scrolling error.
;; (setq scroll-error-top-bottom t)

;; Enables faster scrolling. This may result in brief periods of inaccurate
;; syntax highlighting, which should quickly self-correct.
(setq fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

;; Keep screen position if scroll command moved it vertically out of the window.
(setq scroll-preserve-screen-position t)

;; 1. Preventing automatic adjustments to `window-vscroll' for long lines.
;; 2. Resolving the issue of random half-screen jumps during scrolling.
(setq auto-window-vscroll nil)

;; Number of lines of margin at the top and bottom of a window.
(setq scroll-margin 0)

;; Number of lines of continuity when scrolling by screenfuls.
(setq next-screen-context-lines 0)

;; Horizontal scrolling
(setq hscroll-margin 2
      hscroll-step 1)

;; Disable auto-adding a new line at the bottom when scrolling.
(setq next-line-add-newlines nil)

;; Why is `jit-lock-stealth-time' nil by default?
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2022-02/msg00352.html
(setq jit-lock-stealth-time 1.25 ; Calculate fonts when idle for 1.25 seconds
      jit-lock-stealth-nice 0.5  ; Seconds between font locking
      jit-lock-chunk-size 4096)

;; Avoid fontification while typing
(setq jit-lock-defer-time 0)
(add-hook 'hel-insert-state-enter-hook (lambda () (setq jit-lock-defer-time 0.25)))
(add-hook 'hel-insert-state-exit-hook  (lambda () (setq jit-lock-defer-time 0)))

;;;;; Smooth scrolling

;; `hel-scrolling' is based on `pixel-scroll' and uses its settings
(setup pixel-scroll
  (:setopt pixel-scroll-precision-large-scroll-height 20.0
           ;; The duration of smooth scrolling.
           pixel-scroll-precision-interpolation-total-time 0.3
           ;; Enable smooth scrolling with PageDown and PageUp keys
           pixel-scroll-precision-interpolate-page t)
  (:global-bind
    [remap scroll-up-command]   'pixel-scroll-interpolate-down
    [remap scroll-down-command] 'pixel-scroll-interpolate-up))

;;;;; Scrolling with mouse wheel and touchpad

(setup ultra-scroll
  (:install ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  (:after-init ultra-scroll-mode)
  (:setopt mouse-wheel-tilt-scroll t ; Scroll horizontally with mouse side wheel.
           mouse-wheel-progressive-speed nil))

;;; Text editing
;;;; Misc

;; Remove duplicates from the kill ring to reduce clutter.
(setq kill-do-not-save-duplicates t)

;;;; Undo/redo

(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;;;; Formatting

(setq-default fill-column 80)

;; Prefer spaces over tabs.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent 'complete
              tab-first-completion 'word)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)
;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
;; so off it goes.
(setq truncate-partial-width-windows nil)

;; This was a widespread practice in the days of typewriters, but it is obsolete
;; nowadays.
(setq sentence-end-double-space nil)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline".
(setq require-final-newline t)

;;;; Parens

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

(setopt show-paren-delay 0.1
        show-paren-highlight-openparen t
        ;; show-paren-when-point-inside-paren t
        ;; show-paren-when-point-in-periphery t
        )

(setup rainbow-delimiters
  (:install t)
  (:hook (prog-mode-hook conf-mode-hook)))

;;;; Prog-mode

(setq comment-empty-lines t
      comment-multi-line t)

(add-hook 'prog-mode-hook 'helheim-show-trailing-whitespace)

;;;; Text-mode

;; (setup text-mode
;;   (:hook text-mode-hook (display-line-numbers-mode
;;                          rainbow-delimiters-mode
;;                          ;; visual-line-mode
;;                          +wrap-line-mode)))

;;;; Tree-sitter

(setq treesit-language-source-alist
      '((bash       "https://github.com/tree-sitter/tree-sitter-bash"
                    "v0.23.3")
        ;; (bibtex     "https://github.com/latex-lsp/tree-sitter-bibtex")
        (c          "https://github.com/tree-sitter/tree-sitter-c"
                    "v0.21.3")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
        (cmake      "https://github.com/uyha/tree-sitter-cmake")
        (c-sharp    "https://github.com/tree-sitter/tree-sitter-c-sharp"
                    "v0.23.1")
        (css        "https://github.com/tree-sitter/tree-sitter-css"
                    "v0.23.2")
        (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp"
                    "v0.4.1")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elixir     "https://github.com/elixir-lang/tree-sitter-elixir")
        (go         "https://github.com/tree-sitter/tree-sitter-go"
                    "v0.23.4")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod"
                    "v1.0.2")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (java       "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                    "v0.23.1")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (latex      "https://github.com/latex-lsp/tree-sitter-latex"
                    "v0.3.0")
        (lua        "https://github.com/tree-sitter-grammars/tree-sitter-lua"
                    "v0.3.0")
        (make       "https://github.com/tree-sitter-grammars/tree-sitter-make")
        (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                    "v0.3.2"
                    "tree-sitter-markdown/src")
        (nix        "https://github.com/nix-community/tree-sitter-nix")
        ;; (nu         "https://github.com/nushell/tree-sitter-nu")
        (perl       "https://github.com/ganezdragon/tree-sitter-perl")
        (python     "https://github.com/tree-sitter/tree-sitter-python"
                    "v0.23.6")
        (r          "https://github.com/r-lib/tree-sitter-r")
        (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust"
                    "v0.23.3")
        (sql        "https://github.com/DerekStride/tree-sitter-sql"
                    "gh-pages")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "v0.23.2"
                    "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
                    "v0.23.2"
                    "tsx/src")
        (toml       "https://github.com/tree-sitter-grammars/tree-sitter-toml")
        (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (zig        "https://github.com/tree-sitter-grammars/tree-sitter-zig"
                    "v1.1.2")))

(setup treesit
  (:when (treesit-available-p))
  (:hook emacs-startup-hook helheim-install-missing-treesit-grammars)
  (:setopt treesit-font-lock-level 4)
  ;; BUG: Emacs provides this setting in yaml-ts-mode.el, but it only works
  ;;   after the package is loaded, defeating autoloading. So, we duplicate
  ;;   them here.
  (:mode ("\\.ya?ml\\'" . yaml-ts-mode)
         ("clang.format\\'" . yaml-ts-mode)))

;; For backward compatibility.
(provide 'helheim-tree-sitter)

;;;; Extra file extensions to support

(add-to-list 'auto-mode-alist '("/LICENSE\\'" . text-mode))
(add-to-list 'auto-mode-alist '("rc\\'" . conf-mode) :append)

;;;; Editing files with very long lines

(setup so-long
  (:install t)
  (:require t)
  (:after-init global-so-long-mode)
  ;;
  ;; Don't disable syntax highlighting and line numbers, or make the buffer
  ;; read-only, in `so-long-minor-mode', so we can have a basic editing
  ;; experience in them, at least. It will remain off in `so-long-mode',
  ;; however, because long files have a far bigger impact on Emacs performance.
  (cl-callf2 delq 'font-lock-mode so-long-minor-modes)
  (cl-callf2 delq 'display-line-numbers-mode so-long-minor-modes)
  (setf (alist-get 'buffer-read-only so-long-variable-overrides nil t) nil)
  ;; ...but at least reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; But disable everything else that may be unnecessary/expensive for large
  ;; or wide buffers.
  (cl-callf nconc so-long-minor-modes '(eldoc-mode
                                        auto-composition-mode
                                        hl-fill-column-mode
                                        spell-fu-mode
                                        undo-tree-mode
                                        ws-butler-mode
                                        highlight-indent-guides-mode)))

;;; Emacs built-in packages

;; This setting forces Emacs to save bookmarks immediately after each change,
;; to make you never lose bookmarks if Emacs crashes.
(setq bookmark-save-flag 1)

;;;; Custom

(setup custom
  (:setopt custom-file (expand-file-name "custom.el" helheim-root-directory)
           custom-theme-directory (expand-file-name "themes/" helheim-root-directory)
           custom-buffer-done-kill t)
  (:after-init
    (load custom-file t t)))

;;;; Help

;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)

(setup help
  (:setopt help-enable-autoload nil
           help-enable-completion-autoload nil
           help-enable-symbol-autoload nil
           help-window-select t) ;; Focus new help windows on open.
  (:after-load
    (:with-keymap help-map
      (:unbind
        "h"       ;; `view-hello-file'
        "C-c")))) ;; `describe-copying'

(setup helpful
  (:install t)
  ;; (:hook helpful-mode-hook outline-minor-mode)
  (:global-bind
    [remap describe-function] 'helpful-callable
    [remap describe-variable] 'helpful-variable
    [remap describe-command]  'helpful-command
    [remap describe-key]      'helpful-key
    [remap describe-symbol]   'helpful-symbol)
  ;; Open links to functions, variables and symbols in helpful buffer in the
  ;; same window.
  (add-to-list 'display-buffer-alist
               '((derived-mode . helpful-mode)
                 (display-buffer-reuse-mode-window display-buffer-pop-up-window)
                 (mode . helpful-mode)
                 (body-function . select-window))))

;;;; Minibuffer

;; Allow opening new minibuffers from inside existing minibuffers.
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(setq resize-mini-windows 'grow-only
      history-delete-duplicates t)

;; Keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '( read-only t
                                      intangible t
                                      cursor-intangible t
                                      face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;;;; Save minibuffer history between sessions

(setup savehist
  (:require t)
  (:setopt history-length 300
           savehist-additional-variables '(kill-ring
                                           mark-ring
                                           global-mark-ring
                                           register-alist
                                           search-ring
                                           regexp-search-ring))
  (:hook savehist-save-hook (helheim-savehist-unpropertize-variables-h
                             helheim-savehist-remove-unprintable-registers-h))
  (:after-init savehist-mode))

;;;; Buffers

(setq uniquify-buffer-name-style 'forward)

;; ;; BUG: This setting breaks `project-kill-buffers' command.
;; ;; "C-x C-b" (`list-buffers') and `M-x buffer-menu'.
;; (when (version<= "30.2" emacs-version)
;;   (setopt Buffer-menu-group-by #'Buffer-menu-group-by-root))

;; ;; The initial buffer is created during startup even in non-interactive
;; ;; sessions, and its major mode is fully initialized. Modes like `text-mode',
;; ;; `org-mode', or even the default `lisp-interaction-mode' load extra packages
;; ;; and run hooks, which can slow down startup.
;; ;;
;; ;; Using `fundamental-mode' for the initial buffer to avoid unnecessary
;; ;; startup overhead.
;; (setq initial-major-mode 'fundamental-mode
;;       initial-scratch-message nil)

;;;; Files

;; ;; Resolve symlinks when opening files, so that any operations are conducted
;; ;; from the file's true directory (like `find-file').
;; (setq find-file-visit-truename t
;;       vc-follow-symlinks t)

;; ;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; ;; warning as it will redirect you to the existing buffer anyway.
;; (setq find-file-suppress-same-file-warnings t)

;; ;; Skip confirmation prompts when creating a new file or buffer
;; (setq confirm-nonexistent-file-or-buffer nil)

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive)
      remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Create missing directories when we open a file that doesn't exist under
;; a directory tree that may not exist.
(add-hook 'find-file-not-found-functions #'helheim-create-missing-directories-h)

;;;;; Backup files

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and it is not a good idead to have world-readable
;; copies of potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist `(("." . ,(locate-user-emacs-file "backup")))
      tramp-backup-directory-alist backup-directory-alist)

;;;;; Auto save changes in files
;; Use `recover-file' or `recover-session' commands to restore auto-saved data.

(setq auto-save-default t
      auto-save-no-message t
      auto-save-include-big-deletions t
      kill-buffer-delete-auto-save-files t
      auto-save-list-file-prefix (locate-user-emacs-file "autosave/")
      ;; This resolves two issue while ensuring auto-save files are still
      ;; reasonably recognizable at a glance:
      ;;
      ;; 1. Emacs generates long file paths for its auto-save files; long is:
      ;;    `auto-save-list-file-prefix' + `buffer-file-name'. If too long, the
      ;;    filesystem will murder your cat. `sha1' compresses the path into a
      ;;    ~40 character hash!
      ;; 2. The default transform rule writes TRAMP auto-save files to
      ;;    `temporary-file-directory', which TRAMP doesn't like! It'll prompt
      ;;    you about it every time an auto-save file is written, unless
      ;;    `tramp-allow-unsafe-temporary-files' is set. A more sensible default
      ;;    transform is better:
      auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(file-name-concat auto-save-list-file-prefix "tramp-\\2-")
         sha1)
        ("\\`/\\([^/]+/\\)*\\([^/]+\\)\\'"
         ,(file-name-concat auto-save-list-file-prefix "\\2-")
         sha1)))

(add-hook 'auto-save-hook (defun helheim-ensure-auto-save-prefix-exists-h ()
                            (with-file-modes #o700
                              (make-directory auto-save-list-file-prefix t))))

;; HACK: Make sure backup files (like `undo-tree's) don't have ridiculously long
;;   file names that some filesystems will refuse.
(advice-add 'make-backup-file-name-1 :around
            #'helheim-make-hashed-backup-file-name-a)

;;;;; Auto revert

;; Automatically revert the buffer when its visited file changes on disk.
;; Auto Revert will not revert a buffer if it has unsaved changes, or if
;; its file on disk is deleted or renamed.
(setup autorevert
  (:after-init global-auto-revert-mode)
  (:setopt auto-revert-stop-on-user-input nil
           auto-revert-verbose t ; let us know when it happens
           auto-revert-use-notify nil
           auto-revert-stop-on-user-input nil
           ;; Only prompts for confirmation when buffer is unsaved.
           revert-without-query '(".")
           global-auto-revert-non-file-buffers t ; e.g, Dired
           global-auto-revert-ignore-modes '(ibuffer-mode ; has its own `ibuffer-auto-mode'
                                             Buffer-menu-mode)))

;;;;; Keep track of recently opened files and places in them

;; Keep track of opened files.
(setup recentf
  (:setopt recentf-max-saved-items 300 ; default is 20
           ;; Auto clean up recent files only in long-running daemon sessions,
           ;; else do it on quiting Emacs.
           recentf-auto-cleanup (if (daemonp) 300))
  (:after-init
    (let ((inhibit-message t))
      (recentf-mode 1)))
  (:after-load
    (:hook kill-emacs-hook recentf-cleanup)
    ;; Don't remember files in runtime folders.
    (require 'xdg)
    (add-to-list 'recentf-exclude (concat "^" (regexp-quote (or (xdg-runtime-dir)
                                                                "/run"))))
    ;; PERF: Text properties inflate the size of recentf's files, and there is no
    ;;   reason to persist them (must be first in `recentf-filename-handlers'!)
    (add-to-list 'recentf-filename-handlers #'substring-no-properties)))

;; Save the last location within a file upon reopening.
(setup saveplace
  (:setopt save-place-limit 600)
  (:after-init save-place-mode)
  ;; :config
  ;; (setq save-place-file (locate-user-emacs-file "saveplace"))
  )

;;;; TRAMP

(setopt remote-file-name-inhibit-cache 60
        remote-file-name-inhibit-auto-save-visited t
        tramp-default-method "ssh" ; faster than the default "scp"
        tramp-copy-size-limit (* 1024 1024) ; 1mb
        tramp-use-scp-direct-remote-copying t
        ;; tramp-verbose 1 ; Reduce verbosity
        tramp-completion-reread-directory-timeout 60)

;;;; Imenu

;; Automatically rescan the buffer for Imenu entries when `imenu' is invoked.
;; This ensures the index reflects recent edits.
(setq imenu-auto-rescan t)

;; Prevent truncation of long function names in `imenu' listings.
(setq imenu-max-item-length 160)

;;;; Calendar

(setup calendar
  (:setopt calendar-week-start-day 1)) ;; Monday

(add-to-list 'display-buffer-alist
             '((major-mode . calendar-mode)
               ;; display-buffer-at-bottom ;; display-buffer-below-selected
               ;; (window-height . shrink-window-if-larger-than-buffer)
               display-buffer-in-side-window
               (side . bottom)
               ;; (slot . -1) ;; -1 == L  0 == Mid 1 == R
               (body-function . select-window)))

;;;; Completion

;; Ignore cases
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Hide in `M-x' menu commands irrelevant to current buffer's mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;;;;; hippie-expand

(setup hippie-exp
  (:global-bind
    [remap dabbrev-expand] 'hippie-expand)
  (:after-load
    (:setopt hippie-expand-try-functions-list
             '( try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-file-name-partially
                try-complete-file-name
                try-expand-all-abbrevs
                try-expand-list
                try-expand-line
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol))))

;;;; Comint (COMmand INTerpreter)

(setq ansi-color-for-comint-mode t
      comint-prompt-read-only t
      comint-buffer-maximum-size 4096)

(add-to-list 'comint-output-filter-functions 'comint-osc-process-output)

;;;; Compile

(setq compilation-ask-about-save nil ; save all buffers on `compile' without asking
      compilation-always-kill t ; kill compilation process before starting another
      compilation-scroll-output 'first-error)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; Automatically truncate compilation buffers so they don't accumulate too
;; much data and bog down the rest of Emacs.
(setup comint
  (:autoload comint-truncate-buffer)
  (:hook compilation-filter-hook comint-truncate-buffer))

;;;; Eldoc

(setup eldoc
  (:blackout t)
  (:setopt eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))
;; eldoc-documentation-functions

;;;; image-mode

(setq image-animate-loop t
      image-auto-resize 'fit-window)

;;;; occur-mode

;; Create separate *Occur* buffer for each search.
(add-hook 'occur-hook 'occur-rename-buffer)

;; Open `occur-mode' buffer in another window.
(add-to-list 'display-buffer-alist
             '((major-mode . occur-mode)
               (display-buffer-use-some-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (body-function . select-window)))

;;;; project.el

(setup project
  ;; (:install t)
  (:built-in)
  (:setopt project-vc-extra-root-markers '(".project")
           project-vc-merge-submodules nil
           project-kill-buffers-display-buffer-list t
           ;; project-mode-line t
           project-switch-commands '((project-dired "Dired")
                                     (project-find-file "Find file")
                                     ;; (project-find-regexp "Find regexp")
                                     (project-find-dir "Find directory")
                                     (project-vc-dir "VC-Dir")
                                     (project-eshell "Eshell")
                                     (project-any-command "Other"))))

;;;; Version Control

(setup vc
  (:setopt vc-git-print-log-follow t
           ;; Do not backup version controlled files.
           vc-make-backup-files nil
           ;; Faster algorithm for diffing.
           vc-git-diff-switches '("--histogram")
           ;; Remove RCS, CVS, SCCS, and Bzr, because it's a lot less work for vc
           ;; to check them all (especially in TRAMP buffers), and who uses any of
           ;; these?
           vc-handled-backends '(Git Hg SVN SRC)
           ;; PERF: Ignore node_modules (expensive for vc ops to index).
           vc-ignore-dir-regexp (format "%s\\|%s"
                                        locate-dominating-stop-dir-regexp
                                        "[/\\\\]node_modules")))

(with-eval-after-load 'vc-annotate
  (keymap-set vc-annotate-mode-map "<remap> <quit-window>" #'kill-current-buffer))

;; (setup vc-annotate
;;   (:after-load
;;     ;; vc-annotate-mode-map
;;     (:bind [remap quit-window] #'kill-current-buffer)))

;;;; Which-Key

(setup which-key
  (:after-init which-key-mode)
  (:setopt which-key-lighter nil
           which-key-sort-order 'which-key-key-order-alpha
           which-key-idle-delay 1.5
           which-key-idle-secondary-delay 0.25
           which-key-add-column-padding 1
           which-key-max-description-length 40
           which-key-show-remaining-keys t
           which-key-ellipsis "…"
           which-key-allow-multiple-replacements nil)
  ;; WARN: Following `which-key-replacement-alist' is tailored for
  ;;   `which-key-allow-multiple-replacements' set to nil.
  (setq which-key-replacement-alist
        `(((nil . "which-key-show-next-page-no-cycle") . (nil . "wk next pg"))
          (("RET\\|<return>" . nil) . ("Enter" . nil))
          (("<backtab>" . nil) . ("S-tab" . nil))
          ;; DEL -> Bsp
          ;; M-DEL -> M-Bsp
          (("\\([[:word:]-]*\\)DEL" . nil) . ("\\1Bsp" . nil))
          ;; <left> -> left
          ;; <C-m>  -> C-m
          (("<\\([[:alnum:]-]+\\)>") . ("\\1"))
          ;; ((nil . "copy-as-kill") . (nil . "copy"))
          ;; Strip meaningless packages prefixes.
          ,(cons `(nil . ,(rx line-start
                              (or "consult-" "hel-" "helheim-"
                                  "elisp-" "pp-" ;; elisp pretty print
                                  "xref-" "my-")))
                 '(nil . ""))
          ;; Display embark specific commands literally...
          ;; WARN: If `which-key-allow-multiple-replacements' is t all follwing
          ;;   settings will break and "embark-" prefix will be stripped from
          ;;   ALL commands!
          ,(cons `(nil . ,(rx line-start
                              (or "embark-act" "embark-dwim" "embark-become"
                                  "embark-export" "embark-collect"
                                  "embark-cycle" "embark-live")
                              line-end))
                 '(nil . nil))
          ((nil . "embark-copy-as-kill") . (nil . "copy"))
          ;; ... for other commands strip "embark-" prefix.
          ((nil . "^embark-") . (nil . "")))))

(setup which-key-posframe
  (:install t)
  (:after which-key)
  (:hook which-key-mode-hook which-key-posframe-mode)
  (:setopt which-key-max-display-columns 1
           which-key-posframe-poshandler '+posframe-poshandler-window-bottom-right-corner-with-padding))

;;; .
(provide 'helheim-core)
;;; helheim-core.el ends here
