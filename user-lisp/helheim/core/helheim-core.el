;;; helheim-core.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Customization

(defgroup helheim nil
  "Helheim specific settings."
  :prefix 'helheim-)

(defcustom helheim-id-format "%Y%m%dT%H%M%S"
  "The format string of timebased ID as `format-time-string' accepts."
  :type 'string
  :group 'helheim)

(defcustom helheim-file-id-regexp "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)--"
  "File name ID regexp."
  :type 'string
  :group 'helheim)

;;; straight.el package manager

(setopt straight-check-for-modifications '(check-on-save find-when-checking)
        ;; Prepend name with space to hide straight buffer in Ibuffer.
        straight-process-buffer " *straight-process*")

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        (or (bound-and-true-p straight-base-dir)
                                            user-emacs-directory)))
      (bootstrap-version 7)
      (url "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil t))

;;; Dependencies

(progn
  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  (require 'leaf)
  (require 'leaf-keywords)
  ;; leaf.el provides both `:config' and `:defer-config' keywords. However, if
  ;; a keyword from `leaf-defer-keywords' appears in a declaration, `:config'
  ;; is treated as `:defer-config'. I consider implicit behavior harmful, so
  ;; disable it to keep full manual control over what is deferred.
  (setopt leaf-defer-keywords nil)
  (setopt leaf-expand-minimally t)
  (leaf-keywords-init))

(leaf dash     :straight t :require t)
(leaf s        :straight t :require t)
(leaf blackout :straight t :require t)
(leaf pcre2el  :straight t)
(leaf wgrep    :straight t)

(leaf avy
  :straight t
  :config
  (setopt avy-keys (number-sequence ?a ?z) ;; Any lower-case letter a-z.
          avy-style 'at-full
          avy-all-windows nil
          avy-all-windows-alt t
          avy-background t
          ;; the unpredictability of this (when enabled) makes it a poor default
          avy-single-candidate-jump t))

(leaf hel
  :straight (hel :host github :repo "anuvyklack/hel" :files (:defaults "**"))
  :global-minor-mode hel-mode)

(leaf transient
  :straight t
  :config
  (setopt transient-common-command-prefix "SPC"
          ;; Pop up transient windows at the bottom of the current window
          ;; instead of entire frame. This is more ergonomic for users with
          ;; large displays or many splits.
          transient-display-buffer-action '(display-buffer-below-selected
                                            (dedicated . t)
                                            (inhibit-same-window . t))
          transient-show-during-minibuffer-read t
          ;; transient-default-level 5
          )
  :defer-config
  ;; Close transient menus with ESC.
  (keymap-set transient-map "<escape>" #'transient-quit-one))

(leaf nerd-icons
  :straight t
  :custom
  (nerd-icons-scale-factor . 0.95)
  :defer-config
  ;; Add some icons
  (cl-loop for (mode . icon-spec)
           in '((fundamental-mode nerd-icons-faicon "nf-fa-file_o" :face nerd-icons-dsilver)
                (deadgrep-mode nerd-icons-faicon "nf-fa-search"))
           do (setf (alist-get mode nerd-icons-mode-icon-alist)
                    icon-spec)))

;; `helheim-word-wrap' dependencies, which file is autoloaded, so we install
;; its dependences out of it.
(leaf adaptive-wrap :straight t)
(leaf visual-fill-column :straight t)

(require 'helheim-lib)

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
  (define-advice global-hl-line-highlight (:around (orig-fun) helheim)
    (if (buffer-match-p global-hl-line-buffers (current-buffer))
        (funcall orig-fun)
      (global-hl-line-unhighlight))))

(leaf hl-line
  :global-minor-mode global-hl-line-mode
  :custom
  (global-hl-line-sticky-flag . 'window) ;; Emacs 31
  ;; I want current line highlighting only in special modes that are in Hel
  ;; motion state, and disable it when region is active. In text editing modes
  ;; it interferes with Hel selections.
  (global-hl-line-buffers . `(and (or (derived-mode . special-mode)
                                      ;; For `define-compilation-mode'
                                      (derived-mode . compilation-mode)
                                      ;; Follow major modes doesn't inherit
                                      ;; from `special-mode'.
                                      (major-mode . dired-mode))
                                  ;; According to my measures, compiled lambda
                                  ;; is two times faster.
                                  ,(native-compile
                                    '(lambda (buffer)
                                       (with-current-buffer buffer
                                         (and (eq hel-state 'motion)
                                              (not (use-region-p)))))))))

;;;; Display line numbers

(leaf display-line-numbers
  :hook prog-mode-hook conf-mode-hook text-mode-hook
  :custom
  (display-line-numbers-width . 3)
  (display-line-numbers-type . t)
  (display-line-numbers-width-start . t)
  (display-line-numbers-grow-only . t)
  ;; Show absolute line numbers for narrowed regions to make it easier to tell
  ;; the buffer is narrowed, and where you are, exactly.
  (display-line-numbers-widen . t))

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

;; By default Emacs distinguishes between automatic and manual window
;; switching. If you effect a window switch yourself with "C-x b", it’s
;; manual — and exempt from any display action rules you create yourself.
;; Disable this behavior.
(setq switch-to-buffer-obey-display-actions t)

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
(setq fast-but-imprecise-scrolling t)

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

(leaf pixel-scroll
  :custom
  ;; The duration of smooth scrolling.
  (pixel-scroll-precision-interpolation-total-time . 0.3)
  (pixel-scroll-precision-large-scroll-height . 20.0)
  ;; Enable smooth scrolling with PageDown and PageUp keys
  (pixel-scroll-precision-interpolate-page . t)
  :bind
  ([remap scroll-up-command] . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up))

;;;;; Scrolling with mouse wheel and touchpad

(leaf ultra-scroll
  :straight (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :hook (after-init-hook . ultra-scroll-mode)
  :custom
  (mouse-wheel-tilt-scroll . t) ; Scroll horizontally with mouse side wheel.
  (mouse-wheel-progressive-speed . nil))

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

;; Default to soft line-wrapping in text modes. It is more sensibile for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)

;;;; Parens

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

(setopt show-paren-delay 0.1
        show-paren-highlight-openparen t
        ;; show-paren-when-point-inside-paren t
        ;; show-paren-when-point-in-periphery t
        )

(leaf rainbow-delimiters
  :straight t
  :hook prog-mode-hook conf-mode-hook) ; text-mode-hook

;;;; Prog-mode

(setq comment-empty-lines t
      comment-multi-line t)

(add-hook 'prog-mode-hook 'helheim-show-trailing-whitespace)

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

(leaf treesit
  :when (treesit-available-p)
  :hook (emacs-startup-hook . helheim-install-missing-treesit-grammars)
  :custom
  (treesit-font-lock-level . 4)
  :mode
  ;; Emacs provides this setting in yaml-ts-mode.el, but it only works after
  ;; the package is loaded, defeating autoloading. So, we duplicate it here.
  ("\\.ya?ml\\'" . yaml-ts-mode)
  ("clang.format\\'" . yaml-ts-mode))

;; For backward compatibility.
(provide 'helheim-tree-sitter)

;;;; Extra file extensions to support

(add-to-list 'auto-mode-alist '("/LICENSE\\'" . text-mode))
(add-to-list 'auto-mode-alist '("rc\\'" . conf-mode) 'append)

;;;; Editing files with very long lines

(leaf so-long
  :straight t
  :require t
  :hook (after-init-hook . global-so-long-mode)
  :config
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
  ;; But disable everything else that may be unnecessary/expensive for large or
  ;; wide buffers.
  (cl-callf append so-long-minor-modes '(eldoc-mode
                                         auto-composition-mode
                                         hl-fill-column-mode
                                         spell-fu-mode
                                         undo-tree-mode
                                         ws-butler-mode
                                         highlight-indent-guides-mode)))

;;; Emacs built-in packages

;; This setting forces Emacs to save bookmarks immediately after each change.
;; Benefit: you never lose bookmarks if Emacs crashes.
(setq bookmark-save-flag 1)

(setq custom-buffer-done-kill t)

;;;; Help

;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)

(leaf help
  :custom
  (help-enable-autoload . nil)
  (help-enable-completion-autoload . nil)
  (help-enable-symbol-autoload . nil)
  (help-window-select . t) ; Focus new help windows on open.
  :defer-config
  (hel-keymap-set help-map
    "h"   nil   ; unbind `view-hello-file'
    "C-c" nil)) ; unbind `describe-copying'

(leaf helpful
  :straight t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol))
  ;; (helpful-mode-hook . outline-minor-mode)
  :config
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

(leaf savehist
  :require t
  :global-minor-mode savehist-mode
  :custom
  (history-length . 300)
  (savehist-additional-variables . '(kill-ring
                                     mark-ring
                                     global-mark-ring
                                     register-alist
                                     search-ring
                                     regexp-search-ring))
  :hook
  (savehist-save-hook . helheim-savehist-unpropertize-variables-h)
  (savehist-save-hook . helheim-savehist-remove-unprintable-registers-h))

;;;; Buffers

(setq uniquify-buffer-name-style 'forward)

;; "C-x C-b" (`list-buffers') and `M-x buffer-menu'.
(when (version<= "30.2" emacs-version)
  (setopt Buffer-menu-group-by #'Buffer-menu-group-by-root))

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
(add-hook 'find-file-not-found-functions
  (defun helheim-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;;;;; Backup files

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      backup-directory-alist (list (cons "." (expand-file-name "backup" user-emacs-directory)))
      tramp-backup-directory-alist backup-directory-alist
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5)

;;;;; Auto save changes in files
;; Use `recover-file' or `recover-session' commands to restore auto-saved data.

(setq auto-save-default t
      auto-save-no-message t
      ;; Auto-disable auto-save after deleting large chunks of text.
      ;; I believe that in case of crash the more date the better.
      auto-save-include-big-deletions nil
      kill-buffer-delete-auto-save-files t)

(setq auto-save-list-file-prefix (expand-file-name "autosave/" user-emacs-directory)
      tramp-auto-save-directory  (expand-file-name "tramp-autosave/" user-emacs-directory))

;; HACK: Emacs generates long file paths for its auto-save files; long is:
;;   `auto-save-list-file-prefix' + `buffer-file-name'. If too long, the
;;   filesystem will murder your family. To appease it the `buffer-file-name'
;;   is compressed to a stable 40 characters.
;; Borrowed from Doom Emacs.
(define-advice make-auto-save-file-name ( :around (fn)
                                          helheim-make-hashed-auto-save-file-name)
  "Compress the auto-save file name so paths don't get too long."
  (let ((buffer-file-name
         (if (or
              ;; Don't do anything for non-file-visiting buffers. Names
              ;; generated for those are short enough already.
              (null buffer-file-name)
              ;; If an alternate handler exists for this path, bow out. Most of
              ;; them end up calling `make-auto-save-file-name' again anyway, so
              ;; we still achieve this advice's ultimate goal.
              (find-file-name-handler buffer-file-name
                                      'make-auto-save-file-name))
             buffer-file-name
           (sha1 buffer-file-name))))
    (funcall fn)))

;; HACK: ...does the same for Emacs backup files, but also packages that use
;;   `make-backup-file-name-1' directly (like undo-tree).
(define-advice make-backup-file-name-1 ( :around (fn file)
                                         helheim-make-hashed-backup-file-name)
  "A few places use the backup file name so paths don't get too long."
  (let ((alist backup-directory-alist)
        backup-directory)
    (while alist
      (let ((elt (car alist)))
        (if (string-match (car elt) file)
            (setq backup-directory (cdr elt)
                  alist nil)
          (setq alist (cdr alist)))))
    (let ((file (funcall fn file)))
      (if (or (null backup-directory)
              (not (file-name-absolute-p backup-directory)))
          file
        (expand-file-name (sha1 (file-name-nondirectory file))
                          (file-name-directory file))))))

;;;;; Auto revert

;; Automatically revert the buffer when its visited file changes on disk.
;; Auto Revert will not revert a buffer if it has unsaved changes, or if
;; its file on disk is deleted or renamed.
(leaf autorevert
  :global-minor-mode global-auto-revert-mode
  :custom
  (auto-revert-stop-on-user-input . nil)
  (auto-revert-verbose . t) ; let us know when it happens
  (auto-revert-use-notify . nil)
  (auto-revert-stop-on-user-input . nil)
  ;; Only prompts for confirmation when buffer is unsaved.
  (revert-without-query . '("."))
  (global-auto-revert-non-file-buffers . t) ; e.g, Dired
  (global-auto-revert-ignore-modes . '(ibuffer-mode ; has its own `ibuffer-auto-mode'
                                       Buffer-menu-mode)))

;;;;; Keep track of recently opened files and places in them

;; Keep track of opened files.
(leaf recentf
  :hook
  (after-init-hook . (lambda ()
                       (let ((inhibit-message t))
                         (recentf-mode 1))))
  (kill-emacs-hook . recentf-cleanup)
  :config
  (setopt recentf-max-saved-items 300 ; default is 20
          ;; Auto clean up recent files only in long-running daemon sessions,
          ;; else do it on quiting Emacs.
          recentf-auto-cleanup (if (daemonp) 300))
  :defer-config
  ;; Don't remember files in runtime folders.
  (require 'xdg)
  (add-to-list 'recentf-exclude (concat "^" (regexp-quote (or (xdg-runtime-dir)
                                                              "/run"))))
  ;; PERF: Text properties inflate the size of recentf's files, and there is no
  ;;   reason to persist them (must be first in `recentf-filename-handlers'!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties))

;; Save the last location within a file upon reopening.
(leaf saveplace
  :custom (save-place-limit . 600)
  :hook (after-init-hook . save-place-mode)
  ;; :config
  ;; (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  )

;;;; TRAMP

(setq remote-file-name-inhibit-cache 50
      tramp-default-method "ssh" ; faster than the default "scp"
      tramp-verbose 1 ; Reduce verbosity
      tramp-completion-reread-directory-timeout 50)

;;;; Imenu

;; Automatically rescan the buffer for Imenu entries when `imenu' is invoked.
;; This ensures the index reflects recent edits.
(setq imenu-auto-rescan t)

;; Prevent truncation of long function names in `imenu' listings.
(setq imenu-max-item-length 160)

;;;; Calendar

(setopt calendar-week-start-day 1) ;; Monday

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

(leaf hippie-exp
  :config
  (keymap-global-set "<remap> <dabbrev-expand>" 'hippie-expand)
  :defer-config
  (setopt hippie-expand-try-functions-list
          '( try-expand-dabbrev
             try-expand-dabbrev-all-buffers
             try-expand-dabbrev-from-kill
             try-complete-file-name-partially
             try-complete-file-name
             try-expand-all-abbrevs
             try-expand-list
             try-expand-line
             try-complete-lisp-symbol-partially
             try-complete-lisp-symbol)))

;;;; Comint (general command interpreter in a window)

(setq ansi-color-for-comint-mode t
      comint-prompt-read-only t
      comint-buffer-maximum-size 4096)

;;;; Compile

(setq compilation-ask-about-save nil ; save all buffers on `compile' without asking
      compilation-always-kill t ; kill compilation process before starting another
      compilation-scroll-output 'first-error)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; Automatically truncate compilation buffers so they don't accumulate too
;; much data and bog down the rest of Emacs.
(leaf comint
  :commands comint-truncate-buffer
  :hook (compilation-filter-hook . comint-truncate-buffer))

;;;; Ediff

(setopt ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain
        ;; ediff-keep-variants nil
        )

;;;;; Restore windows configuration after quitting ediff

(let (wconf) ; Private variable shared by two functions.
  ;;
  (defun helheim--ediff-save-window-configuration ()
    (setq wconf (current-window-configuration)))
  ;;
  (defun helheim--ediff-restore-window-configuration ()
    (when (window-configuration-p wconf)
      (set-window-configuration wconf))))

(add-hook 'ediff-before-setup-hook #'helheim--ediff-save-window-configuration)
(add-hook 'ediff-quit-hook    #'helheim--ediff-restore-window-configuration 90)
(add-hook 'ediff-suspend-hook #'helheim--ediff-restore-window-configuration 90)

;;;; Eldoc

(leaf eldoc
  :blackout t
  :custom (eldoc-documentation-strategy . 'eldoc-documentation-compose-eagerly))
;; eldoc-documentation-functions

;;;; image-mode

(setq image-animate-loop t)

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

(leaf project
  :straight (project :type built-in)
  :custom
  (project-vc-extra-root-markers . '(".project"))
  (project-vc-merge-submodules . nil)
  (project-kill-buffers-display-buffer-list . t)
  ;; (project-mode-line . t)
  (project-switch-commands . '((project-dired "Dired")
                               (project-find-file "Find file")
                               ;; (project-find-regexp "Find regexp")
                               (project-find-dir "Find directory")
                               (project-vc-dir "VC-Dir")
                               (project-eshell "Eshell")
                               (project-any-command "Other"))))

;;;; Version Control

(setq vc-git-print-log-follow t
      vc-make-backup-files nil ;; Do not backup version controlled files.
      vc-git-diff-switches '("--histogram")) ;; Faster algorithm for diffing.

;; Remove RCS, CVS, SCCS, and Bzr, because it's a lot less work for vc to
;; check them all (especially in TRAMP buffers), and who uses any of these?
(setq vc-handled-backends '(Git Hg SVN SRC))

;; PERF: Ignore node_modules (expensive for vc ops to index).
(setq vc-ignore-dir-regexp (format "%s\\|%s"
                                   locate-dominating-stop-dir-regexp
                                   "[/\\\\]node_modules"))

(with-eval-after-load 'vc-annotate
  (keymap-set vc-annotate-mode-map "<remap> <quit-window>" #'kill-current-buffer))

;;;; Which-Key

(leaf which-key
  :global-minor-mode which-key-mode
  :custom
  (which-key-lighter . nil)
  (which-key-idle-delay . 1.5)
  (which-key-idle-secondary-delay . 0.25)
  (which-key-add-column-padding . 1)
  (which-key-max-description-length . 40))

;;; .
(provide 'helheim-core)
;;; helheim-core.el ends here
