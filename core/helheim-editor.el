;;; helheim-editor.el --- defaults for text editing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'dash)

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

;;;; Current line highlight

;; I want current line highlighting in special modes and don't want in text
;; editing modes because it interferes with Helix selections. However I use
;; `global-hl-line-mode' because the local `hl-line-mode' works unreliably: the
;; highlighting is missing in Ibuffer when you switch to it, and you have to
;; move the cursor to make it appear. Then it disappears again after auto
;; revert, and you need to move once more — very annoying.
(global-hl-line-mode)

(setq global-hl-line-sticky-flag t)

(defun helheim-disable-hl-line-mode ()
  "Disable `global-hl-line-mode' in current buffer."
  (setq-local global-hl-line-mode nil)
  (global-hl-line-unhighlight))

(defun helheim-enable-hl-line-mode ()
  "Re-enable `global-hl-line-mode' in current buffer."
  (setq-local global-hl-line-mode t)
  ;; (global-hl-line-highlight-all)
  )

(add-hook 'text-mode-hook #'helheim-disable-hl-line-mode)
(add-hook 'prog-mode-hook #'helheim-disable-hl-line-mode)
(add-hook 'conf-mode-hook #'helheim-disable-hl-line-mode)

;;;; Line numbers

(use-package display-line-numbers
  :hook prog-mode-hook text-mode-hook conf-mode-hook
  :custom
  (display-line-numbers-width 3)
  (display-line-numbers-type t)
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t)
  ;; Show absolute line numbers for narrowed regions to make it easier to tell
  ;; the buffer is narrowed, and where you are, exactly.
  (display-line-numbers-widen t))

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

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows t)
(setq highlight-nonselected-windows t)

(setq window-resize-pixelwise t)

;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'after-init-hook #'window-divider-mode)

;; Prefer vertical splits over horizontal ones.
(setq split-width-threshold 170
      split-height-threshold nil)

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
(add-hook 'helix-insert-state-enter-hook (lambda () (setq jit-lock-defer-time 0.25)))
(add-hook 'helix-insert-state-exit-hook  (lambda () (setq jit-lock-defer-time 0)))

;;;;; Smooth scrolling

(use-package pixel-scroll
  :custom
  ;; The duration of smooth scrolling.
  (pixel-scroll-precision-interpolation-total-time 0.3)
  (pixel-scroll-precision-large-scroll-height 20.0)
  ;; Enable smooth scrolling with PageDown and PageUp keys
  (pixel-scroll-precision-interpolate-page t)
  :bind
  ([remap scroll-up-command] . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up))

;;;;; Scrolling with mouse wheel and touchpad

(use-package ultra-scroll
  :ensure (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :hook (elpaca-after-init-hook . ultra-scroll-mode)
  :custom
  (mouse-wheel-tilt-scroll t) ; Scroll horizontally with mouse side wheel.
  (mouse-wheel-progressive-speed nil))

;;; Emacs features

;; This setting forces Emacs to save bookmarks immediately after each change.
;; Benefit: you never lose bookmarks if Emacs crashes.
(setq bookmark-save-flag 1)

(setq custom-buffer-done-kill t)

;;;; Help

;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)

(use-package help
  :custom
  (help-enable-autoload nil)
  (help-enable-completion-autoload nil)
  (help-enable-symbol-autoload nil)
  (help-window-select t) ; Focus new help windows when opened
  :config
  (add-hook 'help-mode-hook #'helheim-disable-hl-line-mode)
  (helix-keymap-set help-map
    "h"   nil   ; unbind `view-hello-file'
    "C-c" nil)) ; unbind `describe-copying'

(use-package helpful
  :ensure t
  :defer t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol))
  :config
  (add-hook 'helpful-mode-hook #'helheim-disable-hl-line-mode)
  ;; (add-hook 'helpful-mode-hook #'outline-minor-mode)

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

(use-package savehist
  :hook
  (after-init-hook . savehist-mode)
  (savehist-save-hook . helheim-savehist-unpropertize-variables-h)
  (savehist-save-hook . helheim-savehist-remove-unprintable-registers-h)
  :custom
  (history-length 300)
  (savehist-additional-variables '(kill-ring
                                   mark-ring
                                   global-mark-ring
                                   register-alist
                                   search-ring
                                   regexp-search-ring)))

(defun helheim-savehist-unpropertize-variables-h ()
  "Remove text properties from `kill-ring' to reduce savehist cache size."
  (setq kill-ring (-> kill-ring
                      (-filter #'stringp)
                      (-map #'substring-no-properties)))
  (setq register-alist (-map (-lambda ((reg . item))
                               (if (stringp item)
                                   (cons reg (substring-no-properties item))
                                 (cons reg item)))
                             register-alist)))

(defun helheim-savehist-remove-unprintable-registers-h ()
  "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
  ;; Save new value in the temp buffer savehist is running
  ;; `savehist-save-hook' in. We don't want to actually remove the
  ;; unserializable registers in the current session!
  (setq-local register-alist (-filter #'savehist-printable register-alist)))

;;;; Buffers

(setq uniquify-buffer-name-style 'forward)

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

(setq remote-file-name-inhibit-cache 50
      tramp-default-method "ssh" ; faster than the default "scp"
      tramp-verbose 1
      tramp-completion-reread-directory-timeout 50)

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
(use-package autorevert
  :hook (after-init-hook . global-auto-revert-mode)
  :custom
  (auto-revert-stop-on-user-input nil)
  (auto-revert-verbose t) ; let us know when it happens
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  ;; Only prompts for confirmation when buffer is unsaved.
  (revert-without-query (list "."))
  (global-auto-revert-non-file-buffers t) ; e.g, Dired
  (global-auto-revert-ignore-modes '(ibuffer-mode
                                     Buffer-menu-mode)))

;;;;; Keep track of recently opened files and places in them
(require 'xdg)

;; Keep track of opened files.
(use-package recentf
  :hook (after-init-hook . (lambda ()
                             (let ((inhibit-message t))
                               (recentf-mode 1))))
  :custom
  (recentf-max-saved-items 300) ; default is 20
  :config
  ;; Auto clean up recent files only in long-running daemon sessions, else
  ;; do it on quiting Emacs.
  (setopt recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup)

  ;; Don't remember files in runtime folders.
  (add-to-list 'recentf-exclude (concat "^" (regexp-quote (or (xdg-runtime-dir)
                                                              "/run"))))

  ;; PERF: Text properties inflate the size of recentf's files, and there is no
  ;;   reason to persist them (must be first in `recentf-filename-handlers'!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties))

;; Save the last location within a file upon reopening.
(use-package saveplace
  :custom (save-place-limit 600)
  :hook (after-init-hook . save-place-mode)
  ;; :config
  ;; (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  )

;;;; Imenu

;; Automatically rescan the buffer for Imenu entries when `imenu' is invoked.
;; This ensures the index reflects recent edits.
(setq imenu-auto-rescan t)

;; Prevent truncation of long function names in `imenu' listings.
(setq imenu-max-item-length 160)

;;;; Completion

;; Ignore cases
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;;;;; hippie-expand

(use-package hippie-exp
  :custom
  (hippie-expand-try-functions-list '( try-expand-dabbrev
                                       try-expand-dabbrev-all-buffers
                                       try-expand-dabbrev-from-kill
                                       try-complete-file-name-partially
                                       try-complete-file-name
                                       try-expand-all-abbrevs
                                       try-expand-list
                                       try-expand-line
                                       try-complete-lisp-symbol-partially
                                       try-complete-lisp-symbol))
  :bind ([remap dabbrev-expand] . hippie-expand))

;;;; Comint (general command interpreter in a window)

(setq ansi-color-for-comint-mode t
      comint-prompt-read-only t
      comint-buffer-maximum-size 4096)

;;;; Compilation

(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

;;;; Eldoc

(blackout 'eldoc-mode)

;;;; image-mode

(setq image-animate-loop t)

;;;; occur-mode

;; Create separate *Occur* buffer for each search.
(add-hook 'occur-hook 'occur-rename-buffer)

;;;; version control

(setq vc-git-print-log-follow t
      vc-make-backup-files nil ; Do not backup version controlled files.
      vc-git-diff-switches '("--histogram")) ; Faster algorithm for diffing.

;; Remove RCS, CVS, SCCS, and Bzr, because it's a lot less work for vc to
;; check them all (especially in TRAMP buffers), and who uses any of these?
(setq vc-handled-backends '(Git Hg SVN SRC))

;; PERF: Ignore node_modules (expensive for vc ops to index).
(setq vc-ignore-dir-regexp (format "%s\\|%s"
                                   locate-dominating-stop-dir-regexp
                                   "[/\\\\]node_modules"))

(with-eval-after-load 'vc-annotate
  (keymap-set vc-annotate-mode-map "<remap> <quit-window>" #'kill-current-buffer))

;;;; which-key

(use-package which-key
  :hook (elpaca-after-init-hook . which-key-mode)
  :custom
  (which-key-lighter nil)
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

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

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode-hook conf-mode-hook ; text-mode-hook
  )

;;;; Prog-mode

(add-hook 'prog-mode-hook 'helheim-show-trailing-whitespace)
(add-hook 'prog-mode-hook 'helheim-show-fill-column-indicator)

(defun helheim-show-trailing-whitespace ()
  "Highlight trailing whitespaces with `trailing-whitespace' face.
Use `delete-trailing-whitespace' command."
  (setq-local show-trailing-whitespace t))

;; TODO: report BUG in `display-fill-column-indicator-mode': 2 strings
;;   are compared with `eq' instead of `equal' and result is always nil.
(defun helheim-show-fill-column-indicator ()
  "Display `fill-column' indicator."
  (setq-local display-fill-column-indicator t
              display-fill-column-indicator-character ?\u2502))

;;;; Extra file extensions to support

(add-to-list 'auto-mode-alist '("/LICENSE\\'" . text-mode))
(add-to-list 'auto-mode-alist '("rc\\'" . conf-mode) 'append)

;;;; Editing files with very long lines

(use-package so-long
  :ensure t
  :hook (emacs-startup-hook . global-so-long-mode)
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

;;; provide `helheim-editor'
(provide 'helheim-editor)
;;; helheim-editor.el ends here
