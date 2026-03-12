;;; helheim-ibuffer.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Code:
(require 'dash)
(require 'hel-core)

;;; Customization

(defcustom helheim-ibuffer-not-project-buffer-filters
  `((mode . (help-mode helpful-mode debugger-mode backtrace-mode))
    (name . ,(eval-when-compile
               (rx string-start
                   (or "*Ibuffer*" "*Messages*" "*Warnings*" "*scratch*")
                   string-end))))
  "TODO"
  :type 'list
  :group 'helheim)

(defcustom helheim-ibuffer-format-project-group-name
  (lambda (directory)
    (format "%s : %s" ;; "%s  %s"
            (file-name-nondirectory (directory-file-name directory))
            directory))
  "TODO"
  :type 'function
  :group 'helheim)

;;; Keybindings

;; TODO: Duplicate all keys, so user doesn’t need to look up the original
;;   bindings and mentally merge them with these.
(with-eval-after-load 'ibuffer
  (hel-keymap-set ibuffer-mode-map
    "C-c f f" 'ibuffer-find-file

    "`" 'ibuffer-switch-format ; Type f you want to see the full buffer name.

    ;; Inherit hl keys from `special-mode-map'.
    "h" nil
    "l" nil ;; was `ibuffer-redisplay'

    "j" 'ibuffer-forward-line  ; was `ibuffer-jump-to-buffer' moved to "/"
    "k" 'ibuffer-backward-line ; was `ibuffer-do-kill-lines' moved to "K"

    ">" 'ibuffer-forward-next-marked
    "<" 'ibuffer-backwards-next-marked

    "/" 'ibuffer-jump-to-buffer ; was `ibuffer--filter-map' moved to "f"
    "f" `("filter" . ,ibuffer--filter-map)
    "K" 'ibuffer-do-kill-lines

    "d"   'ibuffer-mark-for-delete
    "M-d" 'ibuffer-mark-for-delete-backwards
    "x"   'ibuffer-do-kill-on-deletion-marks

    "v"     'ibuffer-do-view-horizontally
    "C-x v" 'ibuffer-do-view

    "g" (define-keymap
          "r" 'ibuffer-update
          "R" 'ibuffer-redisplay
          "d" 'ibuffer-do-delete
          "s" 'ibuffer-do-save
          "v" 'ibuffer-do-view-horizontally
          "z" 'ibuffer-bury-buffer
          "/" 'ibuffer-do-replace-regexp)

    ;; Filter groups
    "TAB" 'ibuffer-toggle-filter-group
    "z f" 'ibuffer-jump-to-filter-group

    "C-j" 'ibuffer-forward-filter-group
    "C-k" 'ibuffer-backward-filter-group
    "z j" 'ibuffer-forward-filter-group
    "z k" 'ibuffer-backward-filter-group
    "z u" 'ibuffer-backward-filter-group

    "}"   'ibuffer-forward-filter-group
    "{"   'ibuffer-backward-filter-group
    "] ]" 'ibuffer-forward-filter-group
    "[ [" 'ibuffer-backward-filter-group))

;;; Config

;; <leader> b b
(define-advice ibuffer-jump (:after (&optional _other-window) helheim)
  (ibuffer-switch-to-saved-filter-groups "Project"))

;; <leader> p C-b
(setq project-buffers-viewer 'helheim-ibuffer-project-buffers)

(leaf ibuffer
  :custom
  (ibuffer-truncate-lines . t)
  (ibuffer-show-empty-filter-groups . nil) ; Don't show emtpy filter groups
  ;; (ibuffer-display-summary . nil)
  ;; (ibuffer-movement-cycle . nil)
  (ibuffer-old-time . 2) ;; hours
  (ibuffer-directory-abbrev-alist . `((,abbreviated-home-dir . "~/")))
  (ibuffer-default-sorting-mode . 'filename/process)
  ;; (ibuffer-read-only-char . ?%)
  ;; (ibuffer-modified-char  . ?*)
  ;; (ibuffer-marked-char    . ?>)
  ;; (ibuffer-locked-char    . ?L)
  ;; (ibuffer-deletion-char  . ?D)
  :hook
  (ibuffer-mode-hook . ibuffer-auto-mode) ;; Automatically update Ibuffer.
  :defer-config
  (require 'mule-util)
  (require 'ibuf-ext)
  (setopt ibuffer-eliding-string (truncate-string-ellipsis)
          ibuffer-maybe-show-predicates (list #'helheim-ibuffer-maybe-show-p))
  (cl-pushnew 'helpful-mode ibuffer-help-buffer-modes))

(advice-add 'ibuffer-generate-filter-groups
            :before #'helheim-ibuffer-update-project-filter-groups)

(setq ibuffer-formats
      `(( mark modified read-only locked
          " " (icon 2 2 :left :elide)
          ,(propertize " " 'display `(space :align-to 8))
          (helheim-name 26 26 :left :elide)
          "  " ;; 2 spaces
          ;; (mode 18 18 :left :elide)
          ;; " "
          project-relative-filename-or-process)
        ( mark modified read-only locked
          " " (icon 2 2 :left :elide)
          ,(propertize " " 'display `(space :align-to 8))
          helheim-name)))

;;;; Custom columns

;; Icons column
(define-ibuffer-column icon
  ( :name "  ")
  (if-let* ((icon (nerd-icons-icon-for-buffer))
            ((stringp icon)))
      icon
    (nerd-icons-icon-for-mode 'fundamental-mode)))

(with-eval-after-load 'ibuffer
  (define-ibuffer-column helheim-name
    ( :inline t
      :header-mouse-map ibuffer-name-header-map
      :props ( 'mouse-face 'highlight
               'keymap ibuffer-name-map
	       'ibuffer-name-column t
	       'help-echo '(if tooltip-mode
			       "mouse-1: mark this buffer\nmouse-2: select this buffer\nmouse-3: operate on this buffer"
			     "mouse-1: mark buffer   mouse-2: select buffer   mouse-3: operate"))
      :summarizer (lambda (strings)
                    (pcase (length strings)
                      (0 "No buffers")
                      (1 "1 buffer")
                      (n (format "%s buffers" n)))))
    (let ((name (helheim-ibuffer-buffer-name)))
      (string-replace "\n" (propertize "^J" 'font-lock-face 'escape-glyph)
                      (propertize name 'font-lock-face (ibuffer-buffer-name-face
                                                        buffer mark))))))

(defun helheim-ibuffer-buffer-name (&optional buffer)
  "Return the name of BUFFER with ID stripped from it.
ID is determined by `helheim-file-id-regexp'.
BUFFER defaults to the current buffer."
  (let ((name (buffer-name buffer)))
    (if (string-match helheim-file-id-regexp name)
        (substring-no-properties name (match-end 0))
      name)))

;; Render filenames relative to project root
(with-eval-after-load 'ibuffer
  (define-ibuffer-column project-relative-filename-or-process
    ( :name "Filename/Process"
      :header-mouse-map ibuffer-filename/process-header-map
      :summarizer (lambda (strings)
                    (cl-callf2 delete "" strings)
                    (let ((procs (-count (lambda (s)
                                           (get-text-property 1 'ibuffer-process s))
                                         strings)))
                      (concat (pcase (- (length strings) procs)
                                (0 "No files")
                                (1 "1 file")
                                (n (format "%d files" n)))
                              (pcase procs
                                (0 "")
                                (1 ", 1 process")
                                (_ (format ", %d processes" procs)))))))
    (let ((filename (ibuffer-make-column-filename buffer mark)))
      (or (if-let ((proc (get-buffer-process buffer)))
              (concat (propertize (format "(%s %s)" proc (process-status proc))
                                  'font-lock-face 'italic
                                  'ibuffer-process proc)
                      (if (length> filename 0)
                          (format " %s" filename)
                        "")))
          (if-let ((root (-some-> (project-current) (project-root))))
              (let ((filename (file-relative-name filename root)))
                (if (or (member filename '("./" ".")))
                    ""
                  filename)))
          (abbreviate-file-name filename)))))

;;; Functions

(defun helheim-ibuffer-maybe-show-p (buffer)
  (with-current-buffer buffer
    (or (memq major-mode '(completion-list-mode))
        (string-match-p (rx string-start "*Semantic SymRef*" string-end)
                        (buffer-name))
        (and (string-match "^ " (buffer-name))
	     (null (buffer-file-name))))))

(defun helheim-opened-projects ()
  "Return list with all opened projects."
  (->> (buffer-list)
       (-map (lambda (buffer)
               (unless (string-match-p "^ " (buffer-name buffer))
                 (with-current-buffer buffer
                   (project-current)))))
       (delq nil)
       (-uniq)))

(defun helheim-ibuffer-update-project-filter-groups (&rest _)
  "Update \"Project\" filter groups."
  (require 'ibuf-ext)
  (setf (alist-get "Project" ibuffer-saved-filter-groups nil nil #'equal)
        (nconc (->> (helheim-opened-projects)
                    (-map #'project-root)
                    ;; Sort projects roots by length (longest first) to group
                    ;; files for nested projects correctly.
                    (-sort #'string>)
                    (-map (lambda (dir)
                            `(,(funcall helheim-ibuffer-format-project-group-name dir)
                              (directory . ,(expand-file-name dir))
                              (not (or ,@helheim-ibuffer-not-project-buffer-filters))))))
               (list '("Files" (or (visiting-file)
                                   (mode . dired-mode)))))))

(defun helheim-ibuffer-project-buffers (project &optional files-only)
  "List buffers for PROJECT using Ibuffer.
If FILES-ONLY is non-nil, only show the file-visiting buffers."
  (ibuffer nil (format "*Ibuffer-%s*" (project-name project))
           `((not (or ,@helheim-ibuffer-not-project-buffer-filters))
             (predicate . (and (or ,(not files-only) buffer-file-name)
                               (memq (current-buffer)
                                     (project-buffers ',project)))))))

;;; Fixes for built-in filters

;; TODO: Upstream this.
(with-eval-after-load 'ibuf-ext
  ;; FIX: 'mode' filter should accept symbol or list of symbols according to
  ;;   documentation, but actually it accepts only symbol.
  (setf (alist-get 'mode ibuffer-filtering-alist)
        (list "major mode"
              (lambda (buffer qualifier)
                (condition-case nil
                    (memq (buffer-local-value 'major-mode buffer)
                          (ensure-list qualifier))
                  (error (ibuffer-pop-filter))))))

  ;; FIX: `default-directory' can start with "~" on UNIX, but current
  ;;   implementation ignores it.
  (setf (alist-get 'directory ibuffer-filtering-alist)
        (list "directory name"
              (lambda (buffer qualifier)
                (condition-case nil
                    (with-current-buffer buffer
                      (if-let ((dir (if-let ((filename (ibuffer-buffer-file-name)))
                                        (file-name-directory filename)
                                      default-directory)))
                          (string-match qualifier (expand-file-name dir))))
                  (error (ibuffer-pop-filter)))))))

;; Improve `filename/process' sorter.
(with-eval-after-load 'ibuf-ext
  (setf (alist-get 'filename/process ibuffer-sorting-functions-alist)
        (list "file name"
              (lambda (a b)
                (cl-callf car a)
                (cl-callf car b)
                (let ((a-file (helheim-buffer-file-name a))
                      (b-file (helheim-buffer-file-name b)))
                  (cond
                   ;; Indirect buffers -- two buffers for the same file.
                   ((and a-file (equal a-file b-file))
                    (string< (buffer-name a) (buffer-name b)))
                   ((and a-file b-file)
                    (let ((a-dir (file-name-directory a-file))
                          (b-dir (file-name-directory b-file)))
                      (if (string= a-dir b-dir)
                          (string< a-file b-file)
                        (string< a-dir b-dir))))
                   ((or a-file b-file)
                    (not b-file))
                   (t
                    (let* ((a-name (buffer-name a))
                           (b-name (buffer-name b))
                           (a-asterisk (string-match-p "^\\*" a-name))
                           (b-asterisk (string-match-p "^\\*" b-name)))
                      (if (xor a-asterisk b-asterisk)
                          (not b-asterisk)
                        (string< a-name b-name))))))))))

(defun helheim-buffer-file-name (&optional buffer)
  "Return file name for file-visiting buffers, or directory for Dired buffers.
Otherwise return nil."
  (with-current-buffer (or buffer (current-buffer))
    (or (buffer-file-name)
        ;; Dired buffer
        (if-let ((directory (pcase (bound-and-true-p dired-directory)
                              ((and (pred stringp) dir) dir)
                              (`(,dir . ,_) dir))))
            (expand-file-name directory))
        ;; Indirect buffer
        (if (buffer-base-buffer)
            list-buffers-directory))))

(provide 'helheim-ibuffer)
;;; helheim-ibuffer.el ends here
