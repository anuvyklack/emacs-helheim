;;; helheim-ibuffer-lib.el                            -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'ibuf-macs))
(require 'dash)
(require 'ibuffer)
(require 'ibuf-ext)

;;; Custom columns

;; Icons column
(define-ibuffer-column icon
  ( :name "  ")
  (if-let* ((icon (nerd-icons-icon-for-buffer))
            ((stringp icon)))
      icon
    (nerd-icons-icon-for-mode 'fundamental-mode)))

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
                                                      buffer mark)))))

;; Render filenames relative to project root
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
    (or (if-let* ((proc (get-buffer-process buffer)))
            (concat (propertize (format "(%s %s)" proc (process-status proc))
                                'font-lock-face 'italic
                                'ibuffer-process proc)
                    (if (length> filename 0)
                        (format " %s" filename)
                      "")))
        (if-let* ((root (-some-> (project-current) (project-root))))
            (let ((filename (file-relative-name filename root)))
              (if (or (member filename '("./" ".")))
                  ""
                filename)))
        (abbreviate-file-name filename))))

;;; Project filter groups

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
       (-non-nil)
       (-uniq)))

(defun helheim-ibuffer-update-project-filter-groups (&rest _)
  "Update \"Project\" filter groups."
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
               '(("Notmuch" (mode . (notmuch-hello-mode
                                     notmuch-search-mode
                                     notmuch-tree-mode
                                     notmuch-show-mode
                                     notmuch-message-mode)))
                 ("Files" (or (visiting-file)
                              (mode . dired-mode)))))))

;;;###autoload
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
                    (if-let* ((dir (if-let ((filename (ibuffer-buffer-file-name)))
                                       (file-name-directory filename)
                                     default-directory)))
                        (string-match qualifier (expand-file-name dir))))
                (error (ibuffer-pop-filter))))))

;; Improve `filename/process' sorter.
(setf (alist-get 'filename/process ibuffer-sorting-functions-alist)
      (list "file name"
            (lambda (a b)
              (cl-callf car a)
              (cl-callf car b)
              (let ((a-file (+buffer-file-name a))
                    (b-file (+buffer-file-name b)))
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
                      (string< a-name b-name)))))))))

(defun helheim-ibuffer-buffer-name (&optional buffer)
  "Return the name of BUFFER with ID stripped from it.
ID is determined by `helheim-file-id-regexp'.
BUFFER defaults to the current buffer."
  (let ((name (buffer-name buffer)))
    (if (string-match helheim-file-id-regexp name)
        (substring-no-properties name (match-end 0))
      name)))

;;; .
(provide 'helheim-ibuffer '(lib))
;;; helheim-ibuffer-lib.el ends here
