;;; helheim-dired-lib.el                              -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (require 'dash))
(require 'dired)
(require 'hel)

;;; Commands

;; j
(defun helheim-dired-next-line (count)
  (interactive "p" dired-mode)
  (if (region-active-p)
      (hel-expand-line-selection count)
    (dired-next-line count)))

;; k
(defun helheim-dired-previous-line (count)
  (interactive "p" dired-mode)
  (if (region-active-p)
      (hel-expand-line-selection (- count))
    (dired-previous-line count)))

;; v
(defun helheim-dired-toggle-selection ()
  "Toggle selection."
  (interactive nil dired-mode)
  (if (use-region-p)
      (deactivate-mark)
    (hel-expand-line-selection 1)))

;; To make it easier to find it in M-x menu.
(defalias '+dired-copy-file-name #'dired-copy-filename-as-kill)

(defun +dired-copy-file-path ()
  "Copy full file name (including path) into kill ring."
  (interactive nil dired-mode)
  (dired-copy-filename-as-kill 0))

(defun +dired-do-flagged-delete-permanently ()
  "Delete files permanently instead of trashing them."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (let ((delete-by-moving-to-trash nil))
    (dired-do-flagged-delete)))

(defalias '+dired-delete-permanently #'+dired-do-flagged-delete-permanently)

;;; dired-subtree

;; Make `dired-subtree-beginning' and `dired-subtree-end' commands
;; work also on the top level and not only in actual subtrees. This
;; also fix `dired-subtree-mark-subtree'.
(define-advice dired-subtree-beginning (:override () helheim)
  "Go to the first file in this subtree."
  (interactive)
  (if-let* ((ov (dired-subtree--get-ov)))
      (progn
        (goto-char (overlay-start ov))
        (dired-move-to-filename))
    ;; else
    (call-interactively #'dired-prev-subdir)
    (while (and (< (point) (point-max))
                (dired-between-files))
      (forward-line 1))))

(define-advice dired-subtree-end (:override () helheim)
  "Go to the last file in this subtree."
  (interactive)
  (if-let* ((ov (dired-subtree--get-ov)))
      (progn
        (goto-char (overlay-end ov))
        (dired-previous-line 1))
    (call-interactively #'dired-next-subdir)))

;;; image-dired

;; TODO: Upstream this.
;; FIX: Original `image-dired-show-all-from-dir' command is inaccurate written:
;;   it calls `image-dired-display-thumbs' which creates and displays
;;   `image-dired-thumbnail-buffer' with `pop-to-buffer' and calls
;;   `image-dired--update-header-line'. And then it does it again itself.
(define-advice image-dired-show-all-from-dir (:override (dir) helheim)
  "Make a thumbnail buffer for all images in DIR and display it.
Any file matching `image-dired--file-name-regexp' is considered an
image file."
  (interactive "DShow thumbnails for directory: ")
  (dired dir)
  (dired-mark-files-regexp (image-dired--file-name-regexp))
  (let ((files (dired-get-marked-files nil nil nil t)))
    (cond ((null (cdr files))
           (message "No image files in directory"))
          ((or (not image-dired-show-all-from-dir-max-files)
               (<= (length (cdr files)) image-dired-show-all-from-dir-max-files)
               (and (length> (cdr files) image-dired-show-all-from-dir-max-files)
                    (y-or-n-p
                     (format
                      "Directory contains more than %d image files.  Proceed?"
                      image-dired-show-all-from-dir-max-files))))
           (image-dired-display-thumbs)
           (let ((inhibit-message t))
             (dired-unmark-all-marks))
           (setq default-directory dir))
          (t
           (message "Image-Dired canceled")))))

;;; Prepend file name with ID

;;;###autoload
(defun helheim-dired-do-add-id ()
  "Prepend marked files names with timestamp based ID.
If file already has ID — do nothing."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (dolist (file (dired-get-marked-files))
    (unless (helheim-dired-file-id file)
      (helheim-file-add-id file)))
  (dired-revert))

;;;###autoload
(defun helheim-dired-file-id (file)
  "Return FILE's ID if it has one."
  (let ((filename (file-name-nondirectory file)))
    (if (string-match helheim-file-id-regexp filename)
        (match-string-no-properties 1 filename))))

(defun helheim-file-add-id (file)
  "Prepende FILE name with timebased ID.
FILE should be absolute filepath. The ID is based on FILE creation time.
If ID is taken, bump it one second until free one."
  (let* ((dir (file-name-directory file))
         (filename (file-name-nondirectory file))
         (time (let ((created (helheim-dired--file-creation-time file))
                     (modified (helheim-dired--file-modification-time file)))
                 (if (time-less-p created modified)
                     created
                   modified)))
         newname)
    (while (file-exists-p
            (setq newname (-> (format "%s--%s"
                                      (format-time-string helheim-id-format time)
                                      filename)
                              (expand-file-name dir))))
      (cl-callf time-add time 1))
    (rename-file file newname)))

(defun helheim-dired--file-creation-time (file)
  "Return the FILE creation time using the `stat' from coreutils."
  (-> (process-lines "stat" "--format=%w" file)
      (car)
      (parse-time-string)
      (encode-time)))

(defun helheim-dired--file-modification-time (filepath)
  (file-attribute-modification-time (file-attributes filepath)))

;;; .
(provide 'helheim-dired '(lib))
;;; helheim-dired-lib.el ends here
