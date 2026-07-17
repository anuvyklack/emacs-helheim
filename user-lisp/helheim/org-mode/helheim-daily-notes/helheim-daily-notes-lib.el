;;; helheim-daily-notes-lib.el -*- lexical-binding: t -*-
;;; Code:

(require 'dash)
(require 'hel)
(require 'org)
(require 'calendar)

;; Defined by the base module `helheim-daily-notes', always loaded first.
(defvar helheimg-daily-directory)

(defun helheim-daily--file (&optional time)
  (expand-file-name (format-time-string "%Y-%m-%d.org" time)
                    helheimg-daily-directory))

(setopt org-default-notes-file (helheim-daily--file))

;;;###autoload
(defun helheim-daily--set-keys ()
  (when (helheim-daily-note-p)
    (hel-keymap-overriding-set :state 'normal
      "] ]" '("next daily note" . helheim-daily-next)
      "[ [" '("previous daily note" . helheim-daily-previous))))

;;;###autoload
(defun helheimg-daily-note (arg)
  "Open today's daily note.
With \\[universal-argument], interactively choose a date and open the daily note
for the specified date."
  (interactive "P")
  (let* ((time (if arg (helheim-daily-read-date)))
         (file (helheim-daily--file time))
         (exist (file-exists-p file)))
    (find-file file)
    (unless exist
      (insert (format-time-string "#+title:    %A, %d %b %Y" time) ?\n
              "#+category: daily" ?\n ?\n)
      (save-buffer))))

(defun helheim-daily-read-date ()
  (let* ((calendar-today-visible-hook '(helheim-daily--mark-in-calendar))
         (calendar-today-invisible-hook '(helheim-daily--mark-in-calendar))
         ;; (org-read-date-prefer-future nil)
         )
    (org-read-date nil t nil "Find daily-note: ")))

(defun helheim-daily--mark-in-calendar ()
  (dolist (file (helheim-daily-list-files))
    (-let* (((_ _ _ d m y _ _ _) (-> file
                                     (file-name-nondirectory)
                                     (file-name-sans-extension)
                                     (org-parse-time-string)))
            (date (list m d y)))
      (when (calendar-date-is-visible-p date)
        (calendar-mark-visible-date date 'lazy-highlight)))))

(defun helheim-daily-list-files ()
  "List all files recursively in `helheimg-daily-directory' sorted by date."
  (->> (directory-files-recursively helheimg-daily-directory
                                    (rx ".org" eos))
       (-remove (lambda (file)
                  (setq file (file-name-nondirectory file))
                  (or (auto-save-file-name-p file)
                      (backup-file-name-p file)
                      (string-match (rx bos ".") file))))
       (-sort (lambda (a b)
                (setq a (file-name-sans-extension (file-name-nondirectory a))
                      b (file-name-sans-extension (file-name-nondirectory b)))
                (string< a b)))))

(defun helheim-daily-note-p (&optional file)
  "Return t if FILE is an daily-note, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (-some-> (or file
               (buffer-file-name (buffer-base-buffer)))
    (expand-file-name)
    (file-in-directory-p helheimg-daily-directory)))

;;;###autoload
(defun helheim-daily-next (&optional n)
  "Find next daily-note.
With numeric argument N, find note N days in the future. If N is
negative, find note N days in the past."
  (interactive "p")
  (unless (helheim-daily-note-p)
    (user-error "Not in a daily note"))
  (let* ((dailies (helheim-daily-list-files))
         (file (buffer-file-name (buffer-base-buffer)))
         (index (-find-index (lambda (elem) (string= elem file))
                             dailies)))
    (unless index
      (user-error "Can't find current note file - have you saved it yet?"))
    (if (< 0 n)
        (when (eq index (- (length dailies) 1))
          (user-error "No next daily note"))
      ;; else
      (when (eq index 0)
        (user-error "No previous daily note")))
    (find-file (nth (+ index n) dailies))))

;;;###autoload
(defun helheim-daily-previous (&optional n)
  "Find previous daily-note.
With numeric argument N, find note N days in the past. If N is
negative, find note N days in the future."
  (interactive "p")
  (helheim-daily-next (- n)))

;;; .
(provide 'helheim-daily-notes-lib)
;;; helheim-daily-notes-lib.el ends here
