;;; fill-paragrap.el -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Inspired with:
;; https://github.com/alphapapa/unpackaged.el#flexibly-fillunfill-paragraphs
;;
;;; Code:

(require 'hel-lib)

(defvar +flex-fill-paragraph-column nil
  "Last fill column used in command `+flex-fill-paragraph'.")

;;;###autoload
(defun +flex-fill-paragraph (&optional justify)
  "Fill paragraph, incrementing fill column to cause a change when repeated.
The global value of `fill-column' is not modified; it is only bound around
calls to `fill-paragraph'.

When called for the first time in a sequence, unfill to the default
`fill-column'.

When called repeatedly, increase `fill-column' until filling changes.

With \\[universal-argument] justify paragraph."
  (interactive "P")
  (let ((fill-column (if (equal last-command this-command)
                         (or (+flex-fill-paragraph--next-fill-column)
                             fill-column)
                       fill-column)))
    (setq +flex-fill-paragraph-column fill-column)
    (call-interactively #'fill-paragraph)
    (message "Fill column: %s" fill-column)))

(defun +flex-fill-paragraph--next-fill-column ()
  "Return next `fill-column' value."
  (let* ((point (point))
         (region (hel-region))
         (source-buffer (current-buffer))
         (mode major-mode)
         (fill-column (or +flex-fill-paragraph-column fill-column))
         (old-fill-column fill-column)
         (hash (buffer-hash)))
    (with-temp-buffer
      (delay-mode-hooks
        (funcall mode))
      (setq-local fill-column old-fill-column)
      (insert-buffer-substring source-buffer)
      (if region
          (apply #'hel-set-region region)
        (goto-char point))
      (catch 'break
        (while (and (fill-paragraph nil region)
                    (string= hash (buffer-hash)))
          (if (< 100 (- fill-column old-fill-column))
              (throw 'break nil)
            (cl-incf fill-column)))
        fill-column))))

;;; fill-paragrap.el ends here
