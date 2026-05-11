;;; helheim-edit-indirect-lib.el                      -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Intorduce `helheim-edit-indirect' command, which differs from
;; `hel-narrow-to-region-indirectly' which clones buffer inidrectly with
;; narrowing in that the text properties are not shared, so the parent buffer
;; major mode and the edit-indirect buffer major mode will not be able to tread
;; on each other's toes by setting up potentially conflicting text properties,
;; which happens surprisingly often when the font-lock mode is used.
;;
;; When done, exit with `edit-indirect-commit', which will remove the original
;; region and replace it with the edited version; or with `edit-indirect-abort',
;; which will drop the modifications.
;;
;; Edit-indirect buffers use the `edit-indirect-mode-map' keymap. Regions with
;; active edit-indirect buffers use the `edit-indirect-overlay-map' keymap.
;;
;; If there's already an edit-indirect buffer for region, use that. If there's
;; already an edit-indirect buffer active overlapping any portion of region, an
;; `edit-indirect-overlapping' error is signaled.
;;
;;; Code:

(require 'edit-indirect)

;;;###autoload
(defun helheim-edit-region-indirect (arg)
  "Copy region without text properties, to a separate buffer.
With \\[universal-argument] ask which major mode to use in edit-indirect buffer.

The region is copied, without text properties, to a separate buffer. When done,
exit with `edit-indirect-commit', which will remove the original region and
replace it with the edited version; or with `edit-indirect-abort', which will
drop the modifications.

This differs from `hel-narrow-to-region-indirectly' which clones buffer
inidrectly with narrowing in that the text properties are not shared, so the
parent buffer major mode and the edit-indirect buffer major mode will not be
able to tread on each other's toes by setting up potentially conflicting text
properties, which happens surprisingly often when the font-lock mode is used.

Edit-indirect buffers use the `edit-indirect-mode-map' keymap. Regions with
active edit-indirect buffers use the edit-indirect-overlay-map keymap.

If there's already an edit-indirect buffer for region, use that. If there's
already an edit-indirect buffer active overlapping any portion of region, an
`edit-indirect-overlapping' error is signaled."
  (interactive "P")
  (unless (use-region-p) (user-error "No region selected"))
  (let ((beg (region-beginning))
        (end (region-end)))
    (deactivate-mark)
    (let ((mode (if current-prefix-arg
                    (completing-read "major mode: "
                                     (apropos-internal "-mode$" #'commandp)
                                     nil nil nil nil
                                     major-mode)
                  major-mode))
          (name (or buffer-file-name
                    list-buffers-directory))
          (vars (cl-loop for symbol in '(default-directory lexical-binding)
                         collect symbol
                         collect (symbol-value symbol))))
      (let ((buffer (edit-indirect-region beg end)))
        (set-buffer buffer)
        (funcall mode)
        (setq list-buffers-directory name)
        (eval `(setq-local ,@vars) t)
        (setq-local header-line-format
                    (substitute-command-keys
                     "Commit with \\[edit-indirect-commit], abort with \\[edit-indirect-abort]"))
        (beginning-of-buffer)
        (switch-to-buffer buffer)))))

(add-hook 'edit-indirect-after-creation-hook #'helheim-edit-indirect--dedent)
(add-hook 'edit-indirect-before-commit-hook  #'helheim-edit-indirect--indent)

(defun helheim-edit-indirect--dedent ()
  (setq-local helheim-edit-indirect--intentation (helheim-common-indentation))
  (save-excursion
    (indent-rigidly (point-min) (point-max)
                    (- helheim-edit-indirect--intentation))))

(defun helheim-edit-indirect--indent ()
  (when (boundp 'helheim-edit-indirect--intentation)
    (save-excursion
      (indent-rigidly (point-min) (point-max)
                      helheim-edit-indirect--intentation))))

(defun helheim-common-indentation ()
  "Return the common indentation off all lines in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((indentation 0))
      (while (not (eobp))
        (unless (s-blank-str? (thing-at-point 'line))
          (cl-callf min indentation (current-indentation)))
        (forward-line))
      indentation)))

;;; .
(provide 'helheim-edit-indirect '(lib))
;;; helheim-edit-indirect-lib.el ends here
