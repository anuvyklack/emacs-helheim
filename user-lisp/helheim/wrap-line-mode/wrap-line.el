;;; wrap-line.el                                      -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This module defines `+wrap-line-mode' — a minor-mode, which visually
;; wraps long lines at `fill-column' in the buffer without modifying the
;; buffer content.
;;
;;; Customization

(defcustom +wrap-line-extra-indent 'double
  "The amount of extra indentation for wrapped code lines.

`double'    indent by twice the major-mode indentation
`single'    indent by the major-mode indentation
integer   indent (or dedent if negative) by this fixed amount
nil       no extra indentation"
  :type '(choice (const :tag "Double indentation" double)
                 (const :tag "Single indentation" single)
                 integer)
  :group 'helheim)

(defcustom +wrap-line-ignored-modes '(fundamental-mode
                                      so-long-mode)
  "Major-modes where `+global-wrap-line-mode' should not enable `+wrap-line-mode'.")

(defcustom +wrap-line-visual-modes '(org-mode)
  "Major-modes where `+wrap-line-mode' should not use `adaptive-wrap-prefix-mode'.")

(defcustom +wrap-line-text-modes '(text-mode
                                   markdown-mode markdown-view-mode
                                   gfm-mode gfm-view-mode
                                   rst-mode
                                   latex-mode LaTeX-mode)
  "Major-modes where `+wrap-line-mode' should not provide extra indentation.")

;;; Code

(defvar-local +wrap-line--major-mode-is-text nil)
(defvar-local +wrap-line--major-mode-indent-var nil)

;;;###autoload
(define-minor-mode +wrap-line-mode
  "Wrap long lines in the buffer with language-aware indentation.

This mode configures `adaptive-wrap', `visual-line-mode' and
`visual-fill-column-mode' to wrap long lines without modifying the buffer
content. This is useful when dealing with legacy code which contains
gratuitously long lines, or running emacs on your wrist-phone.

Wrapped lines will be indented to match the preceding line. In code buffers,
lines which are not inside a string or comment will have additional indentation
according to the configuration of `+wrap-line-extra-indent'."
  :init-value nil
  (if +wrap-line-mode
      (progn
        (setq +wrap-line--major-mode-is-text (memq major-mode +wrap-line-text-modes))
        (visual-line-mode 1)
        (visual-fill-column-mode 1)
        (auto-fill-mode -1)
        (unless (memq major-mode +wrap-line-visual-modes)
          (when (require 'dtrt-indent nil t)
            ;; for dtrt-indent--search-hook-mapping
            ;; TODO: Generalize this?
            (setq +wrap-line--major-mode-indent-var
                  (let ((indent-var (caddr (dtrt-indent--search-hook-mapping major-mode))))
                    (if (listp indent-var)
                        (car indent-var)
                      indent-var)))
            (advice-add 'adaptive-wrap-fill-context-prefix :around
                        #'+wrap-line--adjust-extra-indent-a))
          (adaptive-wrap-prefix-mode 1)))
    ;; else
    (visual-line-mode -1)
    (visual-fill-column-mode -1)
    (auto-fill-mode 1)
    (unless (memq major-mode +wrap-line-visual-modes)
      (advice-remove 'adaptive-wrap-fill-context-prefix #'+wrap-line--adjust-extra-indent-a)
      (adaptive-wrap-prefix-mode -1))))

;;;###autoload
(define-globalized-minor-mode +global-wrap-line-mode +wrap-line-mode
  +wrap-line--initialize)

(defun +wrap-line--initialize ()
  "Turn on `+wrap-line-mode' in current buffer if appropriate."
  (unless (or (eq 'special (get major-mode 'mode-class))
              (memq major-mode +wrap-line-ignored-modes))
    (+wrap-line-mode 1)))

(defvar adaptive-wrap-extra-indent)

(defun +wrap-line--adjust-extra-indent-a (fn beg end)
  "Contextually adjust extra wrap-line indentation."
  (let ((adaptive-wrap-extra-indent (+wrap-line--calc-extra-indent beg)))
    (funcall fn beg end)))

(defun +wrap-line--calc-extra-indent (position)
  "Calculate extra wrap-line indentation at POSITION."
  (if (or +wrap-line--major-mode-is-text
          (hel-comment-at-pos-p position)
          (hel-string-at-pos-p position))
      0
    (pcase +wrap-line-extra-indent
      ('double (* 2 (symbol-value +wrap-line--major-mode-indent-var)))
      ('single (symbol-value +wrap-line--major-mode-indent-var))
      ((and (pred integerp) fixed)
       fixed)
      (_ 0))))

;;; .
(provide 'wrap-line)
;;; wrap-line.el ends here
