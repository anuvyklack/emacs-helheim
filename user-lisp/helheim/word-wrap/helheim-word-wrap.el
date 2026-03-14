;;; helheim-word-wrap.el -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This module defines `+word-wrap-mode' minor-mode, which intelligently
;; wraps long lines in the buffer without modifying the buffer content.
;;
;; Ported from DOOM Emacs.
;;
;;; Customization

(defcustom +word-wrap-extra-indent 'double
  "The amount of extra indentation for wrapped code lines.

`double'    indent by twice the major-mode indentation
`single'    indent by the major-mode indentation
integer   indent (or dedent if negative) by this fixed amount
nil       no extra indentation"
  :type '(choice (const :tag "Double indentation" double)
                 (const :tag "Single indentation" single)
                 integer)
  :group 'helheim)

(defcustom +word-wrap-ignored-modes '(fundamental-mode
                                      so-long-mode)
  "Major-modes where `+global-word-wrap-mode' should not enable `+word-wrap-mode'.")

(defcustom +word-wrap-visual-modes '(org-mode)
  "Major-modes where `+word-wrap-mode' should not use `adaptive-wrap-prefix-mode'.")

(defcustom +word-wrap-text-modes '(text-mode
                                   markdown-mode markdown-view-mode
                                   gfm-mode gfm-view-mode
                                   rst-mode
                                   latex-mode LaTeX-mode)
  "Major-modes where `+word-wrap-mode' should not provide extra indentation.")

;; (when (memq 'visual-line-mode text-mode-hook)
;;   (remove-hook 'text-mode-hook #'visual-line-mode)
;;   (add-hook 'text-mode-hook #'+word-wrap-mode))

;;; Code

(defvar-local +word-wrap--major-mode-is-text nil)
(defvar-local +word-wrap--major-mode-indent-var nil)

;;;###autoload
(define-minor-mode +word-wrap-mode
  "Wrap long lines in the buffer with language-aware indentation.

This mode configures `adaptive-wrap', `visual-line-mode' and
`visual-fill-column-mode' to wrap long lines without modifying the buffer
content. This is useful when dealing with legacy code which contains
gratuitously long lines, or running emacs on your wrist-phone.

Wrapped lines will be indented to match the preceding line. In code buffers,
lines which are not inside a string or comment will have additional indentation
according to the configuration of `+word-wrap-extra-indent'."
  :init-value nil
  (if +word-wrap-mode
      (progn
        (setq +word-wrap--major-mode-is-text (memq major-mode +word-wrap-text-modes))
        (visual-line-mode 1)
        (visual-fill-column-mode 1)
        (auto-fill-mode -1)
        (unless (memq major-mode +word-wrap-visual-modes)
          (when (require 'dtrt-indent nil t)
            ;; for dtrt-indent--search-hook-mapping
            ;; TODO: Generalize this?
            (setq +word-wrap--major-mode-indent-var
                  (let ((indent-var (caddr (dtrt-indent--search-hook-mapping major-mode))))
                    (if (listp indent-var)
                        (car indent-var)
                      indent-var)))
            (advice-add 'adaptive-wrap-fill-context-prefix :around
                        #'+word-wrap--adjust-extra-indent-a))
          (adaptive-wrap-prefix-mode 1)))
    ;; else
    (visual-line-mode -1)
    (visual-fill-column-mode -1)
    (auto-fill-mode 1)
    (unless (memq major-mode +word-wrap-visual-modes)
      (advice-remove 'adaptive-wrap-fill-context-prefix #'+word-wrap--adjust-extra-indent-a)
      (adaptive-wrap-prefix-mode -1))))

;;;###autoload
(define-globalized-minor-mode +global-word-wrap-mode +word-wrap-mode
  +word-wrap--initialize)

(defun +word-wrap--initialize ()
  "Turn on `+word-wrap-mode' in current buffer if appropriate."
  (unless (or (eq 'special (get major-mode 'mode-class))
              (memq major-mode +word-wrap-ignored-modes))
    (+word-wrap-mode 1)))

(defvar adaptive-wrap-extra-indent)

(defun +word-wrap--adjust-extra-indent-a (fn beg end)
  "Contextually adjust extra word-wrap indentation."
  (let ((adaptive-wrap-extra-indent (+word-wrap--calc-extra-indent beg)))
    (funcall fn beg end)))

(defun +word-wrap--calc-extra-indent (position)
  "Calculate extra word-wrap indentation at POSITION."
  (if (or +word-wrap--major-mode-is-text
          (hel-comment-at-pos-p position)
          (hel-string-at-pos-p position))
      0
    (pcase +word-wrap-extra-indent
      ('double (* 2 (symbol-value +word-wrap--major-mode-indent-var)))
      ('single (symbol-value +word-wrap--major-mode-indent-var))
      ((and (pred integerp) fixed)
       fixed)
      (_ 0))))

;;; .
(provide 'helheim-word-wrap)
;;; helheim-word-wrap.el ends here
