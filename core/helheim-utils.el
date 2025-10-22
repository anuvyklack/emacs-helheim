;;; helheim-utils.el -*- lexical-binding: t; -*-
;;; Code:
;; (require 's)
(require 'dash)

(defmacro +theme-set-faces (theme &rest specs)
  "For THEME configure FACE with SPECS.

\(fn THEME &rest (FACE . SPECS))"
  (declare (indent 1))
  `(apply #'custom-theme-set-faces ,theme
          (mapcar (-lambda ((face . spec))
                    `(,face ((t ,spec))))
                  (list ,@specs))))

(defun +original-value (symbol)
  "Return the original value for SYMBOL, if any."
  ;; This code is taken from the `helpful' package. I have no idea why it’s
  ;; written this way, but the original author seems to be a very proficient
  ;; Elisp hacker.
  (let ((orig-val-expr (get symbol 'standard-value)))
    (if (consp orig-val-expr)
        (ignore-errors
          (eval (car orig-val-expr))))))

(defun +hook-values (hook)
  "Return list with all local and global elements of the HOOK.
HOOK should be a symbol."
  (if (local-variable-p hook)
      (append (-remove (lambda (x) (eq x 't))
                       (buffer-local-value hook (current-buffer)))
              (default-value hook))
    ;; else
    (ensure-list (symbol-value hook))))

(provide 'helheim-utils)
;;; helheim-utils.el ends here
