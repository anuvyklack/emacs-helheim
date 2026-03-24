;;; helheim-lib.el -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (require 'cl-macs))
(require 'dash)
(require 's)

;;; Customize color themes
;;
;; Just load the theme you want with `load-theme' function. Thats all. You may
;; use it interactively: press ":" (Hel) or "M-x" (Emacs) and type `load-theme'.
;;
;; If you want to customize particular theme use `helheim-theme-set-faces'.
;; You may enable you customizations imediatly without restarting Emacs place
;; the cursor right after the closing bracket and evaluate the form with
;; "SPC e e" (Hel) or "C-x C-e" (Emacs).

(advice-add 'load-theme :around 'helheim-load-theme-a)

(defun helheim-load-theme-a (orig-fun theme &optional no-confirm no-enable)
  "Load THEME and all customizations made with `helheim-theme-set-faces'."
  (-each custom-enabled-themes #'disable-theme)
  (funcall orig-fun theme no-confirm no-enable)
  (when-let* ((faces (alist-get theme helheim-themes-faces)))
    (apply #'custom-theme-set-faces theme faces))
  (or no-enable
      (enable-theme theme)))

(defvar helheim-themes-faces nil
  "Per theme faces overrides made by `helheim-theme-set-faces'.
Alist of the form:

  ((theme1 . ((face1 . specs)
              (face2 . specs)))
   (theme2 . ((face1 . specs)
              (face2 . specs))))")

;; TODO: write docstring
(defun helheim-theme-set-faces (theme &rest specs)
  "Set FACE for THEME.
See `helheim/color-themes/helheim-modus-themes.el' for examples how to use
this function. (Place cursor somewhere inside path and press \"gf\" to let
Emacs magic happen.)

\(fn THEME [(FACE . SPECS)])"
  (declare (indent 1))
  (-each specs (-lambda ((face . spec))
                 (setf (->> helheim-themes-faces
                            (alist-get theme)
                            (alist-get face))
                       `(((t ,@spec))))))
  (when (memq theme custom-known-themes)
    (apply #'custom-theme-set-faces theme
           (alist-get theme helheim-themes-faces)))
  (when (memq theme custom-enabled-themes)
    (enable-theme theme)))

;;; Save minibuffer history between sessions

(defun helheim-savehist-unpropertize-variables-h ()
  "Remove text properties from tracked variables to reduce savehist cache size."
  (setq kill-ring (->> kill-ring
                       (-filter #'stringp)
                       (-map #'substring-no-properties)))
  ;; modify in place
  (-each register-alist (-lambda ((cons-cell &as _register . val))
                          (when (stringp val)
                            (setcdr cons-cell (substring-no-properties val))))))

(defun helheim-savehist-remove-unprintable-registers-h ()
  "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
  ;; Save new value in the temp buffer savehist is running
  ;; `savehist-save-hook' in. We don't want to actually remove the
  ;; unserializable registers in the current session!
  (setq-local register-alist (-filter #'savehist-printable register-alist)))

;;; Tree-sitter

;;;###autoload
(defun helheim-install-missing-treesit-grammars ()
  "Install all missing tree-sitter grammars."
  (interactive)
  (cl-loop for (lang . _) in treesit-language-source-alist
           unless (treesit-language-available-p lang)
           do (treesit-install-language-grammar lang)))

;;; LSP

(defun helheim-lsp ()
  (cond ((featurep 'helheim-eglot)
         (when (require 'eglot nil t)
           (if (eglot--lookup-mode major-mode)
               (eglot-ensure)
             (eglot--message "No language server found for %s" major-mode))))
        ((featurep 'helheim-lsp-mode)
         (lsp-deferred))))

;;; Utils

(defun +original-value (symbol)
  "Return the original value for SYMBOL, if any."
  ;; The code is taken from the `helpful' package. I have no idea why it’s
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
      (append (->> (buffer-local-value hook (current-buffer))
                   (delq t))
              (default-value hook))
    ;; else
    (ensure-list (symbol-value hook))))

(defun +buffer-file-name (&optional buffer)
  "Return the file name of a file-visiting buffer, the directory of a Dired
buffer, or nil otherwise.

No argument or nil as argument means use the current buffer."
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

;;; .
(provide 'helheim-lib)
;;; helheim-lib.el ends here
