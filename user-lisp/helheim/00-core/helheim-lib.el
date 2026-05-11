;;; helheim-lib.el                                    -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (require 'cl-macs))
(require 'dash)

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
  "Disable all other themes, load and enable THEME and all customizations made
with `helheim-theme-set-faces'."
  (-each custom-enabled-themes #'disable-theme)
  (funcall orig-fun theme no-confirm no-enable)
  (when-let* ((faces (alist-get theme helheim-themes-faces)))
    (apply #'custom-theme-set-faces theme faces))
  (unless no-enable
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

;;; Hooks and Advices

(when (< emacs-major-version 31)
  (defvar global-hl-line-buffers)
  (defun helheim-global-hl-line-highlight-a (orig-fun)
    (if (buffer-match-p global-hl-line-buffers (current-buffer))
        (funcall orig-fun)
      (global-hl-line-unhighlight))))

(defun helheim--global-hl-line-buffers-p (buffer)
  "Enable current line highlighting only in special modes that are in Hel motion
state, and temporary disable it when region is active."
  (with-current-buffer buffer
    (and (eq hel-state 'motion)
         (not (use-region-p)))))

(defun helheim-make-hashed-backup-file-name-a (orig-fun file)
  "Make sure backup file names aren't too long."
  (let ((backup-file (funcall orig-fun file)))
    (if-let* ((backup-directory (map-some (lambda (_regexp directory)
                                            (if (string-match directory file)
                                                directory))
                                          backup-directory-alist))
              ((file-name-absolute-p backup-directory)))
        (expand-file-name (sha1 (file-name-nondirectory backup-file))
                          (file-name-directory backup-file))
      ;; else
      backup-file)))

(defun helheim-create-missing-directories-h ()
  "Automatically create missing directories when creating new files."
  (unless (file-remote-p buffer-file-name)
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent-directory))
           (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                             parent-directory))
           (progn (make-directory parent-directory 'parents)
                  t)))))

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

(defun helheim-install-missing-treesit-grammars ()
  "Install all missing tree-sitter grammars."
  (interactive)
  (cl-loop for (lang . _) in treesit-language-source-alist
           unless (treesit-language-available-p lang)
           do (treesit-install-language-grammar lang)))

(defun helheim-lsp ()
  (cond ((featurep 'helheim-eglot)
         (when (require 'eglot nil t)
           (if (eglot--lookup-mode major-mode)
               (eglot-ensure)
             (eglot--message "No language server found for %s" major-mode))))
        ((featurep 'helheim-lsp-mode)
         (lsp-deferred))))

(defun +posframe-poshandler-window-bottom-right-corner-with-padding (info)
  "Posframe's position handler.
The structure of INFO can be found in docstring of `posframe-show'."
  (let* ((window-left      (plist-get info :parent-window-left))
         (window-top       (plist-get info :parent-window-top))
         (window-width     (plist-get info :parent-window-width))
         (window-height    (plist-get info :parent-window-height))
         (posframe-width   (plist-get info :posframe-width))
         (posframe-height  (plist-get info :posframe-height))
         (mode-line-height (plist-get info :mode-line-height)))
    (cons (- (+ window-left window-width)
             posframe-width 20)
          (- (+ window-top window-height)
             mode-line-height posframe-height 20))))

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
                   (-remove-item t))
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
