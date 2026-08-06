;;; helheim-lib.el                                    -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (require 'cl-macs))
(require 'dash)
(require 'f)

;;; Commands

(defun helheim-regenerate-autoloads ()
  "Rescan `user-lisp-directory' for autoload cookies and regenarate autoloads file."
  (interactive)
  (loaddefs-generate (cons user-lisp-directory
                           (f-directories user-lisp-directory nil t))
                     (expand-file-name ".user-lisp-autoloads.el"
                                       user-lisp-directory)))

;;; Color themes customization
;;
;; Just load the theme you want with `load-theme' function. Thats all. You may
;; use it interactively: press ":" (Hel) or "M-x" (Emacs) and type `load-theme'.
;;
;; If you want to customize particular theme use `helheim-theme-set-faces'.
;; You may enable you customizations imediatly without restarting Emacs place
;; the cursor right after the closing bracket and evaluate the form with
;; ",ee" (Hel) or "C-x C-e" (Emacs).

(define-advice load-theme (:before (&rest _) helheim)
  "Disable all enabled themes before loading a new one."
  (-each custom-enabled-themes #'disable-theme))

(add-hook 'enable-theme-functions 'helheim-apply-theme-faces -90)
(add-hook 'enable-theme-functions 'helheim-refontify-buffers 90)

(defun helheim-apply-theme-faces (theme)
  "Apply helheim face overrides for THEME on top of its own faces."
  (when-let* ((faces (alist-get theme helheim-themes-faces))
              ((custom-theme-p theme)))
    (apply #'custom-theme-set-faces theme faces)))

(defun helheim-refontify-buffers (&rest _)
  "Mark every font-locked buffer for refontification."
  (-each (buffer-list)
    (lambda (buffer)
      (with-current-buffer buffer
        (when font-lock-mode
          (font-lock-flush))))))

(defvar helheim-themes-faces nil
  "Per theme faces overrides made by `helheim-theme-set-faces'.
Alist of the form:

  ((theme1 . ((face1 . specs)
              (face2 . specs)))
   (theme2 . ((face1 . specs)
              (face2 . specs))))")

(defun helheim-theme-set-faces (theme &rest specs)
  "Register face attribute overrides for THEME.

Each element of SPECS is a list (FACE :ATTR VALUE ...) naming a face
and its attribute plist — the same format `set-face-attribute' accepts.

If THEME is currently active changes takes effect immediatly.

\(fn THEME (FACE :ATTR VALUE ...) ...)"
  (declare (indent 1))
  (-each specs (-lambda ((face . spec))
                 (setf (->> helheim-themes-faces
                            (alist-get theme)
                            (alist-get face))
                       `(((t ,@spec))))))
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
  "Enable current line highlighting only in special modes that are in Hel Emacs
state, and temporary disable it when region is active."
  (with-current-buffer buffer
    (and (eq hel-state 'emacs)
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
  (when-let* ((missing (->> treesit-language-source-alist
                            (-map #'car)
                            (-remove #'treesit-language-available-p)))
              ((y-or-n-p
                (format "Install %d missing tree-sitter grammar(s)?\n"
                        (length missing)))))
    (let (failed)
      (-each missing (lambda (language)
                       (condition-case err
                           (treesit-install-language-grammar language)
                         (error
                          (push (cons language (error-message-string err))
                                failed)))))
      (when failed
        (let ((msg (concat (format "Failed to install %d tree-sitter grammar(s):\n"
                                   (length failed))
                           (->> (nreverse failed)
                                (-map (-lambda ((language . message))
                                        (format "  %s: %s" language message)))
                                (s-join "\n")))))
          (display-warning 'helheim msg :error))))))

(defun helheim-lsp ()
  (cond ((featurep 'helheim-eglot)
         (when (require 'eglot nil t)
           (if (eglot--lookup-mode major-mode)
               (eglot-ensure)
             (eglot--message "No language server found for %s" major-mode))))
        ((featurep 'helheim-lsp-mode)
         (lsp-deferred))))

;;; Window managment

(defun +display-buffer-based-on-window-count (buffer alist)
  "Display BUFFER based on the windows count.

  1 or 2    -> open in another window
  3 or more -> open in the current window

ALIST is the usual `display-buffer' action alist."
  (if (length< (window-list nil 'no-minibuffer) 3)
      (display-buffer-pop-up-window buffer alist)
    (display-buffer-same-window buffer alist)))

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

(defun +hook-values (hook)
  "Return list with all local and global elements of the HOOK.
HOOK should be a symbol."
  (if (local-variable-p hook)
      (append (->> (buffer-local-value hook (current-buffer))
                   (-remove-item t))
              (ensure-list (default-value hook)))
    ;; else
    (ensure-list (symbol-value hook))))

(defun +buffer-file-name (&optional buffer)
  "Return the file name of a file-visiting buffer, the directory of a Dired
buffer, or nil otherwise.

No argument or nil as argument means use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or (buffer-file-name)
        ;; Dired buffer
        (if-let* ((directory (pcase (bound-and-true-p dired-directory)
                               ((pred stringp) dired-directory)
                               (`(,dir . ,_) dir))))
            (expand-file-name directory))
        ;; Indirect buffer
        (if (buffer-base-buffer)
            list-buffers-directory))))

;;; .
(provide 'helheim-lib)
;;; helheim-lib.el ends here
