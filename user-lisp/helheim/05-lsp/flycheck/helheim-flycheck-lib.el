;;; helheim-flycheck-lib.el -*- lexical-binding: t -*-

(require 'dash)
(require 'hel)
(require 'flycheck)

(defun +flycheck-setup-eldoc-h ()
  "Pass flycheck diagnostics through Eldoc."
  (add-hook 'eldoc-documentation-functions '+flycheck-eldoc nil t))

(defun +flycheck-eldoc (callback &rest _)
  "Report flycheck errors at point to Eldoc via CALLBACK.
Intended for `eldoc-documentation-functions'."
  (when-let* ((errors (flycheck-overlay-errors-at (point))))
    (funcall callback
             (mapconcat (lambda (err)
                          (let* ((level (flycheck-error-level err))
                                 (face (flycheck-error-level-error-list-face level))
                                 (level-msg (-> (format "%s" level)
                                                (propertize 'face face))))
                            (concat level-msg ": " (flycheck-error-message err))))
                        errors "\n"))))

(defun +flycheck-reveal-error-at-point (&rest _)
  "Open closed overlays and pulse the error region."
  (-each (overlays-at (point)) #'hel-open-overlay)
  (when-let* ((ov (car (flycheck-overlays-at (point)))))
    (pulse-momentary-highlight-region (overlay-start ov)
                                      (overlay-end ov))))

;;; .
(provide 'helheim-flycheck '(lib))
;;; helheim-flycheck-lib.el ends here
