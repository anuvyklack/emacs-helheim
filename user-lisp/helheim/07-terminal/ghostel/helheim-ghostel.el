;;; helheim-ghostel.el            -*- lexical-binding: t; no-byte-compile: t -*-

(setup ghostel
  (:install t)
  (:require t)
  (:setopt ghostel-module-auto-install 'ask)
  (:hook ghostel-mode-hook (lambda ()
                             ;; Do not enable current line highlighting in
                             ;; ghostel emacs and copy modes.
                             (setq ghostel--saved-hl-line-mode nil)))
  (:global-bind
    "C-c o t" 'ghostel
    ;; ghostel-next
    ;; ghostel-previous
    ;; ghostel-list-buffers
    ;; ghostel-project-list-buffers
    ))

(setup project
  (:after-load
    (add-to-list 'project-kill-buffer-conditions '(derived-mode . ghostel-mode))
    (keymap-set project-prefix-map "t" 'ghostel-project)
    (when-let* ((idx (-elem-index '(project-eshell "Eshell") project-switch-commands)))
      (cl-callf ->> project-switch-commands
        (-insert-at (1+ idx) '(ghostel-project "Terminal"))))))

(setup hel-ghostel
  (:install hel-ghostel :host github :repo "anuvyklack/hel-ghostel")
  (:after ghostel)
  (:require t))

;;; .
(provide 'helheim-ghostel)
;;; helheim-ghostel.el ends here
