;;; helheim-ghostel.el            -*- lexical-binding: t; no-byte-compile: t -*-

(setup ghostel
  (:install t)
  (:require t)
  (:setopt ghostel-module-auto-install 'ask)
  (:hook ghostel-mode-hook (lambda ()
                             ;; Do not enable current line highlighting in
                             ;; ghostel emacs and copy modes.
                             (setq ghostel--saved-hl-line-mode nil))))

(setup hel-ghostel
  (:install hel-ghostel :host github :repo "anuvyklack/hel-ghostel")
  (:after ghostel)
  (:require t))

;;; .
(provide 'helheim-ghostel)
;;; helheim-ghostel.el ends here
