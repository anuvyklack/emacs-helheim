;;; helheim-outline.el            -*- lexical-binding: t; no-byte-compile: t -*-
;;; Config

(setup outline
  (:install outli :host github :repo "jdtsmith/outli")
  (:blackout outline-mode
             outline-minor-mode)
  (:hook emacs-lisp-mode-hook outli-mode))

(with-eval-after-load 'consult-imenu
  (cl-pushnew '(?h "Headings" font-lock-comment-face)
              (-> consult-imenu-config
                  (map-elt 'emacs-lisp-mode)
                  (map-elt :types))
              :test #'equal))

;;; .
(provide 'helheim-outline)
;;; helheim-outline.el ends here
