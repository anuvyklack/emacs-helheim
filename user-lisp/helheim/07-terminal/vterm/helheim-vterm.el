;;; helheim-vterm.el              -*- lexical-binding: t; no-byte-compile: t -*-

(setup vterm
  (:install t)
  (:when (bound-and-true-p module-file-suffix)) ; requires dynamic-modules support
  (:require t)
  (setopt vterm-max-scrollback 5000)) ; instead of 1000

(setup hel-vterm
  (:install hel-vterm :host github :repo "helheim-emacs/hel-vterm")
  (:after vterm)
  (:require t))

;;; .
(provide 'helheim-vterm)
;;; helheim-vterm.el ends here
