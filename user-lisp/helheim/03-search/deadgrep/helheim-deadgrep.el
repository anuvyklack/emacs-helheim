;;; helheim-deadgrep.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; Config

(setup deadgrep
  (:install t)
  ;; <leader> ss — deadgrep entry point
  (:keymap search-map
    (:bind "s" 'deadgrep))
  ;; (:hook deadgrep-mode-hook next-error-follow-minor-mode)
  )

;;; .
(provide 'helheim-deadgrep)
;;; helheim-deadgrep.el ends here
