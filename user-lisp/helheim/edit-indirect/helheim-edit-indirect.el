;;; helheim-edit-indirect.el      -*- lexical-binding: t; no-byte-compile: t -*-

(hel-keymap-global-set :state 'normal
  "z n" 'helheim-edit-region-indirect) ; replace `hel-narrow-to-region-indirectly'

;;; .
(provide 'helheim-edit-indirect)
;;; helheim-edit-indirect.el ends here
