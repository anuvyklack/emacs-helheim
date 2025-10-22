;;; helheim-man.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'helix-core)

(helix-set-initial-state 'Man-mode 'normal)

;; You can also enable `outline-minor-mode' in a Man buffer, so the keys
;; should possibly not interfere with it.
(with-eval-after-load 'man
  (helix-inhibit-insert-state Man-mode-map)
  (helix-keymap-set Man-mode-map :state 'normal
    "] ]"   'Man-next-manpage
    "[ ["   'Man-previous-manpage
    "z h"   'Man-next-manpage     ; left
    "z l"   'Man-previous-manpage ; right
    "z j"   'Man-next-section     ; up
    "z k"   'Man-previous-section ; down
    "z /"   'Man-goto-section     ; Relative to sections thats why on "z" layer.
    "g r"   'Man-follow-manual-reference ; go to reference
    "C-w r" 'Man-update-manpage)) ; Standard chord for revert.

(provide 'helheim-man)
;;; helheim-man.el ends here
