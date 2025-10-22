;;; helheim-info.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'helix-macros)
(require 'helix-core)

(helix-set-initial-state 'Info-mode 'normal)

(with-eval-after-load 'info
  (helix-inhibit-insert-state Info-mode-map)
  (helix-keymap-set Info-mode-map :state 'normal
    "C-j"   'Info-next
    "C-k"   'Info-prev
    "z j"   'Info-forward-node
    "z k"   'Info-backward-node
    "z u"   'Info-up
    "z d"   'Info-directory

    "z h"   'Info-history
    "u"     'Info-history-back
    "U"     'Info-history-forward
    "C-<i>" 'Info-history-forward
    "C-o"   'Info-history-back

    "g t"   'Info-toc
    "g i"   'Info-index ; imenu
    "g I"   'Info-virtual-index

    "z i"   'Info-index
    "z I"   'Info-virtual-index
    "z a"   'info-apropos

    "M-h"   'Info-help))

(helix-advice-add 'Info-next-reference :before #'helix-deactivate-mark-a)
(helix-advice-add 'Info-prev-reference :before #'helix-deactivate-mark-a)

(provide 'helheim-info)
;;; helheim-info.el ends here
