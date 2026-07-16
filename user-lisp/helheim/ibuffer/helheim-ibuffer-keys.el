;;; helheim-ibuffer-keys.el       -*- lexical-binding: t; no-byte-compile: t -*-

(require 'ibuffer)

;; TODO: Duplicate all keys, so user doesn’t need to look up the original
;;   bindings and mentally merge them with these.
(setup ibuffer
  (:keymap ibuffer-mode-map
    (:unbind "h" "l")
    (:bind
      "C-c f f" 'ibuffer-find-file

      "`"   'ibuffer-switch-format ;; Type to see the full buffer names.

      "j"   'ibuffer-forward-line ;; was `ibuffer-jump-to-buffer' moved to "/"
      "k"   'ibuffer-backward-line ;; was `ibuffer-do-kill-lines' moved to "K"

      ">"   'ibuffer-forward-next-marked
      "<"   'ibuffer-backwards-next-marked

      "/"   'ibuffer-jump-to-buffer ;; was `ibuffer--filter-map' moved to "f"
      "f"   `("filter" . ,ibuffer--filter-map)
      "K"   'ibuffer-do-kill-lines

      "d"   'ibuffer-mark-for-delete
      "M-d" 'ibuffer-mark-for-delete-backwards
      "x"   'ibuffer-do-kill-on-deletion-marks

      "v"     'ibuffer-do-view-horizontally
      "C-x v" 'ibuffer-do-view

      "g" (define-keymap
            "r" 'ibuffer-update
            "R" 'ibuffer-redisplay ;; "l"
            "d" 'ibuffer-do-delete
            "s" 'ibuffer-do-save
            "v" 'ibuffer-do-view-horizontally
            "z" 'ibuffer-bury-buffer
            "/" 'ibuffer-do-replace-regexp)

      ;; Filter groups
      "TAB" 'ibuffer-toggle-filter-group
      "z f" 'ibuffer-jump-to-filter-group

      "C-j" 'ibuffer-forward-filter-group
      "C-k" 'ibuffer-backward-filter-group
      "z j" 'ibuffer-forward-filter-group
      "z k" 'ibuffer-backward-filter-group
      "z u" 'ibuffer-backward-filter-group

      "}"   'ibuffer-forward-filter-group
      "{"   'ibuffer-backward-filter-group
      "] ]" 'ibuffer-forward-filter-group
      "[ [" 'ibuffer-backward-filter-group)))

(provide 'helheim-ibuffer '(keys))
;;; helheim-ibuffer-keys.el ends here
