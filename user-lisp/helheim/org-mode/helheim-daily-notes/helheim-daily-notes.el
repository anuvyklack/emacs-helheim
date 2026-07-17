;;; helheim-daily-notes.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; Code:

(require 'dash)

;;; Customization

(defcustom helheimg-daily-directory (expand-file-name "daily/" org-directory)
  "Path to daily-notes."
  :group 'helheim
  :type 'string
  :set (lambda (symbol value)
         (set-default symbol (-> (expand-file-name value)
                                 (file-name-as-directory)))))

;;; Keybindings

;; <leader>
(keymap-global-set "C-c n d" '("daily note" . helheimg-daily-note))

(add-hook 'org-mode-hook 'helheim-daily--set-keys)

;;; .
(provide 'helheim-daily-notes)
;;; helheim-daily-notes.el ends here
