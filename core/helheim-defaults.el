;;; helheim-defaults.el --- Emacs Helheim -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Authors: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; URL: https://github.com/anuvyklack/helix.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file content is primarily taken from the following packages:
;; - `minimal-emacs.d' [[https://github.com/jamescherti/minimal-emacs.d]]
;; - `doom' [[https://github.com/doomemacs/doomemacs]]
;; All credit is given to the original projects.
;;
;;; Code:
;;; User interface

;; Which Function mode
(setq which-func-update-delay 1.0)

;;; Misc

(setq whitespace-line-column nil)  ; Use the value of `fill-column'.

(provide 'helheim-defaults)
;;; helheim-defaults.el ends here
