;;; helheim-edit-indirect.el -*- lexical-binding: t; no-byte-compile: t -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module rebinds `zn' key chord to `helheim-edit-indirect' command.
;;
;; It differs from `hel-narrow-to-region-indirectly' (original `zn' binding)
;; which clones buffer inidrectly with narrowing in that the text properties
;; are not shared, so the parent buffer major mode and the edit-indirect buffer
;; major mode will not be able to tread on each other's toes by setting up
;; potentially conflicting text properties, which happens surprisingly often
;; when the font-lock mode is used.
;;
;; When done, exit with `edit-indirect-commit', which will remove the original
;; region and replace it with the edited version; or with `edit-indirect-abort',
;; which will drop the modifications.
;;
;; Edit-indirect buffers use the `edit-indirect-mode-map' keymap. Regions with
;; active edit-indirect buffers use the edit-indirect-overlay-map keymap.
;;
;; If there's already an edit-indirect buffer for region, use that. If there's
;; already an edit-indirect buffer active overlapping any portion of region, an
;; `edit-indirect-overlapping' error is signaled.
;;
;;; Code:

(hel-keymap-global-set :state 'normal
  "z n" 'helheim-edit-region-indirect) ; replace `hel-narrow-to-region-indirectly'

;;; .
(provide 'helheim-edit-indirect)
;;; helheim-edit-indirect.el ends here
