;;; helheim-modus-themes.el       -*- lexical-binding: t; no-byte-compile: t -*-
;;; Code:

(setup modus-themes
  (require-theme 'modus-themes)
  ;; Obsolet since 5.0.0
  (setq modus-themes-custom-auto-reload nil)
  (:setopt
    ;; modus-themes-to-toggle '(modus-operandi modus-vivendi)
    ;; modus-themes-to-rotate modus-themes-items
    modus-themes-mixed-fonts t
    modus-themes-variable-pitch-ui t
    modus-themes-italic-constructs t
    modus-themes-bold-constructs t
    modus-themes-headings '((agenda-structure . (variable-pitch light 2.2))
                            (agenda-date . (variable-pitch regular 1.3))
                            (t . (regular 1.03)))))

;;; modus-operandi
;;;; General

(helheim-theme-set-faces 'modus-operandi
  '(modus-themes-completion-selected :background "#c0deff"))

(helheim-theme-set-faces 'modus-operandi
  '(region :background "#d9eaff") ;; #c0deff
  '(help-key-binding :foreground "#0000b0" :background "grey96"
                     :box (:line-width (-1 . -1) :color "grey80")
                     :inherit fixed-pitch))

(setup modus-themes
  (:setopt modus-operandi-palette-overrides
           '(;; Headings
             (fg-heading-1 "#000000")
             (fg-heading-2 "#624416")
             (fg-heading-3 "#193668")
             (fg-heading-4 "#721045")
             (fg-heading-5 "#2a5045")
             (fg-heading-6 "#7f0000")
             (fg-heading-7 "#3f578f")
             (fg-heading-8 "#595959")
             ;; Search
             (bg-search-current bg-yellow-subtle)
             (bg-search-lazy    bg-cyan-subtle)
             (bg-search-static  bg-magenta-subtle)
             (bg-search-replace bg-red-subtle)
             ;;
             ;; (bg-search-rx-group-0 bg-blue-subtle)
             ;; (bg-search-rx-group-1 bg-green-subtle)
             ;; (bg-search-rx-group-2 bg-red-subtle)
             ;; (bg-search-rx-group-3 bg-magenta-subtle)
             )))

;;;; avy

(helheim-theme-set-faces 'modus-operandi
  '(avy-lead-face   :background "#7feaff" :foreground "black" :weight bold)
  '(avy-lead-face-0 :background "#ffaaff" :foreground "black" :weight bold))

;;;; diff-hl

(helheim-theme-set-faces 'modus-operandi
  '(diff-hl-change :background "#efd299")
  '(diff-hl-delete :background "#f3b5af")
  '(diff-hl-insert :background "#b2e8be"))

;;;; Search: isearch, occur, etc

(helheim-theme-set-faces 'modus-operandi
  '(lazy-highlight :background "#f4ede2"
                   ;; :background "#ffddff"
                   :distantforeground "black"))

;;;; magit

(helheim-theme-set-faces 'modus-operandi
  '(magit-section-heading :foreground "#193668"
                          :height 1.1
                          :weight bold
                          :extend t)
  '(magit-diff-file-heading :foreground "#0031a9"
                            :weight bold
                            :extend t)
  '(magit-diff-file-heading-highlight :foreground "#0031a9"
                                      :background "#e0e0e0"
                                      :extend t))

;; Faces:
;; git-commit-summary

;;;; org-mode

(helheim-theme-set-faces 'modus-operandi
  '(org-verbatim :foreground "#8f0075" :background "#f5f5f5")
  '(org-code :foreground "#005f5f" :background "#f5f5f5")
  '(org-tag :foreground "#7c318f" :height 0.95 :weight light :inherit fixed-pitch))

;; Code block
(helheim-theme-set-faces 'modus-operandi
  '(org-block :background "#f9f9f9" :extend t :inherit fixed-pitch)
  '(org-block-begin-line :background "#efefef"
                         :foreground "#777777"
                         :extend t
                         :inherit fixed-pitch)
  '(org-block-end-line :inherit org-block-begin-line))

;; Org-node backlinks buffer
(helheim-theme-set-faces 'modus-operandi
  '(org-node-context-origin-title :foreground "#731c52" :height 1.05 :extend t))

;;; modus-vivendi

(helheim-theme-set-faces 'modus-vivendi
  '(cursor :background "white")
  '(hel-normal-state-fake-cursor :background "#6fe5ff")
  '(hel-insert-state-fake-cursor :background "#6fe5ff")
  '(default :foreground "#f1f1f1" :background "#000000")
  ;; '(region :foreground "#e5e5e5" :background "#294e63")
  '(region :background "#294e63"))

;;; .
(provide 'helheim-modus-themes)
;;; helheim-modus-themes.el ends here
