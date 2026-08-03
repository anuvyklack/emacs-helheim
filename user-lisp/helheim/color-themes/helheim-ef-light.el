;;; helheim-ef-light.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs color scheme that is based on `el-light' theme faces before the
;; ef-themes 2.0 update with some customizations.
;;
;; The symbol you need to pass to `load-theme' is still `ef-light'!
;;
;;; Code:

(setup ef-themes
  (:install ef-themes :wait t)
  (:setopt modus-themes-mixed-fonts t
           modus-themes-variable-pitch-ui t
           modus-themes-italic-constructs t
           modus-themes-bold-constructs t
           modus-themes-headings '((agenda-structure . (variable-pitch light 2.2))
                                   (agenda-date . (variable-pitch regular 1.3))
                                   (t . (regular 1.03))))
  (modus-themes-include-derivatives-mode))

;;; internal faces

(helheim-theme-set-faces 'ef-light
  '(modus-themes-button :inherit variable-pitch :box (:line-width 1 :color "#bfc4da" :style released-button) :background "#b3b3b3" :foreground "#000000")
  '(modus-themes-heading-0 :inherit bold :foreground "#008858")
  '(modus-themes-heading-1 :inherit bold :foreground "#4f54aa")
  '(modus-themes-heading-2 :inherit bold :foreground "#ba35af")
  '(modus-themes-heading-3 :inherit bold :foreground "#1f77bb")
  '(modus-themes-heading-4 :inherit bold :foreground "#b65050")
  '(modus-themes-heading-5 :inherit bold :foreground "#6052cf")
  '(modus-themes-heading-6 :inherit bold :foreground "#d51272")
  '(modus-themes-heading-7 :inherit bold :foreground "#008858")
  '(modus-themes-heading-8 :inherit bold :foreground "#a45f22")
  ;; '(ef-themes-underline-info :underline (:style wave :color "#02af52"))
  )

;;; all basic faces

(helheim-theme-set-faces 'ef-light
  '(bold :weight bold)
  '(bold-italic :inherit (bold italic))
  '(cursor :background "#0033cc")
  '(default :background "#ffffff" :foreground "#202020")
  '(italic :slant italic)
  '(menu :background "#efefef" :foreground "#202020")
  '(region :background "#d6f4ff") ;; original #bfefff
  '(scroll-bar :background "#efefef" :foreground "#68759f")
  '(tool-bar :background "#efefef" :foreground "#202020")
  '(vertical-border :foreground "#bfc4da")
  '(appt-notification :inherit bold :foreground "#9f0000")
  '(blink-matching-paren-offscreen :background "#dfa0f3")
  '(button :foreground "#4250ef" :underline "#bfc4da")
  '(child-frame-border :background "#bfc4da")
  '(comint-highlight-input :inherit bold)
  '(comint-highlight-prompt :inherit minibuffer-prompt)
  '(edmacro-label :inherit bold :foreground "#4250ef")
  '(elisp-shorthand-font-lock-face :inherit (italic font-lock-preprocessor-face))
  '(error :inherit bold :foreground "#e00033")
  '(escape-glyph :foreground "#b6532f")
  '(fringe :background "#f3f3f3")
  '(header-line :inherit variable-pitch :background "#efefef")
  '(header-line-highlight :inherit highlight)
  '(help-argument-name :foreground "#4250ef")
  '(help-key-binding :foreground "DarkBlue" :background "grey96"
                     :box (:line-width (-1 . -1) :color "grey80")
                     :inherit fixed-pitch)
  '(highlight :background "#aaeccf" :foreground "#000000")
  '(hl-line :background "#e4efd8" :extend t)
  '(icon-button :inherit modus-themes-button)
  '(link :foreground "#4250ef" :underline "#bfc4da")
  '(link-visited :foreground "#ba35af" :underline "#bfc4da")
  '(minibuffer-prompt :foreground "#008858")
  '(mm-command-output :foreground "#3f6faf")
  '(mm-uu-extract :foreground "#3f6faf")
  '(pgtk-im-0 :inherit secondary-selection)
  '(read-multiple-choice-face :inherit warning :background "#ffeabb")
  '(rectangle-preview :inherit secondary-selection)
  '(secondary-selection :background "#ccbfff" :foreground "#000000")
  '(shadow :foreground "#68759f")
  '(success :inherit bold :foreground "#217a3c")
  '(tooltip :background "#dbdbdb" :foreground "#000000")
  '(trailing-whitespace :background "#ff8f88" :foreground "#000000")
  '(warning :inherit bold :foreground "#b6532f"))

;;; all-the-icons

(helheim-theme-set-faces 'ef-light
  '(all-the-icons-blue       :foreground "#065fff")
  '(all-the-icons-blue-alt   :foreground "#4250ef")
  '(all-the-icons-cyan       :foreground "#1f6fbf")
  '(all-the-icons-cyan-alt   :foreground "#3f6faf")
  '(all-the-icons-dblue      :foreground "#4f54aa")
  '(all-the-icons-dcyan      :foreground "#506fa0")
  '(all-the-icons-dgreen     :foreground "#61756c")
  '(all-the-icons-dmaroon    :foreground "#af5a80")
  '(all-the-icons-dorange    :foreground "#c24552")
  '(all-the-icons-dpink      :foreground "#af5a80")
  '(all-the-icons-dpurple    :foreground "#6052cf")
  '(all-the-icons-dred       :foreground "#d3303a")
  '(all-the-icons-dsilver    :foreground "#506fa0")
  '(all-the-icons-dyellow    :foreground "#a65f6a")
  '(all-the-icons-green      :foreground "#217a3c")
  '(all-the-icons-lblue      :foreground "#065fff")
  '(all-the-icons-lcyan      :foreground "#1f6fbf")
  '(all-the-icons-lgreen     :foreground "#4a7d00")
  '(all-the-icons-lmaroon    :foreground "#cf25aa")
  '(all-the-icons-lorange    :foreground "#e00033")
  '(all-the-icons-lpink      :foreground "#ba35af")
  '(all-the-icons-lpurple    :foreground "#af5a80")
  '(all-the-icons-lred       :foreground "#c24552")
  '(all-the-icons-lsilver    :foreground "gray50")
  '(all-the-icons-lyellow    :foreground "#b6532f")
  '(all-the-icons-maroon     :foreground "#ba35af")
  '(all-the-icons-orange     :foreground "#b6532f")
  '(all-the-icons-pink       :foreground "#cf25aa")
  '(all-the-icons-purple     :foreground "#6052cf")
  '(all-the-icons-purple-alt :foreground "#4250ef")
  '(all-the-icons-red        :foreground "#d3303a")
  '(all-the-icons-red-alt    :foreground "#d51272")
  '(all-the-icons-silver     :foreground "gray50")
  '(all-the-icons-yellow     :foreground "#a45f22"))

;;; all-the-icons-dired

(helheim-theme-set-faces 'ef-light
  '(all-the-icons-dired-dir-face :foreground "#4250ef"))

;;; all-the-icons-ibuffer

(helheim-theme-set-faces 'ef-light
  '(all-the-icons-ibuffer-dir-face :foreground "#4250ef")
  '(all-the-icons-ibuffer-file-face :foreground "#506fa0")
  '(all-the-icons-ibuffer-mode-face :foreground "#008858")
  '(all-the-icons-ibuffer-size-face :foreground "#1f77bb"))

;;; ansi-color

(helheim-theme-set-faces 'ef-light
  '(ansi-color-bold :inherit bold)
  '(ansi-color-black          :background "black"   :foreground "black")
  '(ansi-color-blue           :background "#4250ef" :foreground "#4250ef")
  '(ansi-color-bright-black   :background "gray35"  :foreground "gray35")
  '(ansi-color-bright-blue    :background "#065fff" :foreground "#065fff")
  '(ansi-color-bright-cyan    :background "#1f77bb" :foreground "#1f77bb")
  '(ansi-color-bright-green   :background "#4a7d00" :foreground "#4a7d00")
  '(ansi-color-bright-magenta :background "#6052cf" :foreground "#6052cf")
  '(ansi-color-bright-red     :background "#e00033" :foreground "#e00033")
  '(ansi-color-bright-white   :background "white"   :foreground "white")
  '(ansi-color-bright-yellow  :background "#b65050" :foreground "#b65050")
  '(ansi-color-cyan           :background "#1f6fbf" :foreground "#1f6fbf")
  '(ansi-color-green          :background "#217a3c" :foreground "#217a3c")
  '(ansi-color-magenta        :background "#ba35af" :foreground "#ba35af")
  '(ansi-color-red            :background "#d51272" :foreground "#d51272")
  '(ansi-color-white          :background "gray65"  :foreground "gray65")
  '(ansi-color-yellow         :background "#a45f22" :foreground "#a45f22"))

;;; auctex and tex

(helheim-theme-set-faces 'ef-light
  '(font-latex-bold-face                 :inherit bold)
  '(font-latex-doctex-documentation-face :inherit font-lock-doc-face)
  '(font-latex-doctex-preprocessor-face  :inherit font-lock-preprocessor-face)
  '(font-latex-italic-face               :inherit italic)
  '(font-latex-math-face                 :inherit font-lock-constant-face)
  '(font-latex-script-char-face          :inherit font-lock-builtin-face)
  '(font-latex-sectioning-5-face         :inherit (bold variable-pitch) :foreground "#397a70")
  '(font-latex-sedate-face               :inherit font-lock-keyword-face)
  '(font-latex-slide-title-face          :inherit modus-themes-heading-0)
  '(font-latex-string-face               :inherit font-lock-string-face)
  '(font-latex-underline-face            :inherit underline)
  '(font-latex-verbatim-face             :inherit fixed-pitch :foreground "#4250ef")
  '(font-latex-warning-face              :inherit font-lock-warning-face)
  '(tex-verbatim                         :inherit fixed-pitch :foreground "#4250ef")
  '(TeX-error-description-error          :inherit error)
  '(TeX-error-description-help           :inherit success)
  '(TeX-error-description-tex-said       :inherit success)
  '(TeX-error-description-warning        :inherit warning))

;;; auto-dim-other-buffers

(helheim-theme-set-faces 'ef-light
  '(auto-dim-other-buffers-face :background "#f9f9f9")
  '(auto-dim-other-buffers-hide-face :foreground "#f9f9f9" :background "#f9f9f9"))

;;; avy

(helheim-theme-set-faces 'ef-light
  ;; '(avy-background-face :background "#efefef" :foreground "#68759f" :extend t)
  '(avy-background-face :foreground "#68759f" :inherit default :extend t)
  '(avy-goto-char-timer-face :inherit bold :background "#b3b3b3")
  '(avy-lead-face   :background "#7feaff" :inherit (bold modus-themes-reset-soft))
  '(avy-lead-face-0 :background "#ffaaff" :inherit (bold modus-themes-reset-soft))
  '(avy-lead-face-1 :background "#f9f9f9" :inherit modus-themes-reset-soft)
  '(avy-lead-face-2 :background "#dff000" :inherit (bold modus-themes-reset-soft)))

;;; aw (ace-window)

(helheim-theme-set-faces 'ef-light
  '(aw-background-face :foreground "#68759f")
  '(aw-key-face :inherit (bold fixed-pitch) :foreground "#065fff")
  '(aw-leading-char-face :inherit (bold modus-themes-reset-soft) :height 1.5 :foreground "#065fff")
  '(aw-minibuffer-leading-char-face :inherit (bold fixed-pitch) :foreground "#065fff")
  '(aw-mode-line-face :inherit bold))

;;; bongo

(helheim-theme-set-faces 'ef-light
  '(bongo-artist :foreground "#008858")
  '(bongo-currently-playing-track :inherit bold)
  '(bongo-elapsed-track-part :background "#dbdbdb" :underline t)
  '(bongo-filled-seek-bar :background "#aaeccf")
  '(bongo-marked-track :inherit warning :background "#ffeabb")
  '(bongo-marked-track-line :background "#efefef" :extend t)
  '(bongo-played-track :inherit shadow :strike-through t)
  '(bongo-track-length :inherit shadow)
  '(bongo-track-title :foreground "#4f54aa")
  '(bongo-unfilled-seek-bar :background "#efefef"))

;;; bookmark

(helheim-theme-set-faces 'ef-light
  '(bookmark-face :foreground "#217a3c")
  '(bookmark-menu-bookmark :inherit bold))

;;; calendar and diary

(helheim-theme-set-faces 'ef-light
  '(calendar-month-header :inherit bold)
  '(calendar-today :inherit bold :underline t)
  '(calendar-weekday-header :foreground "#1f6fbf")
  '(calendar-weekend-header :foreground "#c24552")
  '(diary :foreground "#1f77bb")
  '(diary-anniversary :foreground "#cf25aa")
  '(diary-time :foreground "#1f77bb")
  '(holiday :foreground "#cf25aa"))

;;; centaur-tabs

(helheim-theme-set-faces 'ef-light
  '(centaur-tabs-active-bar-face :background "#065fff")
  '(centaur-tabs-close-mouse-face :inherit bold :foreground "#e00033" :underline t)
  '(centaur-tabs-close-selected :inherit centaur-tabs-selected)
  '(centaur-tabs-close-unselected :inherit centaur-tabs-unselected)
  '(centaur-tabs-modified-marker-selected :inherit centaur-tabs-selected)
  '(centaur-tabs-modified-marker-unselected :inherit centaur-tabs-unselected)
  '(centaur-tabs-default :background "#ffffff")
  '(centaur-tabs-selected :inherit bold :box (:line-width -2 :color "#ffffff") :background "#ffffff")
  '(centaur-tabs-selected-modified :inherit (italic centaur-tabs-selected))
  '(centaur-tabs-unselected :box (:line-width -2 :color "#b3b3b3") :background "#b3b3b3")
  '(centaur-tabs-unselected-modified :inherit (italic centaur-tabs-unselected)))

;;; cider

(helheim-theme-set-faces 'ef-light
  '(cider-deprecated-face :background "#ffeabb" :foreground "#b6532f")
  '(cider-enlightened-face :box "#b6532f")
  '(cider-enlightened-local-face :inherit warning)
  '(cider-error-highlight-face :underline (:style wave :color "#d63a44"))
  '(cider-fringe-good-face :inherit success :background "#d0efda")
  '(cider-instrumented-face :box "#e00033")
  '(cider-reader-conditional-face :inherit font-lock-type-face)
  '(cider-repl-prompt-face :inherit minibuffer-prompt)
  '(cider-repl-stderr-face :foreground "#e00033")
  '(cider-repl-stdout-face :foreground "#217a3c")
  ;; '(cider-warning-highlight-face)
  )

;;; change-log and log-view

(helheim-theme-set-faces 'ef-light
  '(change-log-acknowledgment :foreground "#af5a80")
  '(change-log-conditionals :inherit error)
  '(change-log-date :foreground "#1f77bb")
  '(change-log-email :foreground "#397a70")
  '(change-log-file :inherit bold)
  '(change-log-function :inherit warning)
  '(change-log-list :inherit bold)
  '(change-log-name :foreground "#6052cf")
  '(log-edit-header :inherit bold)
  '(log-edit-headers-separator :height 1 :background "#bfc4da" :extend t)
  '(log-edit-summary :inherit success)
  '(log-edit-unknown-header :inherit shadow)
  '(log-view-file :inherit bold)
  '(log-view-message :foreground "#af5a80"))

;;; clojure-mode

(helheim-theme-set-faces 'ef-light
  '(clojure-keyword-face :inherit font-lock-builtin-face))

;;; company-mode

(helheim-theme-set-faces 'ef-light
  '(company-echo-common :inherit bold :foreground "#4250ef")
  '(company-preview :background "#efefef" :foreground "#68759f")
  '(company-preview-common :inherit company-echo-common)
  '(company-preview-search :background "#fac200" :foreground "#000000")
  '(company-scrollbar-bg :background "#b3b3b3")
  '(company-scrollbar-fg :background "#202020")
  '(company-template-field :background "#b3b3b3" :foreground "#000000")
  '(company-tooltip :inherit fixed-pitch :background "#f9f9f9")
  '(company-tooltip-annotation :inherit completions-annotations)
  '(company-tooltip-common :inherit company-echo-common)
  '(company-tooltip-deprecated :inherit company-tooltip :strike-through t)
  '(company-tooltip-mouse :inherit highlight)
  '(company-tooltip-scrollbar-thumb :background "#397a70")
  '(company-tooltip-scrollbar-track :background "#dbdbdb")
  '(company-tooltip-search :inherit secondary-selection)
  '(company-tooltip-search-selection :inherit secondary-selection :underline t)
  '(company-tooltip-selection :background "#bfe8ff"))

;;; compilation

(helheim-theme-set-faces 'ef-light
  '(compilation-column-number :inherit compilation-line-number)
  '(compilation-error :inherit error)
  '(compilation-info :inherit bold :foreground "#6052cf")
  '(compilation-line-number :inherit shadow)
  '(compilation-mode-line-exit :inherit bold :foreground "#002fa0")
  '(compilation-mode-line-fail :inherit bold :foreground "#9f0000")
  '(compilation-mode-line-run :inherit bold :foreground "#5f0070")
  '(compilation-warning :inherit warning))

;;; completions

(helheim-theme-set-faces 'ef-light
  '(completions-annotations      :foreground "#506fa0" :inherit italic)
  '(completions-common-part      :foreground "#4250ef" :inherit bold)
  '(completions-first-difference :foreground "#cf25aa" :inherit bold)
  '(completions-group-title      :foreground "#6052cf" :inherit bold)
  '(completions-highlight        :background "#bfe8ff"))

;;; consult

(helheim-theme-set-faces 'ef-light
  '(consult-async-split :inherit warning)
  '(consult-file :foreground "#6052cf")
  '(consult-key :inherit (bold fixed-pitch) :foreground "#065fff")
  '(consult-imenu-prefix :inherit shadow)
  '(consult-line-number :inherit shadow)
  '(consult-line-number-prefix :inherit shadow)
  '(consult-separator :foreground "#bfc4da"))

;;; corfu

(helheim-theme-set-faces 'ef-light
  '(corfu-current :background "#bfe8ff")
  '(corfu-bar     :background "#202020")
  '(corfu-border  :background "#b3b3b3")
  '(corfu-default :inherit fixed-pitch :background "#f9f9f9"))

;;; csv-mode

(helheim-theme-set-faces 'ef-light
  '(csv-separator-face :foreground "#e00033"))

;;; custom (M-x customize)

(helheim-theme-set-faces 'ef-light
  '(custom-button :box (:color "#bfc4da" :style released-button)
                  :background "#b3b3b3" :foreground "#000000")
  '(custom-button-mouse :inherit (highlight custom-button))
  '(custom-button-pressed :inherit (secondary-selection custom-button))
  '(custom-changed :background "#f4e8bd")
  '(custom-comment :inherit shadow)
  '(custom-comment-tag :inherit (bold shadow))
  '(custom-invalid :inherit error :strike-through t)
  '(custom-modified :inherit custom-changed)
  '(custom-rogue :inherit custom-invalid)
  '(custom-set :inherit success)
  '(custom-state :foreground "#397a70")
  '(custom-themed :inherit custom-changed)
  '(custom-variable-obsolete :inherit shadow)
  '(custom-face-tag :inherit bold :foreground "#008858")
  '(custom-group-tag :inherit bold :foreground "#ba35af")
  '(custom-group-tag-1 :inherit bold :foreground "#065fff")
  '(custom-variable-tag :inherit bold :foreground "#1f77bb"))

;;; dashboard

(helheim-theme-set-faces 'ef-light
  '(dashboard-heading :foreground "#6052cf"))

;;; denote

(helheim-theme-set-faces 'ef-light
  '(denote-faces-date :foreground "#1f77bb")
  '(denote-faces-delimiter :inherit shadow)
  '(denote-faces-extension :inherit shadow)
  '(denote-faces-keywords :inherit bold :foreground "#6052cf")
  '(denote-faces-link :inherit link)
  '(denote-faces-prompt-current-name :inherit italic :foreground "#553d00")
  '(denote-faces-prompt-new-name :inherit italic :foreground "#005000")
  '(denote-faces-prompt-old-name :inherit italic :foreground "#8f1313")
  '(denote-faces-signature :inherit bold :foreground "#4250ef")
  '(denote-faces-subdirectory :inherit bold :foreground "#68759f")
  '(denote-faces-time :inherit denote-faces-date)
  '(denote-faces-time-delimiter :inherit shadow))

;;; dictionary

(helheim-theme-set-faces 'ef-light
  '(dictionary-button-face :inherit bold)
  '(dictionary-reference-face :inherit link)
  '(dictionary-word-entry-face :inherit font-lock-comment-face))

;;; diff-hl

(helheim-theme-set-faces 'ef-light
  '(diff-hl-change :background "#efd299")
  '(diff-hl-delete :background "#f3b5af")
  '(diff-hl-insert :background "#b2e8be")
  '(diff-hl-reverted-hunk-highlight :background "#202020" :foreground "#ffffff"))

;;; diff

(helheim-theme-set-faces 'ef-light
  '(diff-added :background "#d0f0d0" :foreground "#005000")
  '(diff-changed :background "#f4e8bd" :foreground "#553d00" :extend t)
  '(diff-changed-unspecified :inherit diff-changed)
  '(diff-removed :background "#ffd8d5" :foreground "#8f1313")
  '(diff-refine-added :background "#b2e8be" :foreground "#005000")
  '(diff-refine-changed :background "#efd299" :foreground "#553d00")
  '(diff-refine-removed :background "#f3b5af" :foreground "#8f1313")
  '(diff-indicator-added :inherit diff-added :foreground "#005000")
  '(diff-indicator-changed :inherit diff-changed :foreground "#553d00")
  '(diff-indicator-removed :inherit diff-removed :foreground "#8f1313")
  '(diff-error :inherit error)
  '(diff-file-header :inherit bold)
  '(diff-function :background "#dbdbdb")
  '(diff-hunk-header :inherit bold :background "#dbdbdb")
  '(diff-index :inherit italic)
  '(diff-nonexistent :inherit bold))

;;; dired

(helheim-theme-set-faces 'ef-light
  '(dired-broken-symlink :inherit (error link))
  '(dired-directory :foreground "#4250ef")
  '(dired-flagged :inherit bold :background "#ffd5ea" :foreground "#e00033" :extend t)
  '(dired-header :inherit bold)
  '(dired-ignored :inherit shadow)
  '(dired-mark :foreground "#e00033")
  '(dired-marked :inherit bold :background "#d0efda" :foreground "#217a3c" :extend t)
  '(dired-symlink :inherit link)
  '(dired-warning :inherit warning))

;;; diredfl

(helheim-theme-set-faces 'ef-light
  '(diredfl-autofile-name :background "#dbdbdb")
  '(diredfl-compressed-file-name :foreground "#b65050")
  '(diredfl-compressed-file-suffix :foreground "#d3303a")
  '(diredfl-date-time :foreground "#1f77bb")
  '(diredfl-deletion :inherit dired-flagged)
  '(diredfl-deletion-file-name :inherit diredfl-deletion)
  '(diredfl-dir-heading :inherit bold)
  '(diredfl-dir-name :inherit dired-directory)
  '(diredfl-dir-priv :inherit dired-directory)
  '(diredfl-exec-priv :foreground "#1f77bb")
  '(diredfl-executable-tag :inherit diredfl-exec-priv)
  '(diredfl-file-name :foreground "#202020")
  '(diredfl-file-suffix :foreground "#1f77bb")
  '(diredfl-flag-mark :inherit dired-marked)
  '(diredfl-flag-mark-line :inherit dired-marked)
  '(diredfl-ignored-file-name :inherit shadow)
  '(diredfl-link-priv :foreground "#4250ef")
  '(diredfl-no-priv :inherit shadow)
  '(diredfl-number :inherit shadow)
  '(diredfl-other-priv :foreground "#008858")
  '(diredfl-rare-priv :foreground "#008858")
  '(diredfl-read-priv :foreground "#4f54aa")
  '(diredfl-symlink :inherit dired-symlink)
  '(diredfl-tagged-autofile-name :inherit (diredfl-autofile-name dired-marked))
  '(diredfl-write-priv :foreground "#ba35af"))

;;; dirvish

(helheim-theme-set-faces 'ef-light
  '(dirvish-hl-line :background "#e4efd8" :extend t))

;;; display-fill-column-indicator-mode

(helheim-theme-set-faces 'ef-light
  '(fill-column-indicator :height 1 :background "#dbdbdb" :foreground "#dbdbdb"))

;;; doom-modeline

(helheim-theme-set-faces 'ef-light
  '(doom-modeline-bar :background "#065fff")
  '(doom-modeline-bar-inactive :background "#dbdbdb")
  '(doom-modeline-battery-charging :foreground "#002fa0")
  '(doom-modeline-battery-critical :foreground "#9f0000" :underline t)
  '(doom-modeline-battery-error :foreground "#9f0000" :underline t)
  '(doom-modeline-battery-warning :foreground "#5f0070" :inherit bold)
  '(doom-modeline-buffer-file :inherit bold)
  '(doom-modeline-buffer-modified :foreground "#9f0000")
  '(doom-modeline-evil-emacs-state :inherit italic)
  '(doom-modeline-evil-insert-state :foreground "#002fa0")
  '(doom-modeline-evil-operator-state :inherit bold)
  '(doom-modeline-evil-replace-state :foreground "#9f0000" :inherit bold)
  '(doom-modeline-evil-visual-state :foreground "#5f0070" :inherit bold)
  '(doom-modeline-info :foreground "#002fa0" :inherit bold)
  '(doom-modeline-lsp-error :inherit bold)
  '(doom-modeline-lsp-success :foreground "#002fa0" :inherit bold)
  '(doom-modeline-lsp-warning :foreground "#5f0070" :inherit bold)
  '(doom-modeline-notification :foreground "#9f0000" :inherit mode-line-emphasis)
  '(doom-modeline-repl-success :foreground "#002fa0" :inherit bold)
  '(doom-modeline-repl-warning :foreground "#5f0070" :inherit bold)
  '(doom-modeline-urgent :foreground "#9f0000" :inherit bold)
  '(doom-modeline-warning :foreground "#5f0070" :inherit bold))

;;; ediff

(helheim-theme-set-faces 'ef-light
  '(ediff-current-diff-A :background "#ffd8d5" :foreground "#8f1313")
  '(ediff-current-diff-Ancestor :background "#bfefff")
  '(ediff-current-diff-B :background "#d0f0d0" :foreground "#005000")
  '(ediff-current-diff-C :background "#f4e8bd" :foreground "#553d00")
  '(ediff-even-diff-A :background "#efefef")
  '(ediff-even-diff-Ancestor :background "#efefef")
  '(ediff-even-diff-B :background "#efefef")
  '(ediff-even-diff-C :background "#efefef")
  '(ediff-fine-diff-A :background "#f3b5af" :foreground "#8f1313")
  '(ediff-fine-diff-Ancestor :background "#b3b3b3" :foreground "#000000")
  '(ediff-fine-diff-B :background "#b2e8be" :foreground "#005000")
  '(ediff-fine-diff-C :background "#efd299" :foreground "#553d00")
  '(ediff-odd-diff-A :inherit ediff-even-diff-A)
  '(ediff-odd-diff-Ancestor :inherit ediff-even-diff-Ancestor)
  '(ediff-odd-diff-B :inherit ediff-even-diff-B)
  '(ediff-odd-diff-C :inherit ediff-even-diff-C))

;;; eglot

(helheim-theme-set-faces 'ef-light
  '(eglot-mode-line :inherit bold :foreground "#002fa0")
  ;; '(eglot-diagnostic-tag-unnecessary-face :inherit ef-themes-underline-info)
  )

;;; elfeed

(helheim-theme-set-faces 'ef-light
  '(elfeed-log-date-face :inherit elfeed-search-date-face)
  '(elfeed-log-debug-level-face :inherit elfeed-search-filter-face)
  '(elfeed-log-error-level-face :inherit error)
  '(elfeed-log-info-level-face :inherit success)
  '(elfeed-log-warn-level-face :inherit warning)
  '(elfeed-search-date-face :foreground "#1f77bb")
  '(elfeed-search-feed-face :foreground "#cf25aa")
  '(elfeed-search-filter-face :inherit bold)
  '(elfeed-search-last-update-face :inherit bold :foreground "#1f77bb")
  '(elfeed-search-tag-face :foreground "#4250ef")
  '(elfeed-search-title-face :foreground "#68759f")
  '(elfeed-search-unread-title-face :inherit bold :foreground "#202020"))

;;; embark

(helheim-theme-set-faces 'ef-light
  '(embark-collect-group-title :inherit bold :foreground "#6052cf")
  '(embark-keybinding :inherit (bold fixed-pitch) :foreground "#065fff")
  '(embark-keybinding-repeat :inherit bold)
  '(embark-selected :inherit success :background "#d0efda"))

;;; epa

(helheim-theme-set-faces 'ef-light
  '(epa-field-name :inherit bold :foreground "#68759f")
  '(epa-mark :inherit bold)
  '(epa-string :foreground "#4250ef")
  '(epa-validity-disabled :foreground "#e00033")
  '(epa-validity-high :inherit success)
  '(epa-validity-low :inherit shadow)
  '(epa-validity-medium :foreground "#217a3c"))

;;; erc

(helheim-theme-set-faces 'ef-light
  '(erc-action-face :foreground "#008858")
  '(erc-bold-face :inherit bold)
  '(erc-button :inherit button)
  '(erc-command-indicator-face :inherit bold :foreground "#b6532f")
  '(erc-current-nick-face :inherit match)
  '(erc-dangerous-host-face :inherit error)
  '(erc-direct-msg-face :inherit shadow)
  '(erc-error-face :inherit error)
  '(erc-fool-face :inherit shadow)
  '(erc-input-face :foreground "#cf25aa")
  '(erc-inverse-face :inherit erc-default-face :inverse-video t)
  '(erc-fill-wrap-merge-indicator-face :foreground "#68759f")
  '(erc-keep-place-indicator-arrow :foreground "#217a3c")
  '(erc-keyword-face :inherit bold :foreground "#6052cf")
  '(erc-my-nick-face :inherit bold :foreground "#6052cf")
  '(erc-my-nick-prefix-face :inherit erc-my-nick-face)
  '(erc-nick-default-face :inherit bold :foreground "#4250ef")
  '(erc-nick-msg-face :inherit warning)
  '(erc-nick-prefix-face :inherit erc-nick-default-face)
  '(erc-notice-face :inherit font-lock-comment-face)
  '(erc-pal-face :inherit bold :foreground "#cf25aa")
  '(erc-prompt-face :inherit minibuffer-prompt)
  '(erc-timestamp-face :foreground "#1f77bb")
  '(erc-underline-face :underline t))

;;; ert

(helheim-theme-set-faces 'ef-light
  '(ert-test-result-expected :background "#d0efda" :foreground "#217a3c")
  '(ert-test-result-unexpected :background "#ffd5ea" :foreground "#e00033"))

;;; eshell

(helheim-theme-set-faces 'ef-light
  '(eshell-ls-archive :foreground "#008858")
  '(eshell-ls-backup :inherit shadow)
  '(eshell-ls-clutter :inherit shadow)
  '(eshell-ls-directory :foreground "#4250ef")
  '(eshell-ls-executable :foreground "#cf25aa")
  '(eshell-ls-missing :inherit error)
  '(eshell-ls-product :inherit shadow)
  '(eshell-ls-readonly :foreground "#b6532f")
  '(eshell-ls-special :foreground "#b6532f")
  '(eshell-ls-symlink :inherit link)
  '(eshell-ls-unreadable :inherit shadow)
  '(eshell-prompt :inherit minibuffer-prompt))

;;; evil-mode

(helheim-theme-set-faces 'ef-light
  '(evil-ex-commands :inherit font-lock-keyword-face)
  '(evil-ex-info :inherit font-lock-type-face)
  '(evil-ex-substitute-replacement :inherit query-replace))

;;; eww

(helheim-theme-set-faces 'ef-light
  '(eww-invalid-certificate :foreground "#e00033")
  '(eww-valid-certificate :foreground "#217a3c")
  '(eww-form-checkbox :inherit eww-form-text)
  '(eww-form-file :inherit eww-form-submit)
  '(eww-form-select :inherit eww-form-submit)
  '(eww-form-submit :inherit modus-themes-button)
  '(eww-form-text :inherit widget-field)
  '(eww-form-textarea :inherit eww-form-text))

;;; flycheck

(helheim-theme-set-faces 'ef-light
  '(flycheck-error :underline (:style wave :color "#d63a44"))
  '(flycheck-fringe-error   :background "#ffd5ea" :inherit error)
  '(flycheck-fringe-info    :background "#d0efda" :inherit success)
  '(flycheck-fringe-warning :background "#ffeabb" :inherit warning)
  '(flycheck-verify-select-checker :box (:style released-button))
  ;; error list
  '(flycheck-error-list-error   :inherit error)
  '(flycheck-error-list-warning :inherit warning)
  '(flycheck-error-list-info    :inherit success)
  ;; flycheck-error-list-line-number
  ;; flycheck-error-list-column-number
  '(flycheck-error-list-filename :inherit mode-line-buffer-id :bold nil)
  '(flycheck-error-list-id :inherit font-lock-type-face)
  '(flycheck-error-list-id-with-explainer :box (:style released-button)
                                          :inherit flycheck-error-list-id)
  '(flycheck-error-list-checker-name :inherit font-lock-function-name-face)
  ;; flycheck-error-list-error-message
  '(flycheck-error-list-highlight :background "#fff2d9" :extend t))

;;; flymake

(helheim-theme-set-faces 'ef-light
  '(flymake-end-of-line-diagnostics-face :inherit italic :height 0.85 :box "#bfc4da")
  '(flymake-error :underline (:style wave :color "#d63a44"))
  '(flymake-error-echo :inherit error)
  '(flymake-error-echo-at-eol :inherit flymake-end-of-line-diagnostics-face :foreground "#e00033")
  ;; '(flymake-note :inherit ef-themes-underline-info)
  '(flymake-note-echo :inherit success)
  '(flymake-note-echo-at-eol :inherit flymake-end-of-line-diagnostics-face :foreground "#217a3c")
  ;; '(flymake-warning)
  '(flymake-warning-echo :inherit warning))

;;; flyspell

(helheim-theme-set-faces 'ef-light
  ;; '(flyspell-duplicate)
  '(flyspell-incorrect :underline (:style wave :color "#d63a44"))
  )

;;; font-lock

(helheim-theme-set-faces 'ef-light
  '(font-lock-builtin-face :inherit bold :foreground "#ba35af")
  '(font-lock-comment-delimiter-face :inherit font-lock-comment-face)
  '(font-lock-comment-face :inherit italic :foreground "#a65f6a")
  '(font-lock-constant-face :foreground "#065fff")
  '(font-lock-doc-face :inherit italic :foreground "#506fa0")
  '(font-lock-function-name-face :foreground "#cf25aa")
  '(font-lock-keyword-face :inherit bold :foreground "#6052cf")
  '(font-lock-negation-char-face :inherit bold)
  '(font-lock-preprocessor-face :foreground "#d3303a")
  '(font-lock-regexp-grouping-backslash :foreground "#008858")
  '(font-lock-regexp-grouping-construct :foreground "#ba35af")
  '(font-lock-string-face :foreground "#4250ef")
  '(font-lock-type-face :foreground "#008858")
  '(font-lock-variable-name-face :foreground "#1f77bb")
  '(font-lock-warning-face :foreground "#b6532f"))

;;; forge

(helheim-theme-set-faces 'ef-light
  '(forge-dimmed :inherit shadow)
  '(forge-issue-completed :inherit shadow)
  '(forge-issue-unplanned :inherit forge-dimmed :strike-through t)
  '(forge-post-author :inherit bold :foreground "#6052cf")
  '(forge-post-date :inherit bold :foreground "#1f77bb")
  '(forge-pullreq-merged :foreground "#397a70")
  '(forge-pullreq-open :foreground "#217a3c")
  '(forge-pullreq-rejected :foreground "#e00033" :strike-through t)
  '(forge-topic-pending :inherit italic)
  '(forge-topic-slug-completed :inherit forge-dimmed)
  '(forge-topic-slug-open :inherit forge-dimmed)
  '(forge-topic-slug-saved :inherit success)
  '(forge-topic-slug-unplanned :inherit forge-dimmed :strike-through t)
  '(forge-topic-unread :inherit bold))

;;; git-commit

(helheim-theme-set-faces 'ef-light
  '(git-commit-comment-action :inherit font-lock-comment-face)
  '(git-commit-comment-branch-local :inherit font-lock-comment-face :foreground "#4250ef")
  '(git-commit-comment-branch-remote :inherit font-lock-comment-face :foreground "#cf25aa")
  '(git-commit-comment-heading :inherit (bold font-lock-comment-face))
  '(git-commit-comment-file :inherit font-lock-comment-face :foreground "#6052cf")
  '(git-commit-keyword :foreground "#6052cf")
  '(git-commit-nonempty-second-line :foreground "#e00033")
  '(git-commit-overlong-summary :foreground "#b6532f")
  '(git-commit-summary :inherit success))

;;; git-gutter

(helheim-theme-set-faces 'ef-light
  '(git-gutter:added :background "#d0f0d0" :foreground "#005000")
  '(git-gutter:deleted :background "#ffd8d5" :foreground "#8f1313")
  '(git-gutter:modified :background "#f4e8bd" :foreground "#553d00")
  '(git-gutter:separator :inherit success)
  '(git-gutter:unchanged :inherit bold))

;;; git-gutter-fr

(helheim-theme-set-faces 'ef-light
  '(git-gutter-fr:added :background "#d0f0d0" :foreground "#005000")
  '(git-gutter-fr:deleted :background "#ffd8d5" :foreground "#8f1313")
  '(git-gutter-fr:modified :background "#f4e8bd" :foreground "#553d00"))

;;; git-rebase

(helheim-theme-set-faces 'ef-light
  '(git-rebase-comment-hash :inherit (bold font-lock-comment-face) :foreground "#af5a80")
  '(git-rebase-comment-heading :inherit (bold font-lock-comment-face))
  '(git-rebase-description :foreground "#202020")
  '(git-rebase-hash :foreground "#af5a80"))

;;; gnus

(helheim-theme-set-faces 'ef-light
  '(gnus-button :inherit button :underline nil)
  '(gnus-cite-1 :inherit message-cited-text-1)
  '(gnus-cite-2 :inherit message-cited-text-2)
  '(gnus-cite-3 :inherit message-cited-text-3)
  '(gnus-cite-4 :inherit message-cited-text-4)
  '(gnus-cite-5 :inherit message-cited-text-1)
  '(gnus-cite-6 :inherit message-cited-text-2)
  '(gnus-cite-7 :inherit message-cited-text-3)
  '(gnus-cite-8 :inherit message-cited-text-4)
  '(gnus-cite-9 :inherit message-cited-text-1)
  '(gnus-cite-10 :inherit message-cited-text-2)
  '(gnus-cite-11 :inherit message-cited-text-3)
  '(gnus-cite-attribution :inherit italic)
  '(gnus-emphasis-bold :inherit bold)
  '(gnus-emphasis-bold-italic :inherit bold-italic)
  '(gnus-emphasis-highlight-words :inherit warning)
  '(gnus-emphasis-italic :inherit italic)
  '(gnus-emphasis-underline-bold :inherit gnus-emphasis-bold :underline t)
  '(gnus-emphasis-underline-bold-italic :inherit gnus-emphasis-bold-italic :underline t)
  '(gnus-emphasis-underline-italic :inherit gnus-emphasis-italic :underline t)
  '(gnus-group-mail-1 :inherit bold :foreground "#4f54aa")
  '(gnus-group-mail-1-empty :foreground "#4f54aa")
  '(gnus-group-mail-2 :inherit bold :foreground "#ba35af")
  '(gnus-group-mail-2-empty :foreground "#ba35af")
  '(gnus-group-mail-3 :inherit bold :foreground "#1f77bb")
  '(gnus-group-mail-3-empty :foreground "#1f77bb")
  '(gnus-group-mail-low :inherit bold :foreground "#397a70")
  '(gnus-group-mail-low-empty :foreground "#397a70")
  '(gnus-group-news-1 :inherit bold :foreground "#4f54aa")
  '(gnus-group-news-1-empty :foreground "#4f54aa")
  '(gnus-group-news-2 :inherit bold :foreground "#ba35af")
  '(gnus-group-news-2-empty :foreground "#ba35af")
  '(gnus-group-news-3 :inherit bold :foreground "#1f77bb")
  '(gnus-group-news-3-empty :foreground "#1f77bb")
  '(gnus-group-news-4 :inherit bold :foreground "#b65050")
  '(gnus-group-news-4-empty :foreground "#b65050")
  '(gnus-group-news-5 :inherit bold :foreground "#6052cf")
  '(gnus-group-news-5-empty :foreground "#6052cf")
  '(gnus-group-news-6 :inherit bold :foreground "#d51272")
  '(gnus-group-news-6-empty :foreground "#d51272")
  '(gnus-group-news-low :inherit bold :foreground "#397a70")
  '(gnus-group-news-low-empty :foreground "#397a70")
  '(gnus-header-content :inherit message-header-other)
  '(gnus-header-from :inherit message-header-to :underline nil)
  '(gnus-header-name :inherit message-header-name)
  '(gnus-header-newsgroups :inherit message-header-newsgroups)
  '(gnus-header-subject :inherit message-header-subject)
  '(gnus-server-agent :inherit bold)
  '(gnus-server-closed :inherit italic)
  '(gnus-server-cloud :inherit bold :foreground "#397a70")
  '(gnus-server-cloud-host :inherit bold :foreground "#397a70" :underline t)
  '(gnus-server-denied :inherit error)
  '(gnus-server-offline :inherit shadow)
  '(gnus-server-opened :inherit success)
  '(gnus-summary-cancelled :background "#ffeabb" :foreground "#b6532f" :extend t)
  '(gnus-summary-high-ancient :inherit bold :foreground "#397a70")
  '(gnus-summary-high-read :inherit bold :foreground "#68759f")
  '(gnus-summary-high-ticked :inherit bold :foreground "#e00033")
  '(gnus-summary-high-undownloaded :inherit bold-italic :foreground "#b6532f")
  '(gnus-summary-high-unread :inherit bold)
  '(gnus-summary-low-ancient :inherit italic)
  '(gnus-summary-low-read :inherit (shadow italic))
  '(gnus-summary-low-ticked :inherit italic :foreground "#e00033")
  '(gnus-summary-low-undownloaded :inherit italic :foreground "#b6532f")
  '(gnus-summary-low-unread :inherit italic)
  '(gnus-summary-normal-read :inherit shadow)
  '(gnus-summary-normal-ticked :foreground "#e00033")
  '(gnus-summary-normal-undownloaded :foreground "#b6532f")
  '(gnus-summary-selected :inherit highlight))

;;; helpful-mode

(helheim-theme-set-faces 'ef-light
  '(helpful-heading :inherit modus-themes-heading-1))

;;; hexl-mode

(helheim-theme-set-faces 'ef-light
  '(hexl-address-region :foreground "#065fff")
  '(hexl-ascii-region :foreground "#1f77bb"))

;;; misc
(helheim-theme-set-faces 'ef-light
  '(gnus-summary-selected :inherit highlight))

;;; helpful-mode
(helheim-theme-set-faces 'ef-light
  '(helpful-heading :inherit bold :foreground "#4f54aa"))

;;; hexl-mode
(helheim-theme-set-faces 'ef-light
  '(hexl-address-region :foreground "#065fff")
  '(hexl-ascii-region :foreground "#1f77bb"))

;;; hi-lock (M-x highlight-regexp)
(helheim-theme-set-faces 'ef-light
  '(hi-aquamarine :background "white" :foreground "#227f9f" :inverse-video t)
  '(hi-black-b :inverse-video t)
  '(hi-black-hb :background "#ffffff" :foreground "#68759f" :inverse-video t)
  '(hi-blue :background "white" :foreground "#3366dd" :inverse-video t)
  '(hi-blue-b :inherit (bold hi-blue))
  '(hi-green :background "white" :foreground "#008a00" :inverse-video t)
  '(hi-green-b :inherit (bold hi-green))
  '(hi-pink :background "white" :foreground "#bd30aa" :inverse-video t)
  '(hi-red-b :background "white" :foreground "#dd0000" :inverse-video t)
  '(hi-salmon :background "white" :foreground "#af4f6f" :inverse-video t)
  '(hi-yellow :background "white" :foreground "#af6f00" :inverse-video t))

;;; highlight-indentation mode
(helheim-theme-set-faces 'ef-light
  '(highlight-indentation-face :background "#efefef"))

;;; ibuffer
(helheim-theme-set-faces 'ef-light
  '(ibuffer-locked-buffer :foreground "#b6532f"))

;;; image-dired
(helheim-theme-set-faces 'ef-light
  '(image-dired-thumb-flagged :background "#e00033" :box (:line-width -3))
  '(image-dired-thumb-header-file-name :inherit bold)
  '(image-dired-thumb-header-file-size :foreground "#217a3c")
  '(image-dired-thumb-mark :background "#217a3c" :box (:line-width -3)))

;;; imenu-list
(helheim-theme-set-faces 'ef-light
  '(imenu-list-entry-face-0 :foreground "#4f54aa")
  '(imenu-list-entry-face-1 :foreground "#ba35af")
  '(imenu-list-entry-face-2 :foreground "#1f77bb")
  '(imenu-list-entry-face-3 :foreground "#b65050")
  '(imenu-list-entry-subalist-face-0 :inherit bold :foreground "#4f54aa" :underline t)
  '(imenu-list-entry-subalist-face-1 :inherit bold :foreground "#ba35af" :underline t)
  '(imenu-list-entry-subalist-face-2 :inherit bold :foreground "#1f77bb" :underline t)
  '(imenu-list-entry-subalist-face-3 :inherit bold :foreground "#b65050" :underline t))

;;; info
(helheim-theme-set-faces 'ef-light
  '(Info-quoted :inherit fixed-pitch :foreground "#4250ef")
  '(info-header-node :inherit (shadow bold))
  '(info-index-match :inherit match)
  '(info-menu-header :inherit bold)
  '(info-menu-star :foreground "#d3303a")
  '(info-node :inherit bold)
  '(info-title-1 :inherit bold :foreground "#4f54aa")
  '(info-title-2 :inherit bold :foreground "#ba35af")
  '(info-title-3 :inherit bold :foreground "#1f77bb")
  '(info-title-4 :inherit bold :foreground "#b65050"))

;;; isearch, occur, and the like
(helheim-theme-set-faces 'ef-light
  '(isearch :background "#fac200" :foreground "#000000")
  '(isearch-fail :inherit error :background "#ffd5ea" :foreground "#e00033")
  '(isearch-group-1 :background "#df8fff" :foreground "#000000")
  '(isearch-group-2 :background "#9adf90" :foreground "#000000")
  '(lazy-highlight :background "#ffeabb") ;; original :background "#cbcfff" :foreground "#000000"
  '(match :background "#ffeabb")
  '(query-replace :background "#ff8f88" :foreground "#000000"))

;;; jit-spell
(helheim-theme-set-faces 'ef-light
  '(jit-spell-misspelling :underline (:style wave :color "#d63a44")))

;;; jinx
(helheim-theme-set-faces 'ef-light
  '(jinx-misspelled :underline (:style wave :color "#bf5f00")))

;;; keycast
(helheim-theme-set-faces 'ef-light
  '(keycast-command :inherit bold)
  '(keycast-key :inherit bold :background "#aaeccf" :foreground "#000000" :box (:line-width -1 :color "#68759f")))

;;; lin
(helheim-theme-set-faces 'ef-light
  '(lin-blue :background "#ccdfff" :extend t)
  '(lin-cyan :background "#bfefff" :extend t)
  '(lin-green :background "#b3fabf" :extend t)
  '(lin-magenta :background "#ffddff" :extend t)
  '(lin-red :background "#ffcfbf" :extend t)
  '(lin-yellow :background "#fff576" :extend t)
  '(lin-blue-override-fg :background "#ccdfff" :foreground "#000000" :extend t)
  '(lin-cyan-override-fg :background "#bfefff" :foreground "#000000" :extend t)
  '(lin-green-override-fg :background "#b3fabf" :foreground "#000000" :extend t)
  '(lin-magenta-override-fg :background "#ffddff" :foreground "#000000" :extend t)
  '(lin-red-override-fg :background "#ffcfbf" :foreground "#000000" :extend t)
  '(lin-yellow-override-fg :background "#fff576" :foreground "#000000" :extend t))

;;; line numbers (display-line-numbers-mode and global variant)
(helheim-theme-set-faces 'ef-light
  '(line-number :background "#f5f5f5" :inherit fixed-pitch)
  '(line-number-current-line :background "#dddddd"
                             :weight bold
                             :inherit line-number)
  '(line-number-major-tick :inherit (bold line-number) :foreground "#008858")
  '(line-number-minor-tick :inherit (bold line-number)))

;;; magit
(helheim-theme-set-faces 'ef-light
  '(magit-bisect-bad :inherit error)
  '(magit-bisect-good :inherit success)
  '(magit-bisect-skip :inherit warning)
  '(magit-blame-dimmed :inherit shadow)
  '(magit-blame-highlight :background "#b3b3b3" :foreground "#000000")
  '(magit-branch-local :foreground "#4250ef")
  '(magit-branch-remote :foreground "#cf25aa")
  '(magit-branch-upstream :inherit italic)
  '(magit-branch-warning :inherit warning)
  '(magit-cherry-equivalent :foreground "#ba35af")
  '(magit-cherry-unmatched :foreground "#1f6fbf")
  '(magit-diff-added :background "#e5ffe5" :foreground "#005000")
  '(magit-diff-added-highlight :background "#d0f0d0" :foreground "#005000")
  '(magit-diff-base :background "#f9efcb" :foreground "#553d00")
  '(magit-diff-base-highlight :background "#f4e8bd" :foreground "#553d00")
  '(magit-diff-context :inherit shadow)
  '(magit-diff-context-highlight :background "#efefef")
  '(magit-diff-file-heading :inherit bold :foreground "#4250ef")
  '(magit-diff-file-heading-highlight :inherit magit-diff-file-heading :background "#dbdbdb")
  '(magit-diff-file-heading-selection :inherit bold :background "#ccbfff" :foreground "#000000")
  '(magit-diff-hunk-heading :background "#dbdbdb")
  '(magit-diff-hunk-heading-highlight :inherit bold :background "#b3b3b3" :foreground "#000000")
  '(magit-diff-hunk-heading-selection :inherit bold :background "#ccbfff" :foreground "#000000")
  '(magit-diff-hunk-region :inherit bold)
  '(magit-diff-lines-boundary :background "#000000")
  '(magit-diff-lines-heading :background "#397a70" :foreground "#dbdbdb")
  '(magit-diff-removed :background "#ffe9e9" :foreground "#8f1313")
  '(magit-diff-removed-highlight :background "#ffd8d5" :foreground "#8f1313")
  '(magit-diffstat-added :inherit success)
  '(magit-diffstat-removed :inherit error)
  '(magit-dimmed :inherit shadow)
  '(magit-filename :foreground "#6052cf")
  '(magit-hash :foreground "#af5a80")
  '(magit-head :inherit magit-branch-local)
  '(magit-header-line :inherit bold)
  '(magit-header-line-key :inherit (bold fixed-pitch) :foreground "#065fff")
  '(magit-header-line-log-select :inherit bold)
  '(magit-keyword :foreground "#6052cf")
  '(magit-keyword-squash :inherit bold :foreground "#b6532f")
  '(magit-log-author :foreground "#6052cf")
  '(magit-log-date :foreground "#1f77bb")
  '(magit-log-graph :inherit shadow)
  '(magit-mode-line-process :inherit bold :foreground "#002fa0")
  '(magit-mode-line-process-error :inherit bold :foreground "#9f0000")
  '(magit-process-ng :inherit error)
  '(magit-process-ok :inherit success)
  '(magit-reflog-amend :inherit warning)
  '(magit-reflog-checkout :inherit bold :foreground "#3740cf")
  '(magit-reflog-cherry-pick :inherit success)
  '(magit-reflog-commit :inherit bold)
  '(magit-reflog-merge :inherit success)
  '(magit-reflog-other :inherit bold :foreground "#1f6fbf")
  '(magit-reflog-rebase :inherit bold :foreground "#ba35af")
  '(magit-reflog-remote :inherit (bold magit-branch-remote))
  '(magit-reflog-reset :inherit error)
  '(magit-refname :inherit shadow)
  '(magit-refname-pullreq :inherit shadow)
  '(magit-refname-stash :inherit shadow)
  '(magit-refname-wip :inherit shadow)
  '(magit-section :background "#efefef" :foreground "#202020")
  '(magit-section-heading :inherit bold)
  '(magit-section-heading-selection :inherit bold :background "#ccbfff" :foreground "#000000")
  '(magit-section-highlight :background "#efefef")
  '(magit-sequence-done :inherit success)
  '(magit-sequence-drop :inherit error)
  '(magit-sequence-exec :inherit bold :foreground "#ba35af")
  '(magit-sequence-head :inherit bold :foreground "#1f6fbf")
  '(magit-sequence-onto :inherit (bold shadow))
  '(magit-sequence-part :inherit warning)
  '(magit-sequence-pick :inherit bold)
  '(magit-sequence-stop :inherit error)
  '(magit-signature-bad :inherit error)
  '(magit-signature-error :inherit error)
  '(magit-signature-expired :inherit warning)
  '(magit-signature-expired-key :foreground "#b6532f")
  '(magit-signature-good :inherit success)
  '(magit-signature-revoked :inherit bold :foreground "#b6532f")
  '(magit-signature-untrusted :inherit (bold shadow))
  '(magit-tag :foreground "#b6532f"))

;;; man
(helheim-theme-set-faces 'ef-light
  '(Man-overstrike :inherit bold :foreground "#4250ef")
  '(Man-underline :foreground "#cf25aa" :underline t))

;;; marginalia
(helheim-theme-set-faces 'ef-light
  '(marginalia-archive :foreground "#4250ef")
  '(marginalia-char :foreground "#008858")
  '(marginalia-date :foreground "#1f77bb")
  '(marginalia-documentation :inherit italic :foreground "#506fa0")
  '(marginalia-file-owner :inherit shadow)
  '(marginalia-file-priv-exec :foreground "#1f77bb")
  '(marginalia-file-priv-link :foreground "#4250ef")
  '(marginalia-file-priv-no :inherit shadow)
  '(marginalia-file-priv-other :foreground "#008858")
  '(marginalia-file-priv-rare :foreground "#008858")
  '(marginalia-file-priv-read :foreground "#4f54aa")
  '(marginalia-file-priv-write :foreground "#ba35af")
  '(marginalia-function :foreground "#cf25aa")
  '(marginalia-key :inherit (bold fixed-pitch) :foreground "#065fff")
  '(marginalia-lighter :inherit shadow)
  '(marginalia-liqst :inherit shadow)
  '(marginalia-mode :foreground "#065fff")
  '(marginalia-modified :inherit warning)
  '(marginalia-null :inherit shadow)
  '(marginalia-number :foreground "#065fff")
  '(marginalia-size :foreground "#1f77bb")
  '(marginalia-string :foreground "#4250ef")
  '(marginalia-symbol :foreground "#ba35af")
  '(marginalia-type :foreground "#008858")
  '(marginalia-value :inherit shadow)
  '(marginalia-version :foreground "#cf25aa"))

;;; markdown-mode
(helheim-theme-set-faces 'ef-light
  '(markdown-blockquote-face :inherit font-lock-doc-face)
  '(markdown-bold-face :inherit bold)
  '(markdown-code-face :inherit fixed-pitch :background "#f9f9f9" :extend t)
  '(markdown-gfm-checkbox-face :foreground "#b6532f")
  '(markdown-header-face-1 :inherit bold :foreground "#4f54aa")
  '(markdown-header-face-2 :inherit bold :foreground "#ba35af")
  '(markdown-header-face-3 :inherit bold :foreground "#1f77bb")
  '(markdown-header-face-4 :inherit bold :foreground "#b65050")
  '(markdown-header-face-5 :inherit bold :foreground "#6052cf")
  '(markdown-header-face-6 :inherit bold :foreground "#d51272")
  '(markdown-highlighting-face :background "#d0efda" :foreground "#217a3c")
  '(markdown-inline-code-face :inherit fixed-pitch :foreground "#cf25aa")
  '(markdown-italic-face :inherit italic)
  '(markdown-language-keyword-face :inherit fixed-pitch :background "#efefef")
  '(markdown-line-break-face :inherit nobreak-space)
  '(markdown-link-face :inherit link)
  '(markdown-markup-face :inherit shadow)
  '(markdown-metadata-key-face :inherit bold)
  '(markdown-metadata-value-face :foreground "#4250ef")
  '(markdown-missing-link-face :inherit warning)
  '(markdown-pre-face :inherit markdown-code-face)
  '(markdown-table-face :inherit fixed-pitch :foreground "#397a70")
  '(markdown-url-face :foreground "#397a70"))

;;; mct
(helheim-theme-set-faces 'ef-light
  '(mct-highlight-candidate :background "#bfe8ff"))

;;; messages
(helheim-theme-set-faces 'ef-light
  '(message-cited-text-1 :foreground "#4250ef")
  '(message-cited-text-2 :foreground "#ba35af")
  '(message-cited-text-3 :foreground "#1f77bb")
  '(message-cited-text-4 :foreground "#b65050")
  '(message-header-name :inherit bold)
  '(message-header-newsgroups :inherit message-header-other)
  '(message-header-to :inherit bold :foreground "#6052cf")
  '(message-header-cc :foreground "#6052cf")
  '(message-header-subject :inherit bold :foreground "#065fff")
  '(message-header-xheader :inherit message-header-other)
  '(message-header-other :foreground "#1f6fbf")
  '(message-mml :foreground "#3f6faf")
  '(message-separator :background "#efefef" :foreground "#202020"))

;;; mode-line
(helheim-theme-set-faces 'ef-light
  '(mode-line :inherit variable-pitch :background "#b7c7ff" :foreground "#151515")
  '(mode-line-active :inherit mode-line)
  '(mode-line-buffer-id :inherit bold)
  '(mode-line-emphasis :inherit bold :foreground "#002fa0")
  '(mode-line-highlight :inherit highlight)
  '(mode-line-inactive :inherit variable-pitch :background "#dbdbdb" :foreground "#68759f"))

;;; mood-line
(helheim-theme-set-faces 'ef-light
  '(mood-line-modified :inherit italic)
  '(mood-line-status-error :inherit error)
  '(mood-line-status-info :foreground "#217a3c")
  '(mood-line-status-success :inherit success)
  '(mood-line-status-warning :inherit warning)
  '(mood-line-unimportant :inherit shadow))

;;; mu4e
(helheim-theme-set-faces 'ef-light
  '(mu4e-attach-number-face :inherit bold :foreground "#68759f")
  '(mu4e-cited-1-face :inherit message-cited-text-1)
  '(mu4e-cited-2-face :inherit message-cited-text-2)
  '(mu4e-cited-3-face :inherit message-cited-text-3)
  '(mu4e-cited-4-face :inherit message-cited-text-4)
  '(mu4e-cited-5-face :inherit message-cited-text-1)
  '(mu4e-cited-6-face :inherit message-cited-text-2)
  '(mu4e-cited-7-face :inherit message-cited-text-3)
  '(mu4e-compose-header-face :inherit mu4e-compose-separator-face)
  '(mu4e-compose-separator-face :inherit message-separator)
  '(mu4e-contact-face :inherit message-header-to)
  '(mu4e-context-face :inherit bold)
  '(mu4e-draft-face :foreground "#217a3c")
  '(mu4e-flagged-face :foreground "#6052cf")
  '(mu4e-footer-face :inherit italic :foreground "#397a70")
  '(mu4e-forwarded-face :inherit italic :foreground "#217a3c")
  '(mu4e-header-face :inherit shadow)
  '(mu4e-header-highlight-face :inherit hl-line)
  '(mu4e-header-key-face :inherit message-header-name)
  '(mu4e-header-marks-face :inherit mu4e-special-header-value-face)
  '(mu4e-header-title-face :foreground "#008858")
  '(mu4e-header-value-face :inherit message-header-other)
  '(mu4e-highlight-face :inherit (bold fixed-pitch) :foreground "#065fff")
  '(mu4e-link-face :inherit link)
  '(mu4e-moved-face :inherit italic :foreground "#b6532f")
  '(mu4e-ok-face :inherit success)
  '(mu4e-region-code :foreground "#ba35af")
  '(mu4e-related-face :inherit (italic shadow))
  '(mu4e-replied-face :foreground "#217a3c")
  '(mu4e-special-header-value-face :inherit message-header-subject)
  '(mu4e-system-face :inherit italic)
  '(mu4e-thread-fold-face :foreground "#bfc4da")
  '(mu4e-trashed-face :foreground "#e00033")
  '(mu4e-unread-face :inherit bold)
  '(mu4e-url-number-face :inherit shadow)
  '(mu4e-warning-face :inherit warning))

;;; nerd-icons
(helheim-theme-set-faces 'ef-light
  '(nerd-icons-blue :foreground "#065fff")
  '(nerd-icons-blue-alt :foreground "#4250ef")
  '(nerd-icons-cyan :foreground "#1f6fbf")
  '(nerd-icons-cyan-alt :foreground "#3f6faf")
  '(nerd-icons-dblue :foreground "#4f54aa")
  '(nerd-icons-dcyan :foreground "#506fa0")
  '(nerd-icons-dgreen :foreground "#61756c")
  '(nerd-icons-dmaroon :foreground "#af5a80")
  '(nerd-icons-dorange :foreground "#c24552")
  '(nerd-icons-dpink :foreground "#af5a80")
  '(nerd-icons-dpurple :foreground "#6052cf")
  '(nerd-icons-dred :foreground "#d3303a")
  '(nerd-icons-dsilver :foreground "#506fa0")
  '(nerd-icons-dyellow :foreground "#a65f6a")
  '(nerd-icons-green :foreground "#217a3c")
  '(nerd-icons-lblue :foreground "#065fff")
  '(nerd-icons-lcyan :foreground "#1f6fbf")
  '(nerd-icons-lgreen :foreground "#4a7d00")
  '(nerd-icons-lmaroon :foreground "#cf25aa")
  '(nerd-icons-lorange :foreground "#e00033")
  '(nerd-icons-lpink :foreground "#ba35af")
  '(nerd-icons-lpurple :foreground "#af5a80")
  '(nerd-icons-lred :foreground "#c24552")
  '(nerd-icons-lsilver :foreground "gray50")
  '(nerd-icons-lyellow :foreground "#b6532f")
  '(nerd-icons-maroon :foreground "#ba35af")
  '(nerd-icons-orange :foreground "#b6532f")
  '(nerd-icons-pink :foreground "#cf25aa")
  '(nerd-icons-purple :foreground "#6052cf")
  '(nerd-icons-purple-alt :foreground "#4250ef")
  '(nerd-icons-red :foreground "#d3303a")
  '(nerd-icons-red-alt :foreground "#d51272")
  '(nerd-icons-silver :foreground "gray50")
  '(nerd-icons-yellow :foreground "#a45f22"))

;;; nerd-icons-completion
(helheim-theme-set-faces 'ef-light
  '(nerd-icons-completion-dir-face :foreground "#4250ef"))

;;; nerd-icons-dired
(helheim-theme-set-faces 'ef-light
  '(nerd-icons-dired-dir-face :foreground "#4250ef"))

;;; nerd-icons-ibuffer
(helheim-theme-set-faces 'ef-light
  '(nerd-icons-ibuffer-dir-face :foreground "#4250ef")
  '(nerd-icons-ibuffer-file-face :foreground "#506fa0")
  '(nerd-icons-ibuffer-mode-face :foreground "#008858")
  '(nerd-icons-ibuffer-size-face :foreground "#1f77bb"))

;;; neotree
(helheim-theme-set-faces 'ef-light
  '(neo-banner-face :foreground "#4250ef")
  '(neo-button-face :inherit button)
  '(neo-header-face :inherit bold)
  '(neo-root-dir-face :inherit bold :foreground "#4250ef")
  '(neo-vc-added-face :inherit success)
  '(neo-vc-conflict-face :inherit error)
  '(neo-vc-edited-face :inherit italic)
  '(neo-vc-ignored-face :inherit shadow)
  '(neo-vc-missing-face :inherit error)
  '(neo-vc-needs-merge-face :inherit italic)
  '(neo-vc-needs-update-face :underline t)
  '(neo-vc-removed-face :strike-through t)
  '(neo-vc-unlocked-changes-face :inherit success)
  '(neo-vc-user-face :inherit warning))

;;; notmuch
(helheim-theme-set-faces 'ef-light
  '(notmuch-crypto-decryption :inherit bold)
  '(notmuch-crypto-part-header :foreground "#3f6faf")
  '(notmuch-crypto-signature-bad :inherit error)
  '(notmuch-crypto-signature-good :inherit success)
  '(notmuch-crypto-signature-good-key :inherit success)
  '(notmuch-crypto-signature-unknown :inherit warning)
  '(notmuch-jump-key :inherit (bold fixed-pitch) :foreground "#065fff")
  '(notmuch-message-summary-face :inherit bold :background "#dbdbdb")
  '(notmuch-search-count :foreground "#68759f")
  '(notmuch-search-date :foreground "#1f77bb")
  '(notmuch-search-flagged-face :foreground "#6052cf")
  '(notmuch-search-matching-authors :foreground "#6052cf")
  '(notmuch-search-non-matching-authors :inherit shadow)
  '(notmuch-search-subject :foreground "#202020")
  '(notmuch-search-unread-face :inherit bold)
  '(notmuch-tag-added :underline "#02af52")
  '(notmuch-tag-deleted :strike-through "#d63a44")
  '(notmuch-tag-face :foreground "#4250ef")
  '(notmuch-tag-flagged :foreground "#6052cf")
  '(notmuch-tag-unread :foreground "#cf25aa")
  '(notmuch-tree-match-author-face :inherit notmuch-search-matching-authors)
  '(notmuch-tree-match-date-face :inherit notmuch-search-date)
  '(notmuch-tree-match-face :foreground "#202020")
  '(notmuch-tree-match-tag-face :inherit notmuch-tag-face)
  '(notmuch-tree-no-match-face :inherit shadow)
  '(notmuch-tree-no-match-date-face :inherit shadow)
  '(notmuch-wash-cited-text :inherit message-cited-text-1)
  '(notmuch-wash-toggle-button :background "#efefef" :foreground "#397a70"))

;;; olivetti
(helheim-theme-set-faces 'ef-light
  '(olivetti-fringe :background unspecified))

;;; orderless
(helheim-theme-set-faces 'ef-light
  '(orderless-match-face-0 :inherit bold :foreground "#4250ef")
  '(orderless-match-face-1 :inherit bold :foreground "#cf25aa")
  '(orderless-match-face-2 :inherit bold :foreground "#008858")
  '(orderless-match-face-3 :inherit bold :foreground "#b6532f"))

;;; org

(helheim-theme-set-faces 'ef-light
  '(org-level-1 :foreground "#2c4ab0" :height 1.03)  ; original "#4f54aa"
  '(org-level-2 :foreground "#96197a" :height 1.03)  ; original "#ba35af"
  '(org-level-3 :foreground "#145688" :height 1.03)  ; original "#1f77bb"
  '(org-level-4 :foreground "#883b3b" :height 1.03)  ; original "#b65050"
  '(org-level-5 :foreground "#4d3ac3" :height 1.03)  ; original "#6052cf"
  '(org-level-6 :foreground "#a20b55" :height 1.03)  ; original "#d51272"
  '(org-level-7 :foreground "#00603c" :height 1.03)  ; original "#008858"
  '(org-level-8 :foreground "#7b4617" :height 1.03)) ; original "#a45f22"

(helheim-theme-set-faces 'ef-light
  '(org-node-context-origin-title :foreground "salmon4" :extend t)
  ;; '(org-node-context-origin-title :inherit magit-section-secondary-heading)
  )

(helheim-theme-set-faces 'ef-light
  '(org-agenda-calendar-daterange :foreground "#397a70")
  '(org-agenda-calendar-event :foreground "#397a70")
  '(org-agenda-calendar-sexp :inherit (italic org-agenda-calendar-event))
  '(org-agenda-clocking :background "#ffeabb" :foreground "#b6532f")
  '(org-agenda-column-dateline :background "#dbdbdb")
  '(org-agenda-current-time :foreground "#202020")
  '(org-agenda-date :foreground "#1f6fbf" :inherit bold)
  '(org-agenda-date-today :background "#e5e7ff" :inherit org-agenda-date) ;; "#ffeabb"
  '(org-agenda-date-weekend :foreground "#c24552" :inherit org-agenda-date)
  '(org-agenda-date-weekend-today :foreground "#c24552" :inherit org-agenda-date-today)
  '(org-agenda-diary :inherit org-agenda-calendar-sexp)
  '(org-agenda-dimmed-todo-face :inherit shadow)
  '(org-agenda-done :inherit org-done)
  '(org-agenda-filter-category :inherit bold :foreground "#9f0000")
  '(org-agenda-filter-effort :inherit bold :foreground "#9f0000")
  '(org-agenda-filter-regexp :inherit bold :foreground "#9f0000")
  '(org-agenda-filter-tags :inherit bold :foreground "#9f0000")
  '(org-agenda-restriction-lock :background "#efefef" :foreground "#68759f")
  '(org-agenda-structure :inherit bold :foreground "#397a70")
  '(org-agenda-structure-filter :inherit org-agenda-structure :foreground "#b6532f")
  '(org-agenda-structure-secondary :inherit font-lock-doc-face)
  '(org-archived :background "#dbdbdb" :foreground "#202020")
  '(org-block :inherit fixed-pitch :background "#f9f9f9" :extend t)
  '(org-block-begin-line :inherit (shadow fixed-pitch) :background "#efefef" :extend t)
  '(org-block-end-line :inherit org-block-begin-line)
  '(org-checkbox :inherit fixed-pitch :foreground "#b6532f")
  '(org-checkbox-statistics-done :inherit org-done)
  '(org-checkbox-statistics-todo :inherit org-todo)
  '(org-clock-overlay :background "#ccbfff")
  '(org-code :inherit fixed-pitch :foreground "#cf25aa")
  '(org-column :inherit default :background "#dbdbdb")
  '(org-column-title :inherit (fixed-pitch bold default) :underline t :background "#dbdbdb")
  '(org-date :inherit fixed-pitch :foreground "#1f77bb")
  '(org-date-selected :foreground "#1f77bb" :inverse-video t)
  '(org-dispatcher-highlight :inherit warning :background "#ffeabb")
  '(org-document-info :foreground "#397a70")
  '(org-document-info-keyword :inherit fixed-pitch :foreground "#68759f")
  '(org-document-title :inherit bold :foreground "#008858")
  '(org-done :foreground "#217a3c")
  '(org-drawer :inherit fixed-pitch :foreground "#68759f")
  '(org-footnote :inherit link)
  '(org-formula :inherit fixed-pitch :foreground "#e00033")
  '(org-headline-done :inherit org-done)
  '(org-headline-todo :inherit org-todo)
  '(org-hide :foreground "#ffffff")
  '(org-indent :inherit (fixed-pitch org-hide))
  '(org-imminent-deadline :inherit bold :foreground "#d3303a")
  '(org-latex-and-related :foreground "#008858")
  '(org-link :inherit link)
  '(org-list-dt :inherit bold)
  '(org-macro :inherit fixed-pitch :foreground "#008858")
  '(org-meta-line :inherit fixed-pitch :foreground "#68759f")
  '(org-mode-line-clock-overrun :inherit bold :foreground "#9f0000")
  '(org-priority :foreground "#61756c")
  '(org-property-value :inherit fixed-pitch :foreground "#397a70")
  '(org-quote :inherit org-block)
  '(org-scheduled :foreground "#a65f6a")
  '(org-scheduled-previously :inherit (bold org-scheduled-today))
  '(org-scheduled-today :foreground "#a45f22")
  '(org-sexp-date :foreground "#1f77bb")
  '(org-special-keyword :inherit (shadow fixed-pitch))
  '(org-table :inherit fixed-pitch :foreground "#397a70")
  '(org-table-header :inherit (bold org-table))
  '(org-tag :foreground "#61756c" :height 0.9)
  '(org-tag-group :inherit (bold org-tag))
  '(org-target :underline t)
  '(org-time-grid :foreground "#68759f")
  '(org-todo :foreground "#e00033")
  '(org-upcoming-deadline :foreground "#d51272")
  '(org-upcoming-distant-deadline :foreground "#202020")
  '(org-verbatim :inherit fixed-pitch :foreground "#4250ef")
  '(org-verse :inherit org-block)
  '(org-warning :inherit warning))

;;; org-habit
(helheim-theme-set-faces 'ef-light
  '(org-habit-alert-face :background "#ffcf00" :foreground "black")
  '(org-habit-alert-future-face :background "#f9ff00")
  '(org-habit-clear-face :background "#7f90ff" :foreground "black")
  '(org-habit-clear-future-face :background "#a6c0ff")
  '(org-habit-overdue-face :background "#ef7969")
  '(org-habit-overdue-future-face :background "#ffaab4")
  '(org-habit-ready-face :background "#45c050" :foreground "black")
  '(org-habit-ready-future-face :background "#75ef30"))

;;; outline-mode
(helheim-theme-set-faces 'ef-light
  '(outline-1 :inherit bold :foreground "#4f54aa")
  '(outline-2 :inherit bold :foreground "#ba35af")
  '(outline-3 :inherit bold :foreground "#1f77bb")
  '(outline-4 :inherit bold :foreground "#b65050")
  '(outline-5 :inherit bold :foreground "#6052cf")
  '(outline-6 :inherit bold :foreground "#d51272")
  '(outline-7 :inherit bold :foreground "#008858")
  '(outline-8 :inherit bold :foreground "#a45f22"))

;;; package (M-x list-packages)
(helheim-theme-set-faces 'ef-light
  '(package-description :foreground "#506fa0")
  '(package-help-section-name :inherit bold)
  '(package-name :inherit link)
  '(package-status-available :foreground "#1f77bb")
  '(package-status-avail-obso :inherit error)
  '(package-status-built-in :foreground "#ba35af")
  '(package-status-dependency :foreground "#b6532f")
  '(package-status-disabled :inherit error :strike-through t)
  '(package-status-from-source :foreground "#008858")
  '(package-status-held :foreground "#b6532f")
  '(package-status-incompat :inherit warning)
  '(package-status-installed :foreground "#397a70")
  '(package-status-new :inherit success)
  '(package-status-unsigned :inherit error))

;;; perspective
(helheim-theme-set-faces 'ef-light
  '(persp-selected-face :inherit mode-line-emphasis))

;;; proced
(helheim-theme-set-faces 'ef-light
  '(proced-cpu :foreground "#6052cf")
  '(proced-emacs-pid :foreground "#af5a80" :underline t)
  '(proced-executable :foreground "#6052cf")
  '(proced-interruptible-sleep-status-code :inherit shadow)
  '(proced-mem :foreground "#008858")
  '(proced-memory-high-usage :foreground "#e00033")
  '(proced-memory-low-usage :foreground "#217a3c")
  '(proced-memory-medium-usage :foreground "#b6532f")
  '(proced-pgrp :inherit proced-pid)
  '(proced-pid :foreground "#af5a80")
  '(proced-ppid :inherit proced-pid)
  '(proced-run-status-code :inherit success)
  '(proced-sess :inherit proced-pid)
  '(proced-session-leader-pid :inherit bold :foreground "#af5a80")
  '(proced-uninterruptible-sleep-status-code :inherit error))

;;; powerline
(helheim-theme-set-faces 'ef-light
  '(powerline-active0 :background "#68759f" :foreground "#ffffff")
  '(powerline-active1 :inherit mode-line)
  '(powerline-active2 :inherit mode-line-inactive)
  '(powerline-inactive0 :background "#b3b3b3" :foreground "#68759f")
  '(powerline-inactive1 :background "#ffffff" :foreground "#68759f")
  '(powerline-inactive2 :inherit mode-line-inactive))

;;; pulsar
(helheim-theme-set-faces 'ef-light
  '(pulsar-blue :background "#ccdfff" :extend t)
  '(pulsar-cyan :background "#bfefff" :extend t)
  '(pulsar-green :background "#b3fabf" :extend t)
  '(pulsar-magenta :background "#ffddff" :extend t)
  '(pulsar-red :background "#ffcfbf" :extend t)
  '(pulsar-yellow :background "#fff576" :extend t))

;;; pulse
(helheim-theme-set-faces 'ef-light
  '(pulse-highlight-start-face :background "#ccdfff" :extend t))

;;; rainbow-delimiters
(helheim-theme-set-faces 'ef-light
  '(rainbow-delimiters-base-error-face :inherit (bold rainbow-delimiters-mismatched-face))
  '(rainbow-delimiters-base-face :foreground "#008858")
  '(rainbow-delimiters-depth-1-face :foreground "#008858")
  '(rainbow-delimiters-depth-2-face :foreground "#4f54aa")
  '(rainbow-delimiters-depth-3-face :foreground "#ba35af")
  '(rainbow-delimiters-depth-4-face :foreground "#1f77bb")
  '(rainbow-delimiters-depth-5-face :foreground "#b65050")
  '(rainbow-delimiters-depth-6-face :foreground "#6052cf")
  '(rainbow-delimiters-depth-7-face :foreground "#d51272")
  '(rainbow-delimiters-depth-8-face :foreground "#008858")
  '(rainbow-delimiters-depth-9-face :foreground "#a45f22")
  '(rainbow-delimiters-mismatched-face :background "#ff8f88" :foreground "#000000")
  '(rainbow-delimiters-unmatched-face :inherit (bold rainbow-delimiters-mismatched-face)))

;;; rcirc
(helheim-theme-set-faces 'ef-light
  '(rcirc-bright-nick :inherit bold :foreground "#000000")
  '(rcirc-dim-nick :inherit shadow)
  '(rcirc-monospace-text :inherit fixed-pitch)
  '(rcirc-my-nick :inherit bold :foreground "#cf25aa")
  '(rcirc-nick-in-message :inherit rcirc-my-nick)
  '(rcirc-nick-in-message-full-line :inherit rcirc-my-nick)
  '(rcirc-other-nick :inherit bold :foreground "#4250ef")
  '(rcirc-prompt :inherit minibuffer-prompt)
  '(rcirc-server :inherit font-lock-comment-face)
  '(rcirc-timestamp :foreground "#1f77bb")
  '(rcirc-track-keyword :inherit bold :foreground "#5f0070")
  '(rcirc-track-nick :inherit rcirc-my-nick)
  '(rcirc-url :inherit link))

;;; recursion-indicator
(helheim-theme-set-faces 'ef-light
  '(recursion-indicator-general :foreground "#9f0000")
  '(recursion-indicator-minibuffer :foreground "#002fa0"))

;;; regexp-builder (re-builder)
(helheim-theme-set-faces 'ef-light
  '(reb-match-0 :background "#df8fff" :foreground "#000000")
  '(reb-match-1 :background "#9adf90" :foreground "#000000")
  '(reb-match-2 :background "#ffcfbf" :foreground "#000000")
  '(reb-match-3 :background "#bfefff" :foreground "#000000")
  '(reb-regexp-grouping-backslash :inherit font-lock-regexp-grouping-backslash)
  '(reb-regexp-grouping-construct :inherit font-lock-regexp-grouping-construct))

;;; rst-mode
(helheim-theme-set-faces 'ef-light
  '(rst-level-1 :inherit bold :foreground "#4f54aa")
  '(rst-level-2 :inherit bold :foreground "#ba35af")
  '(rst-level-3 :inherit bold :foreground "#1f77bb")
  '(rst-level-4 :inherit bold :foreground "#b65050")
  '(rst-level-5 :inherit bold :foreground "#6052cf")
  '(rst-level-6 :inherit bold :foreground "#d51272"))

;;; ruler-mode
(helheim-theme-set-faces 'ef-light
  '(ruler-mode-column-number :inherit ruler-mode-default)
  '(ruler-mode-comment-column :inherit ruler-mode-default :foreground "#d3303a")
  '(ruler-mode-current-column :inherit ruler-mode-default :background "#b3b3b3" :foreground "#000000")
  '(ruler-mode-default :inherit default :background "#efefef" :foreground "#68759f")
  '(ruler-mode-fill-column :inherit ruler-mode-default :foreground "#217a3c")
  '(ruler-mode-fringes :inherit ruler-mode-default :foreground "#1f6fbf")
  '(ruler-mode-goal-column :inherit ruler-mode-default :foreground "#3740cf")
  '(ruler-mode-margins :inherit ruler-mode-default :foreground "#ffffff")
  '(ruler-mode-pad :inherit ruler-mode-default :background "#dbdbdb" :foreground "#68759f")
  '(ruler-mode-tab-stop :inherit ruler-mode-default :foreground "#a45f22"))

;;; shortdoc
(helheim-theme-set-faces 'ef-light
  '(shortdoc-heading :inherit bold))

;;; show-paren-mode
(helheim-theme-set-faces 'ef-light
  '(show-paren-match :background "#dfa0f3" :foreground "#000000")
  '(show-paren-match-expression :background "#dbdbdb")
  '(show-paren-mismatch :background "#ff8f88" :foreground "#000000"))

;;; shell-script-mode (sh-mode)
(helheim-theme-set-faces 'ef-light
  '(sh-heredoc :inherit font-lock-doc-face)
  '(sh-quoted-exec :inherit font-lock-builtin-face))

;;; shr
(helheim-theme-set-faces 'ef-light
  '(shr-code :inherit fixed-pitch :foreground "#cf25aa")
  '(shr-h1 :inherit bold :foreground "#4f54aa")
  '(shr-h2 :inherit bold :foreground "#ba35af")
  '(shr-h3 :inherit bold :foreground "#1f77bb")
  '(shr-h4 :inherit bold :foreground "#b65050")
  '(shr-h5 :inherit bold :foreground "#6052cf")
  '(shr-h6 :inherit bold :foreground "#d51272")
  '(shr-mark :inherit match)
  '(shr-selected-link :inherit link :background "#efefef"))

;;; smerge
(helheim-theme-set-faces 'ef-light
  '(smerge-base :inherit diff-changed)
  '(smerge-lower :inherit diff-added)
  '(smerge-markers :inherit diff-header)
  '(smerge-refined-added :inherit diff-refine-added)
  '(smerge-refined-removed :inherit diff-refine-removed)
  '(smerge-upper :inherit diff-removed))

;;; spacious-padding
(helheim-theme-set-faces 'ef-light
  '(spacious-padding-subtle-mode-line-active :foreground "#4250ef")
  '(spacious-padding-subtle-mode-line-inactive :foreground "#bfc4da"))

;;; tab-bar-mode
(helheim-theme-set-faces 'ef-light
  '(tab-bar :inherit variable-pitch :background "#dbdbdb")
  '(tab-bar-tab-group-current :inherit bold :background "#ffffff" :box (:line-width -2 :color "#ffffff") :foreground "#397a70")
  '(tab-bar-tab-group-inactive :background "#dbdbdb" :box (:line-width -2 :color "#dbdbdb") :foreground "#397a70")
  '(tab-bar-tab :inherit bold :box (:line-width -2 :color "#ffffff") :background "#ffffff")
  '(tab-bar-tab-inactive :box (:line-width -2 :color "#b3b3b3") :background "#b3b3b3")
  '(tab-bar-tab-ungrouped :inherit tab-bar-tab-inactive))

;;; tab-line-mode
(helheim-theme-set-faces 'ef-light
  '(tab-line :inherit variable-pitch :background "#dbdbdb" :height 0.95)
  '(tab-line-close-highlight :foreground "#e00033")
  '(tab-line-highlight :inherit highlight)
  '(tab-line-tab-current :inherit bold :box (:line-width -2 :color "#ffffff") :background "#ffffff")
  '(tab-line-tab-inactive :box (:line-width -2 :color "#b3b3b3") :background "#b3b3b3")
  '(tab-line-tab-inactive-alternate :inherit tab-line-tab-inactive :foreground "#397a70")
  '(tab-line-tab-modified :foreground "#b6532f"))

;;; tempel
(helheim-theme-set-faces 'ef-light
  '(tempel-default :inherit italic :background "#dbdbdb" :foreground "#397a70")
  '(tempel-field :background "#d0efda" :foreground "#217a3c")
  '(tempel-form :background "#ffd5ea" :foreground "#e00033"))

;;; term
(helheim-theme-set-faces 'ef-light
  '(term :background "#ffffff" :foreground "#202020")
  '(term-bold :inherit bold)
  '(term-color-black :background "gray35" :foreground "gray35")
  '(term-color-blue :background "#4250ef" :foreground "#4250ef")
  '(term-color-cyan :background "#1f6fbf" :foreground "#1f6fbf")
  '(term-color-green :background "#217a3c" :foreground "#217a3c")
  '(term-color-magenta :background "#ba35af" :foreground "#ba35af")
  '(term-color-red :background "#d51272" :foreground "#d51272")
  '(term-color-white :background "white" :foreground "white")
  '(term-color-yellow :background "#a45f22" :foreground "#a45f22")
  '(term-underline :underline t))

;;; tldr
(helheim-theme-set-faces 'ef-light
  '(tldr-command-argument :inherit font-lock-string-face)
  '(tldr-command-itself :inherit font-lock-builtin-face)
  '(tldr-description :inherit font-lock-doc-face)
  '(tldr-introduction :inherit font-lock-comment-face)
  '(tldr-title :inherit bold))

;;; transient
(helheim-theme-set-faces 'ef-light
  '(transient-active-infix :background "#b3b3b3" :foreground "#000000")
  '(transient-amaranth :inherit bold :foreground "#b6532f")
  '(transient-argument :inherit warning :background "#ffeabb")
  '(transient-blue :inherit bold :foreground "#065fff")
  '(transient-disabled-suffix :strike-through t)
  '(transient-enabled-suffix :inherit success :background "#d0efda")
  '(transient-heading :inherit bold)
  '(transient-inactive-argument :inherit shadow)
  '(transient-inactive-value :inherit shadow)
  '(transient-key :inherit (bold fixed-pitch) :foreground "#065fff")
  '(transient-key-exit :inherit (bold fixed-pitch) :foreground "#065fff")
  '(transient-key-noop :inherit (shadow bold fixed-pitch) :foreground "#065fff")
  '(transient-key-return :inherit (bold fixed-pitch) :foreground "#065fff")
  '(transient-key-stay :inherit (bold fixed-pitch) :foreground "#065fff")
  '(transient-mismatched-key :underline t)
  '(transient-nonstandard-key :underline t)
  '(transient-pink :inherit bold :foreground "#ba35af")
  '(transient-purple :inherit bold :foreground "#6052cf")
  '(transient-red :inherit bold :foreground "#d3303a")
  '(transient-teal :inherit bold :foreground "#1f77bb")
  '(transient-unreachable :inherit shadow)
  '(transient-unreachable-key :inherit shadow)
  '(transient-value :inherit success :background "#d0efda"))

;;; trashed
(helheim-theme-set-faces 'ef-light
  '(trashed-restored :inherit warning :background "#ffeabb"))

;;; tree-sitter
(helheim-theme-set-faces 'ef-light
  '(tree-sitter-hl-face:attribute :inherit font-lock-variable-name-face)
  '(tree-sitter-hl-face:constant.builtin :inherit tree-sitter-hl-face:constant)
  '(tree-sitter-hl-face:escape :inherit font-lock-regexp-grouping-backslash)
  '(tree-sitter-hl-face:function :inherit font-lock-function-name-face)
  '(tree-sitter-hl-face:function.call :inherit tree-sitter-hl-face:function)
  '(tree-sitter-hl-face:operator :inherit bold)
  '(tree-sitter-hl-face:property.definition :inherit font-lock-variable-name-face)
  '(tree-sitter-hl-face:punctuation.special :inherit font-lock-regexp-grouping-construct)
  '(tree-sitter-hl-face:string.special :inherit tree-sitter-hl-face:string)
  '(tree-sitter-hl-face:tag :inherit font-lock-function-name-face))

;;; tty-menu
(helheim-theme-set-faces 'ef-light
  '(tty-menu-disabled-face :background "#dbdbdb" :foreground "#68759f")
  '(tty-menu-enabled-face :background "#dbdbdb" :foreground "#000000")
  '(tty-menu-selected-face :inherit highlight))

;;; vc (vc-dir.el, vc-hooks.el)
(helheim-theme-set-faces 'ef-light
  '(vc-dir-file :foreground "#6052cf")
  '(vc-dir-header :inherit bold)
  '(vc-dir-header-value :foreground "#4250ef")
  '(vc-dir-mark-indicator :foreground "#000000")
  '(vc-dir-status-edited :inherit italic)
  '(vc-dir-status-ignored :inherit shadow)
  '(vc-dir-status-up-to-date :foreground "#217a3c")
  '(vc-dir-status-warning :inherit error)
  '(vc-conflict-state :inherit error)
  '(vc-edited-state :inherit italic)
  '(vc-git-log-edit-summary-max-warning :foreground "#e00033")
  '(vc-git-log-edit-summary-target-warning :foreground "#b6532f")
  '(vc-locally-added-state :inherit italic)
  '(vc-locked-state :inherit success)
  '(vc-missing-state :inherit error)
  '(vc-needs-update-state :inherit error)
  '(vc-removed-state :inherit error))

;;; vertico
(helheim-theme-set-faces 'ef-light
  '(vertico-current :background "#bfe8ff")
  '(vertico-group-title :height 0.95 :foreground "#6052cf" :inherit bold))

;;; vertico-quick
(helheim-theme-set-faces 'ef-light
  '(vertico-quick1 :inherit bold :background "#7feaff")
  '(vertico-quick2 :inherit bold :background "#ffaaff"))

;;; vterm
(helheim-theme-set-faces 'ef-light
  '(vterm-color-black :background "black" :foreground "black")
  '(vterm-color-blue :background "#4250ef" :foreground "#4250ef")
  '(vterm-color-cyan :background "#1f6fbf" :foreground "#1f6fbf")
  '(vterm-color-default :background "#ffffff" :foreground "#202020")
  '(vterm-color-green :background "#217a3c" :foreground "#217a3c")
  '(vterm-color-inverse-video :background "#ffffff" :inverse-video t)
  '(vterm-color-magenta :background "#ba35af" :foreground "#ba35af")
  '(vterm-color-red :background "#d51272" :foreground "#d51272")
  '(vterm-color-underline :underline t)
  '(vterm-color-white :background "gray65" :foreground "gray65")
  '(vterm-color-yellow :background "#a45f22" :foreground "#a45f22"))

;;; vundo
(helheim-theme-set-faces 'ef-light
  '(vundo-default :inherit shadow)
  '(vundo-highlight :inherit (bold vundo-node) :foreground "#e00033")
  '(vundo-last-saved :inherit (bold vundo-node) :foreground "#000000")
  '(vundo-saved :inherit vundo-node :foreground "#000000"))

;;; wgrep
(helheim-theme-set-faces 'ef-light
  '(wgrep-delete-face :inherit warning)
  '(wgrep-done-face :background "#d0efda" :foreground "#217a3c")
  '(wgrep-face :inherit bold)
  '(wgrep-file-face :foreground "#397a70")
  '(wgrep-reject-face :background "#ffd5ea" :foreground "#e00033"))

;;; which-function-mode
(helheim-theme-set-faces 'ef-light
  '(which-func :inherit bold :foreground "#000000"))

;;; which-key
(helheim-theme-set-faces 'ef-light
  '(which-key-command-description-face :foreground "#202020")
  '(which-key-group-description-face :foreground "#6052cf")
  '(which-key-highlighted-command-face :foreground "#b6532f" :underline t)
  '(which-key-key-face :inherit (bold fixed-pitch) :foreground "#065fff")
  '(which-key-local-map-description-face :foreground "#202020")
  '(which-key-note-face :inherit shadow)
  '(which-key-separator-face :inherit shadow)
  '(which-key-special-key-face :inherit error))

;;; whitespace-mode
(helheim-theme-set-faces 'ef-light
  '(whitespace-big-indent :background "#fac200")
  '(whitespace-empty :background unspecified)
  '(whitespace-hspace :background unspecified :foreground "#bfc4da")
  '(whitespace-indentation :background unspecified :foreground "#bfc4da")
  '(whitespace-line :background unspecified :foreground "#b6532f")
  '(whitespace-newline :background unspecified :foreground "#bfc4da")
  '(whitespace-space :background unspecified :foreground "#bfc4da")
  '(whitespace-space-after-tab :inherit warning :background unspecified)
  '(whitespace-space-before-tab :inherit warning :background unspecified)
  '(whitespace-tab :background unspecified :foreground "#bfc4da")
  '(whitespace-trailing :background "#fac200"))

;;; widget
(helheim-theme-set-faces 'ef-light
  '(widget-button :inherit bold :foreground "#4250ef")
  '(widget-button-pressed :inherit widget-button :foreground "#ba35af")
  '(widget-documentation :inherit font-lock-doc-face)
  '(widget-field :background "#dbdbdb" :foreground "#202020" :extend nil)
  '(widget-inactive :inherit shadow :background "#efefef")
  '(widget-single-line-field :inherit widget-field))

;;; window-divider-mode
(helheim-theme-set-faces 'ef-light
  '(window-divider :foreground "#bfc4da")
  '(window-divider-first-pixel :foreground "#f9f9f9")
  '(window-divider-last-pixel :foreground "#f9f9f9"))

;;; window-tool-bar-mode
(helheim-theme-set-faces 'ef-light
  '(window-tool-bar-button :inherit variable-pitch :box (:line-width 1 :color "#bfc4da" :style released-button) :background "#b3b3b3" :foreground "#000000")
  '(window-tool-bar-button-hover :inherit (highlight variable-pitch) :box (:line-width 1 :color "#bfc4da" :style released-button) :background "#b3b3b3" :foreground "#000000")
  '(window-tool-bar-button-disabled :inherit variable-pitch :box (:line-width 1 :color "#bfc4da" :style released-button) :background "#b3b3b3" :foreground "#000000" :background "#efefef" :foreground "#68759f"))

;;; writegood-mode
(helheim-theme-set-faces 'ef-light
  '(writegood-duplicates-face :underline (:style wave :color "#d63a44"))
  '(writegood-passive-voice-face :underline (:style wave :color "#02af52"))
  '(writegood-weasels-face :underline (:style wave :color "#bf5f00")))

;;; woman
(helheim-theme-set-faces 'ef-light
  '(woman-addition :foreground "#008858")
  '(woman-bold :inherit bold :foreground "#4250ef")
  '(woman-italic :inherit italic :foreground "#cf25aa")
  '(woman-unknown :foreground "#b6532f"))

;;; ztree
(helheim-theme-set-faces 'ef-light
  '(ztreep-arrow-face :inherit shadow)
  '(ztreep-diff-header-face :inherit bold :foreground "#008858")
  '(ztreep-diff-header-small-face :inherit font-lock-doc-face)
  '(ztreep-diff-model-add-face :foreground "#217a3c")
  '(ztreep-diff-model-diff-face :foreground "#e00033")
  '(ztreep-diff-model-ignored-face :foreground "#68759f" :strike-through t)
  '(ztreep-expand-sign-face :inherit shadow)
  '(ztreep-header-face :inherit bold :foreground "#008858")
  '(ztreep-node-count-children-face :inherit (shadow italic)))

;;; .
(provide 'helheim-ef-light)
;;; helheim-ef-light.el end here
