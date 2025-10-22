;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Yuriy Artemyev
;; URL: https://github.com/anuvyklack/emacs-twist
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.0.1
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;; Code:
;;; Fonts

(set-face-font 'default     (font-spec :family "PragmataPro Mono Liga" :size 13.9))
(set-face-font 'fixed-pitch (font-spec :family "PragmataPro Mono Liga" :size 13.9))
(setq use-default-font-for-symbols t)

;; (set-face-font 'default (font-spec :family "Inconsolata LGC" :size 17))
;; (setopt use-default-font-for-symbols nil)

;; (set-face-attribute 'fixed-pitch nil
;;                     :font (font-spec :family "PragmataPro Mono Liga" :size 13.9))
;; (set-face-attribute 'fixed-pitch-serif nil
;;                     :family "Iosevka Term Curly Slab Medium")

;; Nerd Font
(set-fontset-font t (cons ?\xf0001 ?\xf1af0) "Symbols Nerd Font Mono" nil 'prepend)

;; Unicode Symbols for Legacy Computing
(set-fontset-font t (cons ?\x1fb00 ?\x1fbca) "LegacyComputing" nil 'prepend)
(set-fontset-font t (cons ?🯰 ?🯹) "LegacyComputing" nil 'prepend)

;; (char-to-string ?\x1fb00) ;; ?🬀
;; (char-to-string ?\x1fbca) ;; ?🯊

;;; Package manager

;; In case you use VPN:
;;
;; Emacs populates `url-proxy-services' variable from:
;; `https_proxy', `socks_proxy', `no_proxy' environment variables.
(setq url-proxy-services '(("socks" . "127.0.0.1:10808")
                           ("https" . "127.0.0.1:10809"))
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'helheim-elpaca)

;;; Color theme
(require 'helheim-utils)

(use-package ef-themes
  :ensure (ef-themes :host github :repo "anuvyklack/ef-themes" :wait t)
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  ;; Use `ef-themes-toggle' to cycle between these themes.
  (ef-themes-to-toggle '(ef-light ef-dream))
  :config
  (mapc #'disable-theme custom-enabled-themes) ; Disable all other themes.
  (load-theme 'ef-light :no-confirm)
  ;; Load my customizations
  (helheim-load-file "modules/ef-light.el")
  ;; (load-file (file-name-concat helheim-root-directory "modules/ef-light.el"))
  (enable-theme 'ef-light))

;;; Helheim modules

(require 'helheim-core)
(require 'helheim-emacs-lisp)
(require 'helheim-keybindings)

(require 'helheim-vertico)
(require 'helheim-consult)
(require 'helheim-embark)
(require 'helheim-xref)

(require 'helheim-deadgrep)
;; (require 'helheim-dired)
(require 'helheim-outline)
(require 'helheim-tab-bar)
;; (require 'helheim-edit-indirect)

;;; Config
;;;; Appearance
;;;;; Colorize strings that represent colors

(use-package rainbow-mode
  :ensure t
  :blackout t
  :hook (emacs-lisp-mode-hook
         conf-mode-hook
         fish-mode-hook
         toml-ts-mode-hook))

;;;;; DISABLED show time in tab bar

;; (use-package time
;;   :custom
;;   (display-time-24hr-format t)
;;   (display-time-use-mail-icon t)
;;   :hook (elpaca-after-init-hook . display-time-mode))

;;;; repeat-mode

;; Evaluate `describe-repeat-maps' to see all repeatable commands.
(use-package repeat
  :hook (elpaca-after-init-hook . repeat-mode)
  :custom
  (repeat-exit-key "<escape>")
  (repeat-exit-timeout 2)
  (repeat-check-key nil)
  ;; :config
  ;; ;; Disable repeating for following commands
  ;; (put 'tab-next     'repeat-map nil)
  ;; (put 'tab-previous 'repeat-map nil)
  )

;;;; Keybindings

(use-package helix-leader
  :custom
  (helix-leader-send-C-x-with-control-modifier nil))

(helix-keymap-global-set :state '(normal motion)
  "<backspace>" 'execute-extended-command)

(helix-keymap-global-set
  "M-;"   'eval-expression
  "C-M-;" 'repeat-complex-command)

(helix-keymap-global-set :state 'normal
  "M-;"   nil ;; helix-exchange-point-and-mark
  "C-;"   'helix-exchange-point-and-mark)

;; "C-w"
(helix-keymap-set helix-window-map
  "N" 'other-tab-prefix)

(helix-keymap-global-set :state 'insert
  "C-w" 'backward-kill-word ;; along with "C-backspace"
  "C-/" 'dabbrev-expand)

;;; init.el ends here
