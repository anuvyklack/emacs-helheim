;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Yuriy Artemyev
;; URL: https://github.com/anuvyklack/emacs-helheim
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.0.1
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;; Code:
;;; Fonts

;; (set-face-attribute 'fixed-pitch nil
;;                     :font (font-spec :family "PragmataPro Mono Liga" :size 13.9))
;; (set-face-attribute 'fixed-pitch-serif nil
;;                     :family "Iosevka Term Curly Slab Medium")

(progn
  (setq use-default-font-for-symbols t)
  (set-face-font 'default     (font-spec :family "PragmataPro Mono Liga" :size 13.9))
  (set-face-font 'fixed-pitch (font-spec :family "PragmataPro Mono Liga" :size 13.9))
  ;; (set-fontset-font t (cons ?󰀁 ?󱫰) "Symbols Nerd Font")
  (set-fontset-font t (cons ?󰀁 ?󱫰) "Symbols Nerd Font Mono"))

;; (progn
;;   (setq use-default-font-for-symbols nil)
;;   (set-face-font 'default (font-spec :family "Inconsolata LGC" :size 17))
;;   ;; (set-fontset-font t (cons ?󰀁 ?󱫰) "Symbols Nerd Font")
;;   (set-fontset-font t (cons ?󰀁 ?󱫰) "Symbols Nerd Font Mono")
;;   ;; Unicode Symbols for Legacy Computing
;;   (set-fontset-font t (cons ?🬀 ?🯊) "LegacyComputing")
;;   (set-fontset-font t (cons ?🯰 ?🯹) "LegacyComputing"))

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
(require 'helheim-keybindings)

(require 'helheim-emacs-lisp)

(require 'helheim-dired)
(require 'helheim-outline)
(require 'helheim-tab-bar)
(require 'helheim-xref)

(require 'helheim-info)
(require 'helheim-man)

(require 'helheim-vertico)
(require 'helheim-consult)
(require 'helheim-embark)

(require 'helheim-deadgrep)
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

;;;; Extra facilities
;;;;; repeat-mode

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

;;;;; dired

;;;;;;  Extra highlighting

;; (use-package dired-rainbow
;;   :ensure t
;;   :after dired
;;   :config
;;   (progn
;;     (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
;;     (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
;;     (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
;;     (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
;;     (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
;;     (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
;;     (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
;;     (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
;;     (dired-rainbow-define log "#c17d11" ("log"))
;;     (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
;;     (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
;;     (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
;;     (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
;;     (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
;;     (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
;;     (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
;;     (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
;;     (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
;;     (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
;;     (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
;;     ))

;;;; Keybindings

(use-package helix-leader
  :custom
  (helix-leader-send-C-x-with-control-modifier nil))

(helix-keymap-global-set
  "M-;"   'eval-expression
  "C-M-;" 'repeat-complex-command)

(helix-keymap-global-set :state '(normal motion)
  "<backspace>" 'execute-extended-command)

(helix-keymap-global-set :state 'normal
  "M-;"  nil ; unbind `helix-exchange-point-and-mark'
  "C-;" 'helix-exchange-point-and-mark)

;; "C-w"
(helix-keymap-set helix-window-map
  "N" 'other-tab-prefix)

(helix-keymap-global-set :state 'insert
  "C-w" 'backward-kill-word ; along with "C-backspace"
  "C-/" 'dabbrev-expand)

;;; init.el ends here
