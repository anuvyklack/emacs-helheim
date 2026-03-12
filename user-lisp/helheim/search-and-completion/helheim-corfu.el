;;; helheim-corfu.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Keybindings

(defvar-keymap corfu-map
  :doc "Keymap used when popup is shown."
  "<tab>"     'corfu-next
  "<backtab>" 'corfu-previous ; S-<tab>
  "M-<tab>"   'corfu-expand
  "RET"       'corfu-insert
  "M-SPC"     'corfu-insert-separator
  "C-g"       'corfu-quit

  "C-h"       'corfu-info-documentation
  "C-j"       'corfu-next
  "C-k"       'corfu-previous
  "C-l"       'corfu-complete
  "C-<i>"     'corfu-info-documentation
  "C-d"       'corfu-info-location ; "gd" is go to definition

  ;; Scrolling
  "C-f"       'corfu-scroll-down
  "C-b"       'corfu-scroll-up

  ;; All versions of up/down
  "M-j"       'corfu-next
  "M-k"       'corfu-previous
  "C-n"       'corfu-next
  "C-p"       'corfu-previous
  "M-n"       'corfu-next
  "M-p"       'corfu-previous
  "<down>"    'corfu-next
  "<up>"      'corfu-previous

  "<remap> <next-line>" 'corfu-next
  "<remap> <previous-line>" 'corfu-previous
  "<remap> <move-beginning-of-line>" 'corfu-prompt-beginning
  "<remap> <move-end-of-line>" 'corfu-prompt-end
  "<remap> <scroll-down-command>" 'corfu-scroll-down
  "<remap> <scroll-up-command>" 'corfu-scroll-up
  "<remap> <beginning-of-buffer>" 'corfu-first
  "<remap> <end-of-buffer>" 'corfu-last
  "<remap> <completion-at-point>" 'corfu-complete
  "<remap> <keyboard-escape-quit>" 'corfu-reset)

(defvar-keymap corfu-popupinfo-map
  :doc "Additional keymap activated in popupinfo mode."
  "C-<i>" 'corfu-popupinfo-toggle
  "<remap> <corfu-info-documentation>" 'corfu-popupinfo-documentation
  "<remap> <corfu-info-location>" 'corfu-popupinfo-location
  "<remap> <scroll-other-window>" 'corfu-popupinfo-scroll-up
  "<remap> <scroll-other-window-down>" 'corfu-popupinfo-scroll-down
  "<remap> <end-of-buffer-other-window>" 'corfu-popupinfo-end
  "<remap> <beginning-of-buffer-other-window>" 'corfu-popupinfo-beginning)

;;; Config

(leaf corfu
  :straight t
  :global-minor-mode global-corfu-mode
  :custom
  (corfu-auto . t)
  (corfu-auto-delay . 0.24)
  (corfu-auto-prefix . 2)
  (corfu-cycle . t)
  (corfu-count . 16)
  (corfu-max-width . 120)
  (corfu-quit-at-boundary . 'separator) ;; M-SPC to continue completion.
  (corfu-quit-no-match . 'separator)
  ;; When the completion popup is visible, by default the current candidate is
  ;; previewed into the buffer, and further input commits that candidate as
  ;; previewed. The feature is in line with other common editors.
  ;; - t :: non-inserting preview
  (corfu-preview-current . 'insert)
  (corfu-preselect . 'prompt)
  (corfu-on-exact-match . nil) ;; Handling of exact matches.
  (global-corfu-minibuffer . t)
  (tab-always-indent . 'complete)
  (tab-first-completion . 'word)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion . nil))

(leaf corfu-history
  :hook (global-corfu-mode-hook . corfu-history-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))

(leaf corfu-popupinfo
  :hook (global-corfu-mode-hook . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.5)))

(leaf nerd-icons-corfu
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(leaf cape
  :straight t
  :config
  ;; Add to the global default value of `completion-at-point-functions'
  ;; which is used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history))

;;; Commands

(defun +corfu-move-to-minibuffer ()
  "Move list of candidates to your choice of minibuffer completion UI."
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           (completion-cycle-threshold nil)
           (completion-cycling nil))
       (consult-completion-in-region beg end table pred)))))

(provide 'helheim-corfu)
;;; helheim-corfu.el ends here
