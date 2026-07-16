;;; helheim-completion.el         -*- lexical-binding: t; no-byte-compile: t -*-
;;; Keybindings

(setup corfu
  (:after-load
    (:keymap corfu-map
      (:bind
        "<tab>"     'corfu-next
        "<backtab>" 'corfu-previous ;; S-<tab>
        "M-<tab>"   'corfu-expand
        "RET"       'corfu-insert
        "M-SPC"     'corfu-insert-separator
        ;;
        "C-h"       'corfu-info-documentation
        "C-l"       'corfu-complete
        "C-<i>"     'corfu-info-documentation
        "C-d"       'corfu-info-location ;; "gd" is go to definition
        ;; Scrolling
        "C-f"       'corfu-scroll-down
        "C-b"       'corfu-scroll-up
        ;; All versions of up/down
        "C-j"       'corfu-next
        "C-k"       'corfu-previous
        "M-j"       'corfu-next
        "M-k"       'corfu-previous
        "C-n"       'corfu-next
        "C-p"       'corfu-previous))))

;;; Config

(setup orderless
  (:install t)
  (:setopt completion-styles '(orderless basic)
           ;; completion-styles '(orderless partial-completion basic)
           completion-category-defaults nil
           completion-category-overrides '((file (styles orderless partial-completion)))
           completion-pcm-leading-wildcard t ;; Emacs 31
           orderless-matching-styles '(orderless-literal
                                       orderless-regexp
                                       ;; orderless-flex
                                       )
           orderless-component-separator #'orderless-escapable-split-on-space))

(setup corfu
  (:install t)
  (:setopt tab-always-indent 'complete
           tab-first-completion 'word
           ;; Disable Ispell completion function. As an alternative try `cape-dict'.
           text-mode-ispell-word-completion nil)
  (:setopt global-corfu-minibuffer t
           corfu-auto t
           corfu-auto-delay 0.24
           corfu-auto-prefix 2
           corfu-cycle t
           corfu-count 16
           corfu-max-width 120
           corfu-quit-at-boundary 'separator ;; M-SPC to continue completion.
           corfu-quit-no-match 'separator
           ;; When the completion popup is visible, by default the current
           ;; candidate is previewed into the buffer, and further input commits
           ;; that candidate as previewed. The feature is in line with other
           ;; common editors.
           ;;   - t :: non-inserting preview
           corfu-preview-current 'insert
           corfu-preselect 'prompt
           corfu-on-exact-match nil) ;; Handling of exact matches.
  (global-corfu-mode))

(setup corfu-history
  (:hook global-corfu-mode-hook corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(setup corfu-popupinfo
  (:hook global-corfu-mode-hook corfu-popupinfo-mode)
  (:setopt corfu-popupinfo-delay '(0.8 . 0.5))
  (:after-load
    (:keymap corfu-popupinfo-map
      (:bind "C-<i>" 'corfu-popupinfo-toggle))))

(setup nerd-icons-corfu
  (:install t)
  (:after corfu)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(setup cape
  (:install t)
  ;; Add to the global default value of `completion-at-point-functions'
  ;; which is used by `completion-at-point'.
  (:hook completion-at-point-functions (cape-dabbrev
                                        cape-file
                                        cape-elisp-block
                                        cape-history)))

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

(provide 'helheim-completion)
;;; helheim-completion.el ends here
