;;; helheim-browser.el            -*- lexical-binding: t; no-byte-compile: t -*-

(setup atomic-chrome
  (:install atomic-chrome :host github :repo "KarimAziev/atomic-chrome")
  (:blackout (atomic-chrome-edit-mode . " Browser"))
  (:hook atomic-chrome-edit-mode-hook (lambda () (display-line-numbers-mode -1)))
  (:defer
    (:require t)
    (atomic-chrome-start-server)
    (:setopt atomic-chrome-extension-type-list '(atomic-chrome)
             atomic-chrome-buffer-open-style 'frame
             atomic-chrome-buffer-frame-width  84
             atomic-chrome-buffer-frame-height 30
             atomic-chrome-frame-parameters '((alpha-background . 95)
                                              (fullscreen . nil))
             atomic-chrome-auto-remove-file t
             atomic-chrome-default-major-mode 'markdown-mode)
    (:setopt atomic-chrome-url-major-mode-alist
             '(("github.com" . gfm-mode)
               ("us-east-2.console.aws.amazon.com" . yaml-ts-mode)
               ("ramdajs.com" . js-ts-mode)
               ("gitlab.com" . gfm-mode)
               ("leetcode.com" . typescript-ts-mode)
               ("typescriptlang.org" . typescript-ts-mode)
               ("jsfiddle.net" . js-ts-mode)
               ("w3schools.com" . js-ts-mode)))
    (:keymap atomic-chrome-edit-mode-map
      (:bind :state normal
        "Z Z" 'atomic-chrome-close-current-buffer)
      (:bind
        "C-c t s" '("Sync selection" . atomic-chrome-toggle-selection)
        [remap save-buffers-kill-terminal] 'atomic-chrome-close-current-buffer))))

;;; .
(provide 'helheim-browser)
;;; helheim-browser.el ends here
