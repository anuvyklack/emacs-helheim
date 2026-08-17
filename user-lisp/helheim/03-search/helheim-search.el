;;; helheim-search.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; Consult

(setup consult
  (:install t)
  (:setopt consult-line-start-from-top t
           consult-narrow-key "<"
           consult-fd-args `(,(if (executable-find "fdfind" 'remote) "fdfind" "fd")
                             "--color=never"
                             ;; https://github.com/sharkdp/fd/issues/839
                             "--full-path --absolute-path"
                             "--hidden --exclude .git")
           ;; Aggressive asynchronous that yield instantaneous results (suitable
           ;; for high-performance systems).
           ;; Note: The author of Consult, does not recommend aggressive values.
           ;; Read: https://github.com/minad/consult/discussions/951
           ;;
           ;; However, the author of minimal-emacs.d uses following parameters
           ;; to achieve immediate feedback from Consult.
           ;; consult-async-input-debounce 0.02
           ;; consult-async-input-throttle 0.05
           ;; consult-async-refresh-delay  0.02
           consult-async-input-throttle 0.2
           consult-async-input-debounce 0.1
           consult-async-refresh-delay  0.15)
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  (:after-load
    ;; Configure the register formatting and preview. This improves the register.
    (:setopt register-preview-delay 0.5
             register-preview-function #'consult-register-format)
    (advice-add 'register-preview :override #'consult-register-window)
    ;;
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark consult-recent-file
                       consult-source-bookmark
                       consult-source-file-register
                       consult-source-recent-file
                       consult-source-project-recent-file
                       ;; :preview-key "M-."
                       :preview-key '(:debounce 0.2 any))
    (consult-customize consult-theme
                       :preview-key '(:debounce 0.5 any))))

;;;; Keybindings

(setup consult
  (:global-bind :state (normal emacs)
    "C-/" 'consult-line        ; "/" is for search
    "C-?" 'consult-line-multi) ; "C-S-/"

  (:global-bind :state normal
    "g o" 'consult-imenu
    "g O" 'consult-imenu-multi
    "g p" 'consult-outline
    "g m" 'consult-mark
    "g M" 'consult-global-mark
    "g e" 'consult-compile-error
    "g <return>" 'consult-goto-line)

  (:keymap minibuffer-local-map
    (:bind "C-r" 'consult-history)) ; like in shell

  ;; <leader> s
  (:keymap search-map
    (:bind
      "f" 'consult-fd ;; or `consult-find'
      "l" 'consult-locate
      "g" 'consult-grep
      "v" 'consult-git-grep ; "v" for VC
      "/" 'consult-ripgrep
      "i" 'consult-info
      "k" 'consult-keep-lines
      "u" 'consult-focus-lines))

  ;; "C-x" bindings are in `ctl-x-map'
  (:global-bind
    "C-x b"   'consult-buffer
    "C-x 4 b" 'consult-buffer-other-window
    "C-x 5 b" 'consult-buffer-other-frame
    "C-x t b" 'consult-buffer-other-tab
    "C-x r b" 'consult-bookmark)

  (:global-bind
    [remap repeat-complex-command]        'consult-complex-command
    [remap recentf-open]                  'consult-recent-file
    [remap recentf-open-files]            'consult-recent-file
    [remap bookmark-jump]                 'consult-bookmark
    [remap goto-line]                     'consult-goto-line
    [remap imenu]                         'consult-imenu
    [remap Info-search]                   'consult-info
    [remap load-theme]                    'consult-theme
    [remap switch-to-buffer]              'consult-buffer
    [remap switch-to-buffer-other-window] 'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  'consult-buffer-other-frame
    [remap yank-pop]                      'consult-yank-pop
    [remap locate]                        'consult-locate))

(setup project
  (:after-load
    (:keymap project-prefix-map
      (:bind "b" 'consult-project-buffer)))) ;; <leader> p b

;;; Deadgrep

(setup deadgrep
  (:install t)
  (:keymap search-map
    (:bind "s" 'deadgrep)) ;; <leader> ss
  ;; (:hook deadgrep-mode-hook next-error-follow-minor-mode)
  )

;;; .
(provide 'helheim-search)
;;; helheim-search.el ends here
