;;; helheim-sh.el                 -*- lexical-binding: t; no-byte-compile: t -*-

;; (executable-find "bash-language-server")
;; (executable-find "shellcheck")
;; (executable-find "shfmt")

(setup sh-script
  (:hook sh-base-mode-hook helheim-lsp)
  (:mode ("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
         ("/bspwmrc\\'" . sh-mode))
  (add-to-list 'magic-mode-alist '("#compdef " . sh-mode))
  (setf (alist-get 'sh-mode major-mode-remap-alist) 'bash-ts-mode))

(with-eval-after-load 'consult-imenu
  (setf (alist-get 'sh-mode consult-imenu-config)
        '( :toplevel "Function"
           :types ((?f "Function" font-lock-function-name-face)
                   (?v "Variable" font-lock-constant-face)))))

(setup fish-mode (:install t))

;;; .
(provide 'helheim-sh)
;;; helheim-sh.el ends here
