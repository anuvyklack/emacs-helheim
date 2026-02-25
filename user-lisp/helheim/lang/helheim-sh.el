;;; helheim-sh.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; (executable-find "bash-language-server")
;; (executable-find "shellcheck")
;; (executable-find "shfmt")

(leaf sh-script
  :hook ((sh-mode-hook bash-ts-mode-hook) . helheim-lsp)
  :mode (("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
         ("/bspwmrc\\'" . sh-mode))
  :magic ("#compdef " . sh-mode)
  :config
  (setf (alist-get 'sh-mode major-mode-remap-alist) 'bash-ts-mode))

(with-eval-after-load 'consult-imenu
  (setf (alist-get 'sh-mode consult-imenu-config)
        '( :toplevel "Function"
           :types ((?f "Function" font-lock-function-name-face)
                   (?v "Variable" font-lock-constant-face)))))

(leaf fish-mode :straight t)

;;; .
(provide 'helheim-sh)
;;; helheim-sh.el ends here
