;;; helheim-sh.el                 -*- lexical-binding: t; no-byte-compile: t -*-

;; (executable-find "bash-language-server")
;; (executable-find "shellcheck")
;; (executable-find "shfmt")

(setup sh-script
  (:hook sh-base-mode-hook helheim-lsp)
  (add-to-list 'auto-mode-alist  '("\\.\\(?:zunit\\|env\\)\\'" . sh-mode))
  (add-to-list 'auto-mode-alist  '("/bspwmrc\\'" . sh-mode))
  (add-to-list 'magic-mode-alist '("#compdef " . sh-mode)))

(setup bash-ts-mode
  (:when (treesit-available-p))
  (add-to-list 'major-mode-remap-defaults '(sh-mode . bash-ts-mode))
  (:treesit bash "https://github.com/tree-sitter/tree-sitter-bash"
    ;; :revision "v0.23.3"
    ))

(with-eval-after-load 'consult-imenu
  (setf (alist-get 'sh-mode consult-imenu-config)
        '( :toplevel "Function"
           :types ((?f "Function" font-lock-function-name-face)
                   (?v "Variable" font-lock-constant-face)))))

;;; .
(provide 'helheim-sh)
;;; helheim-sh.el ends here
