;;; helheim-json.el               -*- lexical-binding: t; no-byte-compile: t -*-

(setup json-mode
  (:install t)
  (:mode ("\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'" . json-mode))
  (:hook json-mode-local-vars-hook helheim-lsp)
  (:after-load
    (:keymap json-mode-map
      (:bind :state normal
        "," (define-keymap
              "p" '("Copy path" . json-mode-show-path)
              "t" 'json-toggle-boolean
              "d" 'json-mode-kill-path
              "x" 'json-nullify-sexp
              "+" 'json-increment-number-at-point
              "-" 'json-decrement-number-at-point
              "f" 'json-mode-beautify)))))

(setup json-ts-mode
  (:when (treesit-available-p))
  (:treesit json "https://github.com/tree-sitter/tree-sitter-json")
  (:hook json-ts-mode-local-vars-hook helheim-lsp)
  (add-to-list 'major-mode-remap-defaults '(json-mode . json-ts-mode)))

;;; .
(provide 'helheim-json)
;;; helheim-json.el ends here
