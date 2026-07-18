;;; helheim-lua.el -*- lexical-binding: t; no-byte-compile: t -*-

(setup lua-mode
  (add-to-list 'interpreter-mode-alist
               '("\\<lua\\(?:jit\\)?" . lua-mode))
  (:setopt lua-indent-level 3)) ;; the length of "end" keyword

(setup lua-ts-mode
  (:when (treesit-available-p))
  (:treesit lua "https://github.com/tree-sitter-grammars/tree-sitter-lua"
    :revision (if (< (treesit-library-abi-version) 15)
                  "v0.3.0"))
  (add-to-list 'major-mode-remap-defaults '(lua-mode . lua-ts-mode))
  (:setopt lua-ts-indent-offset 3
           lua-ts-auto-close-block-comments t))

;;; .
(provide 'helheim-lua)
;;; helheim-lua.el ends here
