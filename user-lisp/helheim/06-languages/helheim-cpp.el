;;; helheim-cpp.el -*- lexical-binding: t; no-byte-compile: t -*-

(setup c-ts-mode
  (add-to-list 'major-mode-remap-defaults '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-defaults '(c++-mode . c++-ts-mode))
  (:hook ( c-mode-hook c++-mode-hook
           c-ts-mode-hook c++-ts-mode-hook)
         helheim-lsp)
  (:setopt c-ts-mode-indent-offset 4
           c-ts-mode-indent-style 'helheim-c-ts-indent-style)
  (:after-load
    ;; Set line comment style.
    (c-ts-mode-toggle-comment-style -1)))

(setup cmake
  (when (< emacs-major-version 31)
    (:treesit cmake "https://github.com/uyha/tree-sitter-cmake")))

(setup lsp-mode
  (:after-load
    (:setopt lsp-clients-clangd-args '("--header-insertion-decorators=0"))))

;;;; Custom indentation rule
;;
;; - Write custom treesitter-based indentation rules.
;;   - https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
;;   - info:elisp#Parser-based Indentation
;;
;; - Use `treesit-explore-mode' to see where you're at in the treesitter tree.
;;
;; - Set `treesit--indent-verbose' to debug which rule is being applied at
;;   a given point.

;; (defun helheim-c-ts-indent-style ()
;;   `(;; align function arguments to the start of the first one, offset if standalone
;;     ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
;;     ((parent-is "argument_list") (nth-sibling 1) 0)
;;     ;; same for parameters
;;     ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
;;     ((parent-is "parameter_list") (nth-sibling 1) 0)
;;     ;; indent inside case blocks
;;     ((parent-is "case_statement") standalone-parent c-ts-mode-indent-offset)
;;     ;; do not indent preprocessor statements
;;     ((node-is "preproc") column-0 0)
;;     ;; do not add indentation level for namespace
;;     ((n-p-gp nil nil "namespace_definition") grand-parent 0)
;;     ;; append to bsd style
;;     ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

;;; .
(provide 'helheim-cpp)
;;; helheim-cpp.el ends here
