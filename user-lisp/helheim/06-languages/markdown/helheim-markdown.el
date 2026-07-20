;;; helheim-markdown.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; markdown-mode

(setup markdown-mode
  (:install t)
  (:mode ("README\\.md\\'" . gfm-mode)) ; Github Flavored Markdown
  (:hook markdown-mode-hook +wrap-line-mode)
  (:setopt markdown-list-indent-width 2
           markdown-enable-highlighting-syntax t
           markdown-fontify-code-blocks-natively t
           markdown-gfm-additional-languages '("sh")
           markdown-gfm-uppercase-checkbox t
           ;; Command to convert markdown to HTML
           markdown-command '("pandoc" "--from=markdown" "--to=html5")
           markdown-open-command (pcase system-type
                                   ('gnu/linux "xdg-open")
                                   ('darwin "open")))
  ;; A sensible and simple default preamble for markdown exports that
  ;; takes after the github asthetic (plus highlightjs syntax coloring).
  (:setopt markdown-content-type "application/xhtml+xml")
  (:setopt markdown-css-paths
           '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
             "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (:setopt markdown-xhtml-header-content
           (concat
            "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
            "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
            "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
            "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
            "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))
  ;;
  (:hook markdown-mode-hook
         (defun helheim-markdown-mode-h ()
           (setq-local tab-width 2
                       visual-fill-column-width (+ fill-column 10)
                       ;; Don't trigger autofill in code blocks (see `auto-fill-mode')
                       fill-nobreak-predicate (cons #'markdown-code-block-at-point-p
                                                    fill-nobreak-predicate))
           (setf (alist-get ?` hel-surround-alist)
                 '(:insert (lambda ()
                             ;; If selection is linewise enclose in tripple
                             ;; backticks, otherwise -- in sinlge one.
                             (if (hel-linewise-selection-p)
                                 '("```\n" . "\n```")
                               '("`" . "`"))))))))

(define-advice markdown-match-generic-metadata (:override (&rest  _)
                                                disable-metadata-fontification)
  "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a Markdown
document has a colon in it, then it's distractingly and usually wrongly
fontified as a metadata block.
See https://github.com/jrblevin/markdown-mode/issues/328."
  (ignore (goto-char (point-max))))

;;;; keybindings

(setup markdown-mode
  (:after-load
    (:keymap markdown-mode-map
      (:bind :state normal
        ;; "<tab>"    'markdown-cycle
        ;; "<backtab>" 'markdown-shifttab
        ;; "RET"      'markdown-do
        ;; "{"        'markdown-backward-paragraph
        ;; "}"        'markdown-forward-paragraph
        "m h"      'markdown-mark-subtree
        "m i h"    'markdown-mark-subtree
        "z '"      'markdown-edit-code-block
        "z u"      'markdown-outline-up
        "z j"      'markdown-outline-next
        "z k"      'markdown-outline-previous
        "C-k"      'markdown-outline-previous-same-level
        "C-j"      'markdown-outline-next-same-level
        "C-<up>"   'markdown-outline-previous-same-level
        "C-<down>" 'markdown-outline-next-same-level
        ;; "<return>" 'markdown-toggle-markup-hiding
        "M-<up>"   'markdown-move-up
        "M-<down>" 'markdown-move-down
        "z ,"      'markdown-insert-gfm-code-block)
      (:bind
        ;; toggle
        "C-c t i"  '("show/hide images" . markdown-toggle-inline-images)
        "C-c t l"  '("show/hide links" . markdown-toggle-url-hiding)
        "C-c t m"  '("show/hide markup" . markdown-toggle-markup-hiding)
        "C-c t M"  '("show/hide LaTeX math" . markdown-toggle-math)))))

;;; markdown-ts-mode

(setup markdown-ts-mode ;; 31+ only
  (:when (and (<= 31 emacs-major-version)
              (treesit-available-p)))
  ;; Tree-sitter
  (add-to-list 'major-mode-remap-defaults '(markdown-mode . markdown-ts-mode))
  (add-to-list 'major-mode-remap-defaults '(gfm-mode . markdown-ts-mode))
  (:treesit markdown
    "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
    :revision (if (< (treesit-library-abi-version) 15) "v0.4.1") ;; "v0.5.3"
    :source-dir "tree-sitter-markdown/src")
  (:treesit markdown-inline
    "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
    :revision (if (< (treesit-library-abi-version) 15) "v0.4.1") ;; "v0.5.3"
    :source-dir "tree-sitter-markdown-inline/src")
  ;;
  (:command markdown-ts-mode)
  (:hook markdown-ts-mode-hook +wrap-line-mode)
  (:hook markdown-ts-mode-hook
         (defun helheim-markdown-ts-mode-h ()
           (setq-local tab-width 2
                       visual-fill-column-width (+ fill-column 10))
           (setf (alist-get ?` hel-surround-alist)
                 '(:insert (lambda ()
                             ;; If selection is linewise enclose in tripple
                             ;; backticks, otherwise -- in sinlge one.
                             (if (hel-linewise-selection-p)
                                 '("```\n" . "\n```")
                               '("`" . "`"))))))))

;;;; keybindings

(setup markdown-ts-mode
  (:after-load
    (:keymap markdown-ts-mode-map
      (:bind :state normal
        "C-j"       'outline-next-heading
        "C-k"       'outline-previous-heading
        "z u"       'outline-up-heading
        "z C-j"     'outline-forward-same-level
        "z C-k"     'outline-backward-same-level
        ;;
        "] c"       'markdown-ts-move-to-next-code-block
        "[ c"       'markdown-ts-move-to-previous-code-block
        ", ,"       'markdown-ts-insert-structure
        ", r"       'markdown-ts-renumber-list
        ", f"       'markdown-ts-emphasize
        ", t"       'markdown-ts-table-insert-table)
      (:bind
        "C-c C-c"   'markdown-ts-toggle-checkbox
        "C-c t i"   'markdown-ts-toggle-inline-images
        "C-c t m"   'markdown-ts-toggle-hide-markup
        "C-c t RET" 'markdown-ts-toggle-hide-markup
        "M-RET"     'markdown-ts-insert-list-item))

    (:keymap markdown-ts-view-mode-map
      (:bind :state (normal emacs)
        "C-j"       'outline-next-heading
        "C-k"       'outline-previous-heading
        "z u"       'outline-up-heading
        "z C-j"     'outline-forward-same-level
        "z C-k"     'outline-backward-same-level
        ;;
        "] c"       'markdown-ts-move-to-next-code-block
        "[ c"       'markdown-ts-move-to-previous-code-block)
      (:bind
        "C-c t i"   'markdown-ts-toggle-inline-images
        "C-c t m"   'markdown-ts-toggle-hide-markup
        "C-c t RET" 'markdown-ts-toggle-hide-markup))

    (:keymap markdown-ts-code-block-in-context-mode-map
      (:bind :state normal
        "g d"       'markdown-ts--code-block-xref-find-definitions
        "g q"       'markdown-ts--code-block-fill-paragraph))

    (:keymap markdown-ts-in-table-mode-map
      (:bind :state normal
        ", t a"     'markdown-ts-table-align-column
        ", t t"     'markdown-ts-table-transpose-table
        ", t <"     'markdown-ts-table-clone-column-left
        ", t >"     'markdown-ts-table-clone-column-right
        ;; 'markdown-ts-table-delete-column
        ))))

;;; .
(provide 'helheim-markdown)
;;; helheim-markdown.el ends here
