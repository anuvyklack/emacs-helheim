;;; helheim-markdown.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Keybindings

(with-eval-after-load 'markdown-mode
  (hel-keymap-set markdown-mode-map :state 'normal
    ;; "<tab>"     'markdown-cycle
    ;; "<backtab>" 'markdown-shifttab
    ;; "RET"   'markdown-do
    ;; "{"     'markdown-backward-paragraph
    ;; "}"     'markdown-forward-paragraph
    "m h"   'markdown-mark-subtree
    "m i h" 'markdown-mark-subtree
    "z '"   'markdown-edit-code-block
    "z u"   'markdown-outline-up
    "z j"   'markdown-outline-next
    "z k"   'markdown-outline-previous
    "C-k"   'markdown-outline-previous-same-level
    "C-j"   'markdown-outline-next-same-level
    "C-<up>"   'markdown-outline-previous-same-level
    "C-<down>" 'markdown-outline-next-same-level
    ;; "<return>" 'markdown-toggle-markup-hiding
    "M-<up>"   'markdown-move-up
    "M-<down>" 'markdown-move-down
    "z ,"   'markdown-insert-gfm-code-block))

;;; Config

;; Required by `markdown-mode', or it will install it via package.el
;; if it isn't present when you call `markdown-edit-code-block'.
(use-package edit-indirect
  :ensure t
  :defer t
  :config
  (hel-keymap-set edit-indirect-mode-map :state 'normal
    "Z Z" 'edit-indirect-commit
    "Z Q" 'edit-indirect-abort))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . gfm-mode) ;; Github Flavored Markdown
  :custom
  ;; Command to convert plain text to HTML
  (markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  (markdown-list-indent-width 2)
  (markdown-enable-highlighting-syntax t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-gfm-uppercase-checkbox t)
  :hook
  (markdown-mode-hook . helheim-markdown-mode-h)
  (markdown-mode-hook . +word-wrap-mode)
  :config
  (setq markdown-open-command (pcase system-type
                                ('gnu/linux "xdg-open")
                                ('darwin "open")))
  ;; A sensible and simple default preamble for markdown exports that
  ;; takes after the github asthetic (plus highlightjs syntax coloring).
  (setq markdown-content-type "application/xhtml+xml"
        markdown-css-paths
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content
        (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>")))

(defun helheim-markdown-mode-h ()
  (setq tab-width 2
        visual-fill-column-width (+ fill-column 10)))

;;; .
(provide 'helheim-markdown)
;;; helheim-markdown.el ends here
