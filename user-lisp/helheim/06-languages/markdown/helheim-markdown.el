;;; helheim-markdown.el           -*- lexical-binding: t; no-byte-compile: t -*-
;;; Keybindings

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

;;; Config

(setup markdown-mode
  (:install t)
  (:mode ("README\\.md\\'" . gfm-mode)) ; Github Flavored Markdown
  (:hook markdown-mode-hook (helheim-markdown-mode-h
                             +wrap-line-mode))
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
  (:setopt markdown-content-type "application/xhtml+xml"
           markdown-css-paths
           '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
             "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
           markdown-xhtml-header-content
           (concat
            "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
            "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
            "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
            "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
            "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>")))

(defun helheim-markdown-mode-h ()
  (setq tab-width 2
        visual-fill-column-width (+ fill-column 10))
  (unless (assq ?` hel-surround-alist)
    (push '(?` :insert (lambda ()
                         ;; If selection is linewise enclose it in tripple
                         ;; backticks, otherwise -- in sinlge one.
                         (if (hel-linewise-selection-p)
                             '("```\n" . "\n```")
                           '("`" . "`"))))
          hel-surround-alist)))

;;; .
(provide 'helheim-markdown)
;;; helheim-markdown.el ends here
