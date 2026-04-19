;;; helheim-org-node.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; Customization

(defcustom helheimg-org-node-visit-backlink-in-another-window nil
  "When non-nil \"RET\" in backlinks buffer opens target in another window."
  :group 'helheim
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (advice-add 'org-node-context-visit-thing :around
                         'helheim-open-in-another-window-a)
           ;; else
           (advice-remove 'org-node-context-visit-thing
                          'helheim-open-in-another-window-a))))

;;; Keybindings

;; Keys that available from anywhere.
(hel-keymap-set mode-specific-map
  "n" (cons "notes"
            (define-keymap
              "a"   'org-agenda
              "n"   'org-node-find
              "r"   'org-node-visit-random
              "/"   'org-node-grep
              "c"   '("Capture" . org-capture)
              ;; "C"   'org-capture-goto-target ; TODO: autoload
              ;; "j"   (cons "journal"
              ;;             (define-keymap
              ;;               "j" 'org-journal-new-entry
              ;;               "J" 'org-journal-new-scheduled-entry
              ;;               "s" 'org-journal-search-forever))
              "S"   'org-store-link
              "t"   'org-todo-list
              ;; "s"   'org-node-seq-dispatch
              "x t" 'org-mem-list-title-collisions ; "t" for title
              "x a" 'org-node-rename-asset-and-rewrite-links
              "x p" 'org-mem-list-problems
              "x d" 'org-mem-list-dead-id-links
              "x e" 'org-node-list-example
              "x f" 'org-node-list-files
              "x l" 'org-node-lint-all-files
              "x r" 'org-node-list-reflinks
              "x x" 'org-mem-reset
              "X"   'org-mem-reset)))

;; Keys that available in Org mode.
(with-eval-after-load 'org-keys
  (hel-keymap-set (helheim-leader-map org-mode-map)
    ;; "insert"
    "i i"  '("add ID" . org-node-nodeify-entry) ; "ii" - insert ID
    "i I"  '("add ID and ignore" . helheimg-org-node-create-ignored-node)
    "i n"  '("insert link to node" . org-node-insert-link) ; insert node
    "i a"  'org-node-add-alias
    ;; Because "C-c C-q" is `org-set-tags-command'
    "i q"  'org-node-add-tags-here ;; or `org-node-add-tags'
    ;; "i q"  'org-node-set-tags ; or `org-node-set-tags'
    ;;
    ;; "links"
    "l t"  'org-node-insert-transclusion
    "l y"  'org-node-insert-transclusion-as-subtree
    ;;
    ;; "notes"
    "n b"  '("backlinks buffer" . helheim-org-node-backlinks-buffer)
    "n i"  '("add ID" . org-node-nodeify-entry)
    "n I"  '("add ID and ignore" . helheimg-org-node-create-ignored-node)
    "n l"  '("insert link to node" . org-node-insert-link)
    ;; "n I"  'org-node-insert-include ;; TODO. Not yet a good command.
    "n w"  'org-node-refile)) ;; because "C-c C-w" is `org-refile'

;;; Config

(setup org-mem
  (:install t)
  (:setopt org-mem-do-sync-with-org-id t)
  (:after-init org-mem-updater-mode)
  (:require t)
  (unless org-mem-watch-dirs
    (setq org-mem-watch-dirs (list org-directory))))

(setup org-node
  (:install t)
  (:require t)
  (load "helheim-org-node-lib" nil t)
  (:after-init org-node-cache-mode)
  (:setopt org-node-prefer-with-heading t
           org-node-creation-fn #'org-node-new-file
           org-node-file-slug-fn #'org-node-slugify-for-web
           org-node-file-timestamp-format (concat helheim-id-format "--") ;; Denote format
           org-node-blank-input-hint nil
           org-node-alter-candidates t
           org-node-affixation-fn 'helheim-org-node-append-tags
           org-node-filter-fn 'helheim-org-node-filter-p)
  ;; We have this information in ID.
  (remove-hook 'org-node-creation-hook #'org-node-ensure-crtime-property)
  ;; Open backlinks buffer in another window.
  (add-to-list 'display-buffer-alist
               '((major-mode . org-node-context-mode)
                 (display-buffer-use-some-window display-buffer-pop-up-window)
                 (inhibit-same-window . t)
                 (body-function . select-window)))
  ;; (set-face-attribute org-node-context-origin-title nil
  ;;                     :inherit 'magit-section-secondary-heading)
  )

;; (setup org-node-seq
;;   (:after org-node)
;;   (:after-init org-node-seq-mode)
;;   (:require t)
;;   (:setopt org-node-seq-defs
;;            (list
;;             ;; My day-notes, a.k.a. journal/diary.  Currently I still
;;             ;; structure them like org-roam-dailies expects: confined to a
;;             ;; subdirectory, with filenames such as "2024-11-18.org".
;;             ;; This is actually a sequence of files, not sequence of ID-nodes.
;;             (org-node-seq-def-on-filepath-sort-by-basename
;;              "d" "Dailies" helheimg-org-daily-directory))))

;;;; Backlinks drawers

(setup org-node-backlink
  (:setopt org-node-backlink-do-drawers t
           org-node-backlink-drawer-formatter 'helheim-org-node-backlink-format)
  (:after-init org-node-backlink-mode))

;;;; Backlinks buffer

(setup org-node-context
  (:after org-node)
  (:setopt org-node-context-collapse-more-than 1) ;; Start in collapsed state.
  (add-hook 'org-node-context-postprocess-hook
            'helheim-org-node-context--add-empty-line-at-eob
            95))

;;; .
(provide 'helheim-org-node)
;;; helheim-org-node.el ends here
