;;; helheim-org-node.el           -*- lexical-binding: t; no-byte-compile: t -*-
;;; Customization

(defcustom helheim-org-node-visit-backlink-in-another-window nil
  "When non-nil \"RET\" in backlinks buffer opens target in another window."
  :group 'helheim
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (advice-add 'org-node-context-visit-thing :around
                         'helheim-open-in-another-window-a)
           (advice-remove 'org-node-context-visit-thing
                          'helheim-open-in-another-window-a))))

;;; Keybindings

(setup org
  ;; Keys available everywhere
  (:global-bind
    "C-c n n"  '("find or create node" . org-node-find)
    "C-c n r"  '("open random node" . org-node-visit-random)
    "C-c n /"  '("search in all nodes" . org-node-grep)
    ;; "C-c n s"  'org-node-seq-dispatch
    "C-c n x"  (cons "maintenance"
                     (define-keymap
                       "t" 'org-mem-list-title-collisions ; "t" for title
                       "a" 'org-node-rename-asset-and-rewrite-links
                       "p" 'org-mem-list-problems
                       "d" 'org-mem-list-dead-id-links
                       "e" 'org-node-list-example
                       "f" 'org-node-list-files
                       "l" 'org-node-lint-all-files
                       "r" 'org-node-list-reflinks
                       "x" '("reset org-mem cache" . org-mem-reset))))
  ;; Keys active in Org-mode
  (with-eval-after-load 'org-keys
    (require 'helheim-org-keys)
    (:keymap org-mode-map
      (:bind
        ;; notes
        "C-c n b"  '("backlinks buffer" . helheim-org-node-backlinks-buffer)
        "C-c n i"  '("add ID" . org-node-nodeify-entry)
        "C-c n I"  '("add ID and ignore" . helheimg-org-node-create-ignored-node)
        "C-c n w"  '("refile node" . org-node-refile) ;; "C-c C-w" is `org-refile'
        ;; "C-c n I"  'org-node-insert-include ;; TODO. Not yet a good command.
        ))
    ;; "C-c i" or ",i"
    (:keymap helheim-org-insert-map
      (:bind
        "a"  '("add alias" . org-node-add-alias)
        "i"  '("add ID" . org-node-nodeify-entry) ; "ii" - insert ID
        "I"  '("add ID and ignore" . helheimg-org-node-create-ignored-node)
        "q"  '("add tags" . org-node-set-tags-here))) ;; or `org-node-add-tags'
    ;; "C-c l" or ",l"
    (:keymap helheim-org-link-map
      (:bind
        "n"  '("insert link to node" . org-node-insert-link)
        "t"  '("node transclusion" . org-node-insert-transclusion)
        "T"  '("node transclusion as subtree" . org-node-insert-transclusion-as-subtree)))))

;;; Config

(setup org-mem
  (:install t)
  (:defer (:require t))
  (:setopt org-mem-do-sync-with-org-id t
           org-mem-watch-dirs (list org-directory)))

(setup org-node
  (:install t)
  (:defer (:require t))
  (:after-load
    (load "helheim-org-node-lib" nil t)
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
    (add-hook 'org-mem-post-full-scan-functions #'helheim-set-agenda-files)
    (org-mem-updater-mode)
    (org-node-cache-mode)))

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
  (:after-init org-node-backlink-mode)
  ;; Display backlinks buffer in another window.
  (add-to-list 'display-buffer-alist
               '((major-mode . org-node-context-mode)
                 (display-buffer-reuse-mode-window
                  +display-buffer-based-on-window-count
                  display-buffer-use-some-window)
                 (inhibit-same-window . t)
                 (body-function . select-window)))
  ;; BUG: https://github.com/meedstrom/org-node/issues/180
  ;; TODO: remove when will be fixed
  (advice-add 'org-node-backlink--fix-nearby-drawer :around
              'helheim-org-node-inhibit-auto-fill-a)
  (advice-add 'org-node-backlink--add-to-drawer :around
              'helheim-org-node-inhibit-auto-fill-a))

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
