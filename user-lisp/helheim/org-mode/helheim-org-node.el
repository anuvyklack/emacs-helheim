;;; helheim-org-node.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(require 'hel-core)

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
              "a" 'org-agenda
              "n" 'org-node-find
              "r" 'org-node-visit-random
              "/" 'org-node-grep
              "c" 'org-capture
              ;; "C" 'org-capture-goto-target ; TODO: autoload
              ;; "j" (cons "journal"
              ;;           (define-keymap
              ;;             "j" 'org-journal-new-entry
              ;;             "J" 'org-journal-new-scheduled-entry
              ;;             "s" 'org-journal-search-forever))
              "S" 'org-store-link
              "t" 'org-todo-list
              ;; "s" 'org-node-seq-dispatch
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
  (hel-keymap-set (keymap-lookup org-mode-map "C-c")
    ;; "insert"
    "i i" '("add ID" . org-node-nodeify-entry) ; "ii" - insert ID
    "i I" '("add ID and ignore" . helheimg-org-node-create-ignored-node)
    "i n" '("insert link to node" . org-node-insert-link) ; insert node
    "i a" 'org-node-add-alias
    ;; Because "C-c C-q" is `org-set-tags-command'
    "i q" 'org-node-add-tags-here ;; or `org-node-add-tags'
    ;; "i q" 'org-node-set-tags ; or `org-node-set-tags'
    ;;
    ;; "links"
    "l t" 'org-node-insert-transclusion
    "l y" 'org-node-insert-transclusion-as-subtree
    ;;
    ;; "notes"
    "n b" '("backlinks buffer" . helheim-org-node-backlinks-buffer)
    "n i" '("add ID" . org-node-nodeify-entry)
    "n I" '("add ID and ignore" . helheimg-org-node-create-ignored-node)
    "n l" '("insert link to node" . org-node-insert-link)
    ;; "n I" 'org-node-insert-include ;; TODO. Not yet a good command.
    "n w" 'org-node-refile)) ;; because "C-c C-w" is `org-refile'

;;; Code

(setup org-mem
  (:install t)
  (setopt org-mem-do-sync-with-org-id t)
  (:after-init org-mem-updater-mode)
  (:require t)
  (unless org-mem-watch-dirs
    (setq org-mem-watch-dirs (list org-directory))))

(setup org-node
  (:install t)
  (:require t)
  (:after-init org-node-cache-mode)
  (setopt org-node-prefer-with-heading t
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

(defun helheim-org-node-append-tags (node title)
  "Append NODE\\='s tags to TITLE."
  (list title
        ""
        (if-let* ((tags (org-mem-entry-tags node)))
            (propertize (concat "   :" (string-join tags ":") ":")
                        'face 'org-node-tag)
          "")))

(defun helheim-org-node-filter-p (node)
  "Hide NODE if it has or inherits an :IGNORE: or :ROAM_EXCLUDE: properties."
  (not (or (org-mem-property-with-inheritance "IGNORE" node)
           (org-mem-property-with-inheritance "ROAM_EXCLUDE" node))))

;; (setup org-node-seq
;;   (:after org-node)
;;   (:after-init org-node-seq-mode)
;;   (:require t)
;;   (setopt org-node-seq-defs
;;           (list
;;            ;; My day-notes, a.k.a. journal/diary.  Currently I still
;;            ;; structure them like org-roam-dailies expects: confined to a
;;            ;; subdirectory, with filenames such as "2024-11-18.org".
;;            ;; This is actually a sequence of files, not sequence of ID-nodes.
;;            (org-node-seq-def-on-filepath-sort-by-basename
;;             "d" "Dailies" helheimg-org-daily-directory))))

;;;; Backlinks drawers

(setup org-node-backlink
  (setopt org-node-backlink-do-drawers t
          org-node-backlink-drawer-formatter 'helheim-org-node-backlink-format)
  (:after-init org-node-backlink-mode))

(defun helheim-org-node-backlink-format (id desc &optional _time)
  "Format as list item: \"- [[id:ID][Node title]]\".
ID and DESC are link id and description, TIME a Lisp time value."
  (concat "- " (org-link-make-string (concat "id:" id)
                                     (org-link-display-format desc))))

;;;; Backlinks buffer

(setup org-node-context
  (:after org-node)
  (setopt org-node-context-collapse-more-than 1) ;; Start in collapsed state.
  (add-hook 'org-node-context-postprocess-hook
            'helheim-org-node-context--add-empty-line-at-eob
            95))

(defun helheim-org-node-context--add-empty-line-at-eob ()
  "Add empty line at the end of a section to separate it from the following one."
  (goto-char (point-max))
  (insert "\n"))

(defun helheim-org-node-backlinks-buffer ()
  "Show backlinks buffer for the node at point.
Org-node native command is `org-node-context-dwim'."
  (interactive)
  (require 'org-node-context)
  (when (derived-mode-p 'org-mode)
    (let ((buffer (get-buffer-create org-node-context-main-buffer)))
      (org-node-context--refresh buffer (org-entry-get-with-inheritance "ID"))
      (progn (set-buffer buffer)
             (goto-char (point-min)))
      (display-buffer buffer))))

(defun helheim-open-in-another-window-a (orig-fun &rest args)
  "Open backlinks buffer in another window."
  ;; Set `display-buffer-overriding-action' only if it wasn't set before
  ;; us by `same-window-prefix' or `other-window-prefix' or any other.
  (if (equal display-buffer-overriding-action '(nil . nil))
      (let ((display-buffer-overriding-action
             '(nil
               (inhibit-same-window . t))))
        (apply orig-fun args))
    ;; else
    (apply orig-fun args)))

;;;; Commands

(defun helheimg-org-node-create-ignored-node ()
  "Add ID to node, and say Org-node to ignore it."
  (interactive)
  (call-interactively 'org-node-nodeify-entry)
  (org-set-property "ROAM_EXCLUDE" "t"))

;;; .
(provide 'helheim-org-node)
;;; helheim-org-node.el ends here
