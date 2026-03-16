;;; helheim-magit-keys.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'hel-core)
(require 'magit)

;;; Common keys

;; `magit-mode-map' is the common parent keymap for all other Magit keymaps.
(hel-keymap-set magit-mode-map
  :unset '("g" "z")
  "C-<return>" 'magit-visit-thing
  "M-<return>" 'magit-visit-thing
  "RET"        'magit-visit-thing
  "M-TAB"      'magit-dired-jump
  "M-<tab>"    'magit-section-cycle-diffs
  "+"          'magit-diff-more-context
  "-"          'magit-diff-less-context
  "0"          'magit-diff-default-context
  ;; Use the default Hel motion commands instead of `magit-next-line'
  ;; and `magit-previous-line', because they are surprisingly slow and
  ;; make little sense, since we have toggle selection on "v".
  "j"   'next-line
  "k"   'previous-line
  "g g" 'beginning-of-buffer
  "G"   'end-of-buffer
  ;;
  "a"   'magit-cherry-apply
  "A"   'magit-cherry-pick
  "b"   'magit-branch
  "B"   'magit-bisect
  "c"   'magit-commit
  "C"   'magit-clone
  "d"   'magit-delete-thing
  "D"   'magit-file-untrack
  ", d" 'magit-diff
  ", D" 'magit-diff-refresh
  "e"   'magit-ediff-dwim
  "E"   'magit-ediff
  "f"   'magit-fetch
  "F"   'magit-pull
  "g r" 'magit-refresh-all
  "h"   'magit-dispatch
  "H"   'magit-describe-section
  ;; "i"   'helheim-magit-text-mode
  "i"   'magit-gitignore
  ", i" 'magit-gitignore
  "I"   'magit-init
  "l"   'magit-log
  "L"   'magit-log-refresh
  "m"   'magit-merge
  "M"   'magit-remote
  "n"   'magit-show-refs
  "N"   'magit-cherry
  "o"   'magit-submodule
  "O"   'magit-subtree
  "p"   'magit-push
  "q"   'magit-mode-bury-buffer
  "Q"   'magit-git-command
  "r"   'magit-rebase
  "R"   'magit-file-rename
  "s"   'magit-stage-files
  "S"   'magit-stage-modified
  "t"   'magit-tag
  "T"   'magit-notes
  "u"   'magit-unstage-files
  "U"   'magit-unstage-all
  "v"   'helheim-magit-toggle-selection
  "<escape>" (lambda () (interactive) (deactivate-mark))
  ", v" 'magit-revert-no-commit
  ", V" 'magit-revert
  "w"   'magit-am
  "W"   'magit-patch
  "x"   'magit-reset-quickly
  "X"   'magit-reset
  "y"   'magit-copy-section-value
  "Y"   'magit-copy-buffer-revision
  ", z" 'magit-worktree
  "Z"   'magit-stash
  "!"   'magit-run
  ">"   'magit-sparse-checkout
  ";"   'magit-git-command
  "?"   'magit-dispatch
  "$"   'magit-process-buffer
  "%"   'magit-worktree
  ;; "/"   'magit-status-quick
  ", ," 'magit-display-repository-buffer ;; counterpart to "SPC ,"
  ", ?" 'magit-describe-section
  ", e" 'magit-edit-thing
  ", o" 'magit-browse-thing
  ", y" 'magit-copy-thing ;; it seams it does nothing currently
  "g n" 'magit-next-reference
  "g p" 'magit-previous-reference
  "g N" 'magit-previous-reference
  "C-c C-c" 'magit-dispatch
  "C-c SPC" 'magit-dispatch) ;; <leader><leader>

;; Repeat keymap
(hel-keymap-set magit-reference-navigation-repeat-map
  :unset "r"
  "n" 'magit-next-reference
  "p" 'magit-previous-reference
  "N" 'magit-previous-reference)

;; (hel-keymap-set magit-mode-map :state '(motion normal)
;;   "C-w p" 'magit-toggle-buffer-lock)

;;; Magit status buffer

(hel-keymap-set magit-status-mode-map
  :unset "j"
  "/" 'magit-status-jump) ; "j"

;;; Magit diff

(hel-keymap-set magit-diff-mode-map
  :unset "j"
  "C-o"  'magit-go-backward
  "C-i"  'magit-go-forward
  "g d"  'magit-jump-to-diffstat-or-diff ; "j"
  "<remap> <write-file>" 'magit-patch-save)

;; `magit-file-section-map' and `magit-hunk-section-map' keymaps are
;; inherited from `magit-diff-section-map'.
(hel-keymap-set magit-diff-section-map
  :unset "C-j"
  "C-<return>" 'magit-diff-visit-worktree-file
  "M-<return>" 'magit-diff-visit-worktree-file
  ", t" 'magit-diff-trace-definition
  ", e" 'magit-diff-edit-hunk-commit)

;;; Revision buffer

(hel-keymap-set magit-revision-mode-map
  :unset "j"
  "/" 'magit-revision-jump)

;;; Magit log

(hel-keymap-set magit-log-mode-map
  :unset "j"
  "C-j" 'magit-log-move-to-parent
  "C-o" 'magit-go-backward
  "C-i" 'magit-go-forward
  "/"   'magit-log-move-to-revision
  "="   'magit-log-toggle-commit-limit
  "+"   'magit-log-double-commit-limit
  "-"   'magit-log-half-commit-limit
  "q"   'magit-log-bury-buffer)

(hel-keymap-set magit-log-select-mode-map
  "Z Z" #'magit-log-select-quit
  "Z Q" #'magit-log-select-quit)
(put 'magit-log-select-quit :advertised-binding [?Z ?Q])

;; magit-reflog-mode-map
;; magit-stashes-mode-map

;;; Blame

(add-hook 'magit-blame-mode-hook (defun helheim-magit-blame-h ()
                                   (if magit-blame-mode
                                       (hel-motion-state)
                                     (hel-normal-state))))

(hel-keymap-set magit-blame-read-only-mode-map
  :unset '("SPC" "S-SPC" "DEL" "C-m")
  ;; "RET" 'magit-diff-show-or-scroll-up
  "RET" 'magit-show-commit
  "q"   'magit-blame-quit
  ;;
  "j"   'magit-blame-next-chunk
  "k"   'magit-blame-previous-chunk
  "C-j" 'magit-blame-next-chunk-same-commit
  "C-k" 'magit-blame-previous-chunk-same-commit
  ;;
  "p"   'magit-blame-previous-chunk
  "P"   'magit-blame-previous-chunk-same-commit
  "n"   'magit-blame-next-chunk
  "N"   'magit-blame-next-chunk-same-commit
  ;;
  "b"   'magit-blame-addition
  "r"   'magit-blame-removal
  "f"   'magit-blame-reverse
  "B"   'magit-blame
  "v"   '("Cycle view" . magit-blame-cycle-style)
  "y"   '("Copy hash" . magit-blame-copy-hash)
  ", b" 'magit-blame-addition
  ", r" 'magit-blame-removal
  ", f" 'magit-blame-reverse
  ", B" 'magit-blame
  ", v" '("Cycle view" . magit-blame-cycle-style)
  ", y" '("Copy hash" . magit-blame-copy-hash))

;;; Git commit

(hel-keymap-set git-commit-mode-map :state 'normal
  "g j" 'git-commit-next-message ;; "M-n"
  "g k" 'git-commit-prev-message ;; "M-p"
  ;; "z j" 'git-commit-next-message
  ;; "z k" 'git-commit-prev-message
  ", j" 'git-commit-search-message-forward
  ", k" 'git-commit-search-message-backward
  ", i" 'git-commit-insert-trailer
  ", s" 'git-commit-save-message
  ", d" 'magit-diff-while-committing
  ", p" 'magit-pop-revision-stack)

;;; Git rebase

(with-eval-after-load 'git-rebase
  (hel-keymap-set git-rebase-mode-map
    :unset '("C-m" "SPC" "DEL")
    "RET" 'git-rebase-show-commit
    "j"   'forward-line
    "k"   'git-rebase-backward-line
    "M-j" 'git-rebase-move-line-down
    "M-k" 'git-rebase-move-line-up
    "p"   'git-rebase-pick
    "d"   'git-rebase-kill-line ;; or `git-rebase-drop'
    "b"   'git-rebase-break
    "e"   'git-rebase-edit
    "l"   'git-rebase-label
    "m"   'git-rebase-merge
    "M"   'git-rebase-merge-toggle-editmsg
    "s"   'git-rebase-squash
    "S"   'git-rebase-squish
    "f"   'git-rebase-fixup
    "F"   'git-rebase-alter
    "A"   'git-rebase-alter
    "q"   'undefined
    "i"   'undefined
    "v"   'helheim-git-rebase-toggle-selection
    "r"   'git-rebase-reword
    "w"   'git-rebase-reword
    "t"   'git-rebase-reset
    "u"   'git-rebase-undo
    "U"   'git-rebase-update-ref
    "x"   'git-rebase-exec
    "y"   'git-rebase-insert
    "n"   'git-rebase-noop
    ;;
    "}"   'forward-paragraph
    "{"   'backward-paragraph
    "] p" 'forward-paragraph
    "[ p" 'backward-paragraph)
  (remove-hook 'git-rebase-mode-hook #'git-rebase-mode-show-keybindings)
  (add-hook 'git-rebase-mode-hook 'helheim-git-rebase-mode-show-keybindings 90))

(defun helheim-git-rebase-toggle-selection ()
  "Toggle selection."
  (interactive)
  (if (use-region-p)
      (deactivate-mark)
    (set-mark-command nil)))

(defun helheim-git-rebase-mode-show-keybindings ()
  "Modify the \"Commands:\" section of the comment Git generates.
Modify that section to replace Git's one-letter command abbreviation,
with the key bindings used in Magit."
  (let ((inhibit-read-only t))
    ;; (save-excursion
    ;;   (save-match-data))
    (goto-char (point-min))
    (re-search-forward (concat git-rebase-comment-re "\\s-+Commands:")
                       nil t)
    (delete-region (point) (point-max))
    (cl-flet ((key (str)
                (propertize str 'font-lock-face 'help-key-binding))
              (comment (str)
                (propertize str 'font-lock-face 'font-lock-comment-face)))
      (-> (list ""
                (concat (key "M-j") (comment " / ") (key "M-k") (comment " rearrange commits"))
                (concat (key "p") (comment "  pick <commit> = use commit"))
                (concat (key "r") (comment "  reword <commit> = use commit, but edit the commit message"))
                (concat (key "e") (comment "  edit <commit> = use commit, but stop for amending"))
                (concat (key "s") (comment "  squash <commit> = use commit, but meld into previous commit"))
                (concat (key "f") (comment "  fixup <commit> = use commit, but meld into previous commit,"))
                (comment "                    dropping <commit>'s message")
                (concat (key "F") (comment "  fixup -C <commit> = use commit, but meld into previous commit,"))
                (comment "                       dropping previous commit's message")
                (concat (key "S") (comment "  fixup -c <commit> = use commit, but meld into previous commit"))
                (comment "            dropping previous commit's message, and open the editor")
                (concat (key "x") (comment "  exec <command> = run command (the rest of the line using shell"))
                (concat (key "b") (comment "  break = stop here (continue rebase later with 'git rebase --continue'"))
                (concat (key "d") (comment "  drop <commit> = remove commit"))
                (concat (key "l") (comment "  label <label> = label current HEAD with a name"))
                (concat (key "t") (comment "  reset <label> = reset HEAD to a label"))
                (concat (key "m") (comment "  merge [-C <commit> | -c <commit>] <label> [# <oneline>]"))
                (comment "        create a merge commit using the original merge commit's")
                (comment "        message (or the oneline, if no original merge commit was")
                (comment "        specified); use -c <commit> to reword the commit message")
                (concat (key "U") (comment "  update-ref <ref> = track a placeholder for the <ref> to be updated"))
                (comment "                      to this position in the new commits. The <ref> is")
                (comment "                      updated at the end of the rebase")
                (concat (key "RET") (comment " show commit in another window"))
                (concat (key "u") (comment "   undo last change"))
                (concat (key "i") (comment "   switch to normal state"))
                (concat (key "ZZ") (comment "  proceed rebase"))
                (concat (key "ZQ") (comment "  abort rebase"))
                ""
                (comment "these lines can be re-ordered; they are executed from top to bottom.")
                ""
                (comment "if you remove a line here that commit will be lost.")
                ""
                (comment "however, if you remove everything, the rebase will be aborted.")
                "")
          (string-join (comment (concat "\n" comment-start " ")))
          (insert)))
    (goto-char (point-min))))

;;; With-editor

(with-eval-after-load 'with-editor
  (hel-keymap-set with-editor-mode-map :state '(normal motion)
    "Z Z" 'with-editor-finish
    "Z Q" 'with-editor-cancel))

;;; Other

(add-hook 'magit-blob-mode-hook #'hel-update-active-keymaps)

;; Unset all keys in base keymap to be able to use them in Insert state
;; in Ediff in ~{index}~ buffer.
(hel-keymap-set magit-blob-mode-map :unset '("g" "p" "n" "b" "r" "f" "q"))
(hel-keymap-set magit-blob-mode-map :state 'normal
  "] ]" 'magit-blob-next
  "[ [" 'magit-blob-previous
  "] v" 'magit-blob-next
  "[ v" 'magit-blob-previous
  ", a" 'magit-blame-addition
  ", r" 'magit-blame-removal
  ", f" 'magit-blame-reverse
  "q"   'magit-bury-or-kill-buffer)

;;; Transient dispatches

(transient-suffix-put 'magit-dispatch "Z" :key "%") ;; `magit-worktree'
(transient-suffix-put 'magit-dispatch "z" :key "Z") ;; `magit-stash'

(transient-suffix-put 'magit-branch "d" :key "D") ;; `magit-branch.<branch>.description'
(transient-suffix-put 'magit-branch "k" :key "d") ;; `magit-branch-delete'

(transient-suffix-put 'magit-stash "k" :key "d") ;; `magit-stash-drop'
(transient-suffix-put 'magit-worktree "k" :key "d") ;; `magit-worktree-delete'

;;; Commands

(defun helheim-magit-toggle-selection ()
  (interactive)
  (if (use-region-p)
      (deactivate-mark)
    (push-mark-command t)))

;; TODO: currently doesn't work
(define-minor-mode helheim-magit-text-mode
  "Switch to `text-mode' in current magit buffer."
  :init-value nil
  (if helheim-magit-text-mode
      (progn
        (setq-local helheim-magit--previous-magit-mode major-mode)
        (text-mode))
    ;; else
    (funcall helheim-magit--previous-magit-mode)
    (setq-local helheim-magit--previous-magit-mode nil)
    (magit-refresh))
  (hel-switch-to-initial-state))

;;; .
(provide 'helheim-magit '(keys))
;;; helheim-magit-keys.el ends here
