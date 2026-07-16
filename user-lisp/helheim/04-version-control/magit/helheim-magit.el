;;; helheim-magit.el              -*- lexical-binding: t; no-byte-compile: t -*-
;;; Keybindings

;; Entry points
(hel-keymap-set mode-specific-map
  "v RET" 'magit-status
  "v g"   'magit-status   ;; "C-x g"
  "v SPC" 'magit-dispatch
  "v /"   'magit-dispatch
  "v ?"   'magit-dispatch
  "v h"   'magit-dispatch ;; "h" in magit-status buffer
  "v f"   'magit-file-dispatch
  "f v"   'magit-file-dispatch)

;;; Config

(setup magit
  (:install t)
  (setopt magit-refresh-verbose debug-on-error
          magit-diff-refine-hunk t ;; show granular diffs in selected hunk
          ;; magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
          magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
          magit-bury-buffer-function #'magit-restore-window-configuration)
  (:after-load
    (magit-auto-revert-mode))
  ;; Turn ref links into clickable buttons.
  (:hook magit-process-mode-hook goto-address-mode)
  ;; The mode-line isn't useful in these popups and take up valuable screen
  ;; estate, so free it up.
  (:hook magit-popup-mode-hook hide-mode-line-mode))

(setup git-modes (:install t))

;;;; project.el: replace VC-dir with Magit

;; (with-eval-after-load 'project
;;   (hel-keymap-set project-prefix-map
;;     "v" 'helheim-project-magit)
;;   ;;
;;   ;; In `project-switch-project' dispatch replace VC-Dir with Magit.
;;   (when-let* ((index (-elem-index '(project-vc-dir "VC-Dir")
;;                                   project-switch-commands)))
;;     (setcar (nthcdr index project-switch-commands)
;;             '(helheim-project-magit "Magit"))))

(setup project
  (:after-load
    (:keymap project-prefix-map
      (:bind "v" 'helheim-project-magit))
    ;; In `project-switch-project' dispatch replace VC-Dir with Magit.
    (when-let* ((index (-elem-index '(project-vc-dir "VC-Dir")
                                    project-switch-commands)))
      (setcar (nthcdr index project-switch-commands)
              '(helheim-project-magit "Magit")))))

;;;###autoload
(defun helheim-project-magit ()
  "Open Magit status buffer in the current project's root."
  (interactive)
  (magit-status-setup-buffer (project-root (project-current t))))

;;; .
(provide 'helheim-magit)
;;; helheim-magit.el ends here
