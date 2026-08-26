;;; helheim-completion-lib.el -*- lexical-binding: t -*-
;;; Code:

;;;###autoload
(defun +corfu-inhibit-auto-completion-a (&rest _)
  "Honour `+corfu-inhibit-auto-functions'."
  (not (run-hook-with-args-until-success '+corfu-inhibit-auto-functions)))

;;; Commands

;;;###autoload
(defun +corfu-move-to-minibuffer ()
  "Move Corfu completion session to the minibuffer."
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           (completion-cycle-threshold nil)
           (completion-cycling nil))
       (consult-completion-in-region beg end table pred)))))

(add-to-list 'corfu-continue-commands #'+corfu-move-to-minibuffer)

;;; .
(provide 'helheim-completion '(lib))
;;; helheim-completion-lib.el ends here
