;;; -*- lexical-binding: t -*-

;;;###autoload
(defun helheim-keyboard-quit ()
  "Improved `keyboard-quit'."
  (interactive)
  (let ((inhibit-quit t))
    (cond ((< 0 (minibuffer-depth))
           (abort-recursive-edit))
          ;; don't abort macros
          ((not (or defining-kbd-macro
                    executing-kbd-macro))
           (keyboard-quit)))))
