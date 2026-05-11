;;; helheim-whisper.el            -*- lexical-binding: t; no-byte-compile: t -*-

(setup whisper
  (:install whisper :host github :repo "natrys/whisper.el")
  (:setopt whisper-install-directory user-emacs-directory
           whisper-language "auto"
           whisper-model "base"
           whisper-translate nil
           whisper-return-cursor 'end ;; 'start
           whisper-use-threads (1- (num-processors)))
  (:global-bind "M-r" 'whisper-run))

(hel-define-advice whisper--insert-text (:around (orig-fun &rest args))
  "Select the inserted transcription text."
  (let ((deactivate-mark nil))
    (apply orig-fun args)
    (hel-normal-state)
    (hel-set-region whisper--marker (point)
                    (if (eq whisper-return-cursor 'start) -1))))

;;; .
(provide 'helheim-whisper)
;;; helheim-whisper.el ends here
