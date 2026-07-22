;;; helheim-xref-lib.el                               -*- lexical-binding: t -*-
;;; Make Xref try all backends untill first one succeed

(eval-when-compile (require 'cl-macs))
(require 'helheim-lib)
(require 'xref)

(defun helheim-xref--create-fetcher (kind)
  "Create a fetcher function for xref KIND.
Iterates through all backends in `xref-backend-functions' and returns
a lambda that captures the xrefs from the first backend that successfully
finds an identifier at point. Signals a user-error if no backend succeeds.

KIND should be a symbol like 'definitions or 'references, which will be
used to construct the backend method name."
  (or (cl-dolist (func (+hook-values 'xref-backend-functions))
        (if-let* ((backend (funcall func))
                  (identifier (xref-backend-identifier-at-point backend))
                  (method (intern (format "xref-backend-%s" kind)))
                  (xrefs (funcall method backend identifier)))
            (cl-return (lambda () xrefs))))
      (user-error "Nothing found")))

;;;###autoload
(defun helheim-xref-find-definitions ()
  "Find the definition of the identifier at point.

The original `xref-find-definitions' command tries all backends in
`xref-backend-functions' in order to \"find suitable for current context\".
The intended design seems to be that all possible backends for all
major modes are stored in a single global variable, and then `xref-find-backend'
selects an appropriate one.

In practice, however, `xref-find-backend' simply calls each function in
`xref-backend-functions' in order until it finds one that returns a symbol
\(`cl-defgeneric' will dispatch on). In practice typical get backend function
looks like this:

  (defun eglot-xref-backend ()
    \"Eglot xref backend.\"
    'eglot)

Such functions contain no logic of their own, so Xref always picks the
first backend in the list, and the rest are never tried.  Whoever first
puts on the lab coat becomes the doctor.

This command, in contrast, tries all registered backends in sequence until
the first one succeeds in finding definitions."
  (interactive)
  (xref--show-defs (helheim-xref--create-fetcher 'definitions) nil)
  (setq disable-point-adjustment t))

;;;###autoload
(defun helheim-xref-find-references ()
  "Find the references of the identifier at point.
Reed help for `helheim-xref-find-definitions' for the differences from
`xref-find-references'."
  (interactive)
  (xref--show-xrefs (helheim-xref--create-fetcher 'references) nil)
  (setq disable-point-adjustment t))

;;;###autoload
(defun helheim-xref-find-definitions-other-window ()
  "Like `helheim-xref-find-definitions' but switch to the other window."
  (interactive)
  (xref--show-defs (helheim-xref--create-fetcher 'definitions)
                   'window)
  (setq disable-point-adjustment t))

;;;###autoload
(defun helheim-xref-find-definitions-other-frame ()
  "Like `helheim-xref-find-definitions' but switch to the other frame."
  (interactive)
  (xref--show-defs (helheim-xref--create-fetcher 'definitions)
                   'frame)
  (setq disable-point-adjustment t))

;;; .
(provide 'helheim-xref '(lib))
;;; helheim-xref-lib.el ends here
