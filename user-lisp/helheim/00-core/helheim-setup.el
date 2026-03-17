;;; helheim-setup.el -*- lexical-binding: t -*-
;;; Commentary:
;;
;; `setup' doesn't rearrange blocks of code — it is not declarative.
;; It is just a syntax sugar macros.
;;
;;; Code:

(require 'setup)

;; Order matters: functions will be executed in the order they appear in list.
(setopt setup-modifier-list '(helheim-setup--with-eval-after-load
                              helheim-setup--install-with-straight
                              setup-wrap-to-catch-quits
                              helheim-setup--install-with-elpaca))

;; Remove some macros shipped with `setup'.
(dolist (macro '( :package :with-map :autoload-this :require :global :bind
                  :unbind :rebind :bind-into :hook :hook-into :bind-to :option
                  :if-package :if-feature :only-if :file-match :when-loaded))
  (assq-delete-all macro setup-macros))

;;; package manager
;;;; :install

(setup-define :install
  (lambda (package &rest recipe)
    (pcase helheim-package-manager
      ('straight `(:straight ,package ,@recipe))
      ('elpaca   `(:elpaca ,package ,@recipe))))
  :documentation
  "Install PACKAGE using package manager specified in `helheim-package-manager'.")

(setup-define :built-in
  (lambda ()
    '(:install nil))
  :documentation "Use built-in version of package.")

;;;; :straight

(setup-define :straight
  (lambda (package &rest recipe)
    (when (eq helheim-package-manager 'straight)
      (push (cons 'straight (pcase package
                              ('nil `(,(setup-get 'feature) :type built-in))
                              ('t (setup-get 'feature))
                              (_ (cons package recipe))))
            setup-attributes))
    nil)
  :documentation "Install PACKAGE with `straight-use-package'.")

(defun helheim-setup--install-with-straight (body _feature)
  (if-let* ((recipe (alist-get 'straight setup-attributes)))
      `(progn
         (or (straight-use-package ',recipe)
             ,(setup-quit))
         ,@(macroexp-unprogn body))
    body))

;;;; :elpaca

(setup-define :elpaca
  (lambda (package &rest recipe)
    (when (eq helheim-package-manager 'elpaca)
      (push (cons 'elpaca (cond ((eq package 't)
                                 (setup-get 'feature))
                                (recipe
                                 (cons package recipe))
                                (t package)))
            setup-attributes))
    nil)
  :documentation "Install PACKAGE with `elpaca'.")

(defun helheim-setup--install-with-elpaca (body _feature)
  (if-let* ((recipe (alist-get 'elpaca setup-attributes)))
      `(elpaca ',recipe ,@(macroexp-unprogn body))
    body))

;;; :require

(setup-define :require
  (lambda (feature)
    (when (eq feature 't)
      (setq feature (setup-get 'feature)))
    `(or (require ',feature nil t)
         ,(setup-quit)))
  :repeatable t
  :documentation "Try to require FEATURE, or stop evaluating body.")

;;; :config

(setup-define :config
  (lambda (&rest body)
    (macroexp-progn body))
  :debug '(setup)
  :indent 0
  :documentation "Evaluate BODY after the current feature has been loaded.")

;;; :defer-config

(setup-define :after-load
  (lambda (&rest body)
    (macroexp-progn body))
  :debug '(setup)
  :after-loaded t
  :indent 0
  :documentation "Evaluate BODY after the current feature has been loaded.")

;;; :after

(setup-define :after
  (lambda (&rest features)
    (push `(after . ,features) setup-attributes)
    nil)
  :documentation "Evaluate BODY after all the FEATURES will been loaded.")

(defun helheim-setup--with-eval-after-load (body _feature)
  "Wrap BODY in `with-eval-after-load' form."
  (dolist (feature (nreverse (alist-get 'after setup-attributes)))
    (setq body `(with-eval-after-load ',feature
                  ,@(macroexp-unprogn body))))
  body)

;;; :hook

(setup-define :hook
  (lambda (hooks &optional functions)
    (or functions (setq functions (setup-get 'mode)))
    (macroexp-progn (let (result)
                      (dolist (hook (ensure-list hooks))
                        (dolist (func (ensure-list functions))
                          (push `(add-hook ',hook ',func)
                                result)))
                      (nreverse result))))
  :documentation
  "Add FUNCTIONS to HOOKS. The main purpose of this macro is to map many to
many. It doesn't work with `(lambda ...)' — use `add-hook' instead.")

;;; :after-init

(setup-define :after-init
  (lambda (&rest body)
    (let ((hook (pcase helheim-package-manager
                  ('straight 'after-init-hook)
                  ('elpaca   'elpaca-after-init-hook)))
          (func (pcase (car body)
                  ((pred symbolp) `',(car body))
                  (`(lambda . ,_) (car body))
                  (_  `(lambda () ,@body)))))
      `(add-hook ',hook ,func)))
  :indent 0
  :documentation
  "Eval BODY in `after-init-hook'. If Elpaca package manager is used — eval in
`elpaca-after-init-hook' instead. BODY can be a symbol, a lambda, or any number
or forms that will be wrapped in lambda in this case.")

;;; :bind

(setup-define :with-keymap
  (lambda (keymaps &rest body)
    (let (bodies)
      (dolist (map (ensure-list keymaps))
        (push (setup-bind body (map map))
              bodies))
      (macroexp-progn (nreverse bodies))))
  :debug '([&or ([&rest sexp]) sexp] setup)
  :indent 1
  :documentation
  "Change the KEYMAP that BODY will bind to. If KEYMAP is a list, apply BODY to
all elements of MAP.")

(setup-define :bind
  (lambda (&rest args)
    (pcase-let* ((`(,kwargs . ,args) (hel-split-keyword-args args))
                 (args (cl-loop for (key command) on args by #'cddr
                                do (when (vectorp key)
                                     (setq key (key-description key)))
                                collect key collect command)))
      `(hel-keymap-set ,(setup-get 'map) ,@kwargs ,@args)))
  :indent 'defun
  :documentation
  "\(:bind [:state STATE] &rest [KEY DEFINITION]...)

Bind KEYs to DEFINITIONs in current keymap.

STATE is an optional keyword argument that specifies the Hel state in which the
keybindings will be active. Can be a symbol or list of symbols.

KEY and DEFINITION arguments are like those in `keymap-set'.")

(setup-define :unbind
  (lambda (&rest keys)
    `(hel-keymap-set ,(setup-get 'map) ,@(cl-loop for key in keys
                                                  collect key
                                                  collect nil)))
  :indent 'defun
  :documentation
  "\(:unbind [:state STATE] &rest KEYS)

Remove KEYS bindings from current keymap.

STATE is an optional keyword argument that specifies the Hel state. Can be
a symbol or list of symbols.")

;;; :global-bind

(setup-define :global-bind
  (lambda (&rest args)
    (pcase-let* ((`(,kwargs . ,args) (hel-split-keyword-args args))
                 (args (cl-loop for (key command) on args by #'cddr
                                do (when (vectorp key)
                                     (setq key (key-description key)))
                                collect key
                                collect command)))
      `(hel-keymap-global-set ,@kwargs ,@args)))
  :indent 'defun
  :documentation
  "\(:global-bind [:state STATE] &rest [KEY DEFINITION]...)

STATE is an optional keyword argument that specifies the Hel state in which the
keybindings will be active. Can be a symbol or list of symbols.")

;;; :blackout

(setup-define :blackout
  (lambda (mode)
    (when (eq mode 't)
      (setq mode (setup-get 'mode)))
    `(blackout ',mode nil))
  :repeatable t
  :documentation "Don't show MODE in the modeline.")

;;; :autoload

(setup-define :autoload
  (lambda (command)
    `(autoload ,command ,(format "%s" (setup-get 'feature)) nil t))
  :ensure '(func)
  :repeatable t
  :documentation "Autoload COMMAND.")

;;; :mode

(setup-define :mode
  (lambda (cons-cell)
    `(add-to-list 'auto-mode-alist ',cons-cell))
  :debug '(form)
  :repeatable t
  :documentation "Add cons-cell to `auto-mode-alist'.")

;;; :when

(setup-define :when
  (lambda (condition)
    `(unless ,condition
       ,(setup-quit)))
  :debug '(form)
  :repeatable t
  :documentation "If CONDITION evaluates to nil, stop evaluating the body.")

;;; .
(provide 'helheim-setup)
;;; helheim-setup.el ends here
