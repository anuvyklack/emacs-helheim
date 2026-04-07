;;; helheim-setup.el -*- lexical-binding: t -*-
;;; Commentary:
;;
;; `setup' doesn't rearrange blocks of code — it is not declarative.
;; It is just a syntax sugar macros.
;;
;;; Code:

(eval-when-compile (require 'cl-macs))
(require 'dash)
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
         (straight-use-package ',recipe)
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

;;; :after-load

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
  :documentation
  "Evaluate current `setup' macro body after all the FEATURES will been loaded.")

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
    (cond ((memq (car-safe functions) '(lambda defun))
           (cond ((symbolp hooks)
                  `(add-hook ',hooks ,functions))
                 ((proper-list-p hooks)
                  (cl-with-gensyms x
                    (let ((x functions))
                      (cl-loop for hook in hooks
                               collect `(add-hook ',hook ,x)))))))
          ;; else
          ((or (proper-list-p hooks)
               (proper-list-p functions))
           `(progn
              ,@(->> (-table-flat #'list (ensure-list hooks) (ensure-list functions))
                     (-map (-lambda ((hook func))
                             `(add-hook ',hook ',func))))))
          (t
           `(add-hook ',hooks ',functions))))
  :documentation
  "Add FUNCTIONS to HOOKS.
This macro can map many. Also it accepts lambda form.")

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

;;; :with-keymap

(setup-define :with-keymap
  (lambda (keymap &rest body)
    (macroexp-let2 symbolp map keymap
      (setup-bind body (map map))))
  :debug '(sexp setup)
  :indent 1
  :documentation
  "Change the KEYMAP that BODY will bind to.
KEYMAP can be any form that evaluates to keymap.")

(setup-define :with-keymaps
  (lambda (keymaps &rest body)
    (macroexp-progn
     (cl-loop for map in keymaps
              collect (setup-bind body (map map)))))
  :debug '([&rest symbol] setup)
  :indent 1
  :documentation
  "For each item in KEYMAPS expand BODY with current keymap bound to it.
KEYMAPS should be an unquoted list of symbols.")

;;; :bind

(setup-define :bind
  (lambda (&rest args)
    (cl-loop for key in-ref args by #'cddr
             when (vectorp key)
             do (setf key (key-description key)))
    `(hel-keymap-set ,(setup-get 'map) ,@args))
  :indent 'defun
  :documentation
  "\(:bind [:state STATE] &rest [KEY DEFINITION]...)

Bind KEYs to DEFINITIONs in current keymap.

STATE is an optional keyword argument that specifies the Hel state in which the
keybindings will be active. Can be a symbol or list of symbols.

KEY and DEFINITION arguments are like those in `keymap-set'.")

(setup-define :unbind
  (lambda (&rest keys)
    `(hel-keymap-set ,(setup-get 'map)
       ,@(cl-loop for key in keys
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
    (cl-loop for key in-ref args by #'cddr
             when (vectorp key)
             do (setf key (key-description key)))
    `(hel-keymap-global-set ,@args))
  :indent 'defun
  :documentation
  "\(:global-bind [:state STATE] &rest [KEY DEFINITION]...)

STATE is an optional keyword argument that specifies the Hel state in which the
keybindings will be active. Can be a symbol or list of symbols.")

;;; :blackout

(setup-define :blackout
  (lambda (&rest modes)
    (or modes (setq modes '(t)))
    `(with-eval-after-load ',(setup-get 'feature)
       ,@(cl-loop for mode in modes
                  collect (pcase mode
                            ('t
                             `(blackout ',(setup-get 'mode)))
                            ((and (pred symbolp) mode)
                             `(blackout ',mode ))
                            ((and (pred stringp) replacement)
                             `(blackout ',(setup-get 'mode) replacement))
                            (`(,mode . ,replacement)
                             `(blackout ',mode replacement))))))
  :documentation
  "Do not display MODE in the mode line.

    \(setup foo
      (:blackout t))                   =>  \(blackout 'foo-mode nil)

    \(setup foo
      (:blackout \" Foo\"))              =>  \(blackout 'foo-mode \" Foo\")

    \(setup foo
      \(:blackout \(foo-mode . \" Foo\"))  =>  \(blackout 'foo-mode \" Foo\")")

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
    `(or ,condition
         ,(setup-quit)))
  :debug '(form)
  :repeatable t
  :documentation "If CONDITION evaluates to nil, stop evaluating the body.")

;;; .
(provide 'helheim-setup)
;;; helheim-setup.el ends here
