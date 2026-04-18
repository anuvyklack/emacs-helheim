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
(setq setup-modifier-list '(helheim-setup--with-eval-after-load
                            helheim-setup--install-with-straight
                            setup-wrap-to-catch-quits
                            helheim-setup--install-with-elpaca))

;; Remove some macros shipped with `setup'.
(dolist (macro '( :package :with-map :autoload-this :require :global :bind
                  :unbind :rebind :bind-into :hook :hook-into :bind-to :option
                  :if-package :if-feature :only-if :file-match :when-loaded))
  (assq-delete-all macro setup-macros))

;;; utils

(defsubst helheim-setup-unprogn (body)
  "`macroexp-unprogn' BODY and delete all `nil' items from it."
  (->> (macroexp-unprogn body)
       (delete (list nil))
       (delq nil)))

;;; package managers
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
         ,@(helheim-setup-unprogn body))
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
      `(elpaca ',recipe ,@(helheim-setup-unprogn body))
    body))

;;; :setopt

(setup-define :setopt
  (lambda (variable value)
    (cl-assert (symbolp variable) nil "Attempting to set a non-symbol: %s" value)
    `(helheim-setopt ',variable ,value))
  :debug '(setq)
  :indent 0
  :repeatable t
  :documentation
  "Set VARIABLE to VALUE only if it hasn’t already been changed.
This macro is for internal use in Helheim to prevent user settings from
being overwritten by Helheim defaults. The Elpaca package manager is
asynchronous, so user settings in `init.el' are applied before Helheim
sets its defaults in `elpaca' macros. Use `setopt' for your custom
settings.")

(defun helheim-setopt (variable value)
  "Set VARIABLE to VALUE only if it hasn’t been already changed."
  (cond ((not (boundp variable))
         (set-default variable value))
        ((equal (symbol-value variable)
                (+original-value variable))
         (funcall (or (get variable 'custom-set)
                      #'set-default)
                  variable value))))

;;; :require

(setup-define :require
  (lambda (feature)
    `(require ',(if (eq feature t)
                    (setup-get 'feature)
                  feature)))
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

;;; :defer

(defvar helheim-initial-deferred-task-timeout (if (daemonp) 0 2.0)
  "Number of seconds Emacs will wait until starts executing deferred tasks.
Set this to nil to disable delayed loading at startup.
Set this to 0 to load all deferred packages immediately at `after-init-hook'.")

(defvar helheim-deferred-task-timeout 0.75
  "Number of seconds Emacs should idle untill eval next delayed function.")

(defvar helheim-deferred-tasks nil)

(defsubst helheim-defer (task)
  (declare (indent 0))
  (push task helheim-deferred-tasks))

(setup-define :defer
  (lambda (&rest body)
    `(helheim-defer (lambda () ,@body)))
  :debug '(setup)
  :indent 0
  :documentation
  "Evaluate BODY when Emacs is idle for DELAY seconds after startup.")

(add-hook 'emacs-startup-hook 'helheim-schedule-deferred-tasks 99)

(defun helheim-schedule-deferred-tasks ()
  (when helheim-deferred-tasks
    (run-with-idle-timer helheim-initial-deferred-task-timeout nil
                         'helheim--eval-deferred-tasks
                         (reverse helheim-deferred-tasks))
    (unless debug-on-error
      (setq helheim-deferred-tasks nil))))

(defun helheim--eval-deferred-tasks (tasks)
  (when tasks
    (condition-case-unless-debug e
        (let ((gc-cons-threshold most-positive-fixnum))
          (funcall (car tasks)))
      (error
       (message "Error in deferred task: %s" e)))
    (when (cdr tasks)
      (run-with-idle-timer helheim-deferred-task-timeout nil
                           'helheim--eval-deferred-tasks (cdr tasks)))))

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
                  ,@(helheim-setup-unprogn body))))
  body)

;;; :hook

(setup-define :hook
  (lambda (hooks &optional functions)
    (or functions (setq functions (setup-get 'mode)))
    (cond ((memq (car-safe functions) '(lambda defun))
           (cond ((symbolp hooks)
                  `(add-hook ',hooks ,functions))
                 ((proper-list-p hooks)
                  (cl-with-gensyms (fun)
                    `(let ((,fun ,functions))
                       ,@(cl-loop for hook in hooks
                                  collect `(add-hook ',hook ,fun)))))))
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

(setup-define :global-unbind
  (lambda (&rest keys)
    `(hel-keymap-global-set ,@(cl-loop for key in keys
                                       collect key
                                       collect nil)))
  :indent 'defun
  :documentation
  "\(:global-unbind [:state STATE] &rest KEYS)

Remove KEYS bindings from current keymap.

STATE is an optional keyword argument that specifies the Hel state. Can be
a symbol or list of symbols.")

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
