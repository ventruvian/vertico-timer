;;; vertico-timer.el --- Select indexed candidates with a timer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Vincent Trötschel
;;
;; Author: Vincent Trötschel <vincent.troetschel@mailbox.org>
;; Maintainer: Vincent Trötschel <vincent.troetschel@mailbox.org>
;; Created: November 03, 2025
;; Modified: November 03, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/fatal: not a git repository: /Users/indiana/.config/doom/profiles/new/lisp/../../.git/modules/engrave-faces/vertico-timer
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package is a Vertico extension, in fact an extension to the
;;  vertico extension `vertico-indexed-mode', however It allows you to
;;  select candidates _without_ prefix arguments but digits directly.
;;  This is very fast but comes with the drawback of not easily being
;;  able to insert digit's into the minibuffer.
;;  Use `vertico-timer-toggle-in-session' or 'C-q' in that case.
;;
;;  Originally intended for speed an emergend feature is the ability
;;  to change the exit action (like `embark-act') _after_ candidate
;;  selection with very simple keys. Since the selected action is not
;;  executed before the timer runs out (unless you intervene) an
;;  additional keymap is available where no keys are needed for input
;;  anymore.
;;
;;  There are 3 main workflows which depend on the settings:
;;  - `vertico-timer-timeout-seconds'
;;    Determines how long the timer runs.
;;  - `vertico-timer-key-interaction'
;;    Defines how digit keypresses interact with the timer
;;
;;  Workflows:
;;
;;  1. 'Versatile': Set a long timer and prefer exit actions to return
;;     early. You have enough time now for a extra keypress, you can
;;     still be fairly quick with the default exit action on 'RET', but
;;     you have the versatility of chosing another with one simple key,
;;     and still no 'C-u' harmed in the process.
;;     You might want to set interaction to 'none' if you ever want
;;     the timer to safe you the last keypress.
;;  2. 'Speed': Set a short timer. You often use the default exit action
;;     (who doesn't) and want the timer to make that extra keypress for
;;     you. A timer of 0.25 seconds is comfortable enough to press two
;;     digits and instant enough to not having to “wait” for the timer
;;     to complete. With interaction set to 'none'
;;  3. 'Compromise': Set timer as in '2.'. But  interaction to 'reset'.
;;     This will make the second digit reset the timer, buying you some
;;     time to override the exit action, but making it a tiny bit slower
;;     to “wait” for the default action on two-digit candidates.
;;     It shouldn't feel like that though, since you spend the same time
;;     idle as after a single-digit candidate selection.
;;
;;; Todos:
;;
;;  - TODO Don't depend on the user starting `vertico-indexed-mode'
;;  - TODO Provide Functions that set exit action before selection
;;  - TODO Support non-exiting actions (Make 2 macros to register fns)
;;
;;; Code:

(require 'vertico)
(require 'vertico-indexed)

(defgroup vertico-timer nil
  "Select indexed candidates immediately."
  :group 'external)

(defcustom vertico-timer-exit-action-key "RET"
  "The key to press to trigger `vertico-exit'.
Defaults to \\='RET\\='."
  :type 'key
  :group 'vertico-timer)

(defcustom vertico-timer-insert-action-key "i"
  "The key to press to trigger `vertico-insert'.
Defaults to \\='i\\='."
  :type 'key
  :group 'vertico-timer)

(defcustom vertico-timer-timeout-seconds 0.25
  "How many seconds to wait before running `vertico-timer--action'."
  :type 'number
  :group 'vertico-timer)

(defcustom vertico-timer-default-action #'vertico-exit
  "Default Action to run after selecting a candidate with digit keys."
  :type 'function
  :group 'vertico-timer)

(defvar vertico-timer--prefixes '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  "What to index the candidates with.
Must be \\='(\"0\" \"1\" \"2\" \"3\" \"4\" \"5\" \"6\" \"7\" \"8\" \"9\").")

(defvar-local vertico-timer--action #'vertico-exit
  "Action to run after selecting a candidate with digit keys.")

(defcustom vertico-timer-key-interaction 'reset
  "What happens to the timer after a key was pressed.
Only affects keys bound in `vertico-timer-ticking-map'.
Default is \\='reset. Resetting the timer makes it easier to change the action
 executed on a candidate that is prefixed with a two-digit index."
  :type '(radio
          (const :tag "Reset Timer" reset)
          (const :tag "None" none))
  :group 'vertico-timer)

(defvar vertico-timer--vertico-indexed-commands-orig
  vertico-indexed--commands
  "Helper state for teardown. Same as `vertico-indexed--commands'.")

(defvar vertico-timer--commands (list vertico-timer-default-action #'vertico-timer--first-digit)
  "Holds commands recognized by vertico-timer.
Will be appended to `vertico-indexed--commands' during setup.
Since `vertico-indexed--commands' will be reset during teardown
this variable ensures that the mode can be reentered without
the user needing to re-register their commands.")

(defvar-local vertico-timer--action vertico-timer-default-action
  "Action to run after selecting a candidate with digit keys.")

;;:------------------------
;;; Timer
;;:------------------------

(defvar-local vertico-timer--timer nil
  "Stores the current timer.")

(define-error 'vertico-timer-no-timer "Timer wasn't found")

(defun vertico-timer--start-timer ()
  "Start timer with `vertico-timer-timeout-seconds'.
Timer is stored in `vertico-timer--timer'."
  (setq vertico-timer--timer
        (run-at-time vertico-timer-timeout-seconds
                     nil #'vertico-timer--run-action))
  (message "timer started")
  (vertico-timer-ticking 1))

(defun vertico-timer--stop-timer ()
  "Cancel the current timer."
  (vertico-timer-ticking -1)
  (condition-case er
      (cancel-timer vertico-timer--timer)
    (error (signal 'vertico-timer-no-timer
                   `(vertico-timer--timer ,er))))
  (message "timer stopped"))

;; No need to turn `vertico-timer-ticking' off and on
(defun vertico-timer--reset-timer ()
  "Reset the current timer."
  (message "timer reset")
  (cancel-timer vertico-timer--timer)
  (setq vertico-timer--timer
        (run-at-time vertico-timer-timeout-seconds
                     nil #'vertico-timer--run-action)))

(defun vertico-timer--set-digit-keys (map cmd)
  "Set all digit keys in MAP to CMD."
  (mapc (lambda (n)
          (keymap-set map n cmd))
        vertico-timer--prefixes))

(defun vertico-timer--successive-digit (&optional _)
  "Call `digit-argument' with ARG and interact with timer.
Interaction is determined by `vertico-timer-key-interaction'."
  (interactive "P")
  ;; Add to prefix-arg first to be accessible from new timer
  (message ">> called successive digit")
  (call-interactively #'digit-argument)
  ;; Handle timer interaction
  (pcase vertico-timer-key-interaction
    ('reset (vertico-timer--reset-timer)
            (message "> timer reset"))
    ('none (message "> not interacting with timer"))
    (_ nil)))

(define-minor-mode vertico-timer-ticking
  "Restore `digit-argument' call of digit keys and interact with timer.
Mode is active for the duration of `vertico-timer-timeout-seconds'.
Timer interaction is determined by `vertico-timer-key-interaction'."
  :keymap (let ((map (make-sparse-keymap)))
            (vertico-timer--set-digit-keys map #'vertico-timer--successive-digit)
            map)
  (if vertico-timer-ticking
      (message "ticking mode on")
    (message "ticking mode off")))

(defun vertico-timer--run-action ()
  "Call `vertico-timer--action' on the selection.
Disable `i-vertico/timer-mode' beforehand."
  ;; Disable `vertico-timer-ticking' mode
  ;; Since this is called by the timer we swallow the error
  (condition-case _
      (vertico-timer--stop-timer)
    (vertico-timer-no-timer nil))
  (message "
this-command:\t%s
real-this-cmd:\t%s
last-command:\t%s
real-last-cmd:\t%s
prefix-arg:\t%s
curr-pre-arg:\t%s"
           this-command real-this-command
           last-command real-last-command
           prefix-arg current-prefix-arg)
  ;; If timer calls this function `this-command' is unset
  (setq this-command (or this-command last-command))
  (message "tc (post): %s" this-command)
  ;; Run `vertico--prepare'
  (run-hooks 'pre-command-hook)
  (call-interactively vertico-timer--action)
  (run-hooks 'post-command-hook))

(defun vertico-timer--first-digit (&optional _)
  "In `vertico-indexed-mode' quick select a candidate.
Should only be callled from a digit keybind.

Successive digit keys are handled by `vertico-timer--successive-digit'."
  (interactive "P")
  (message "> called first-digit")
  (if (not vertico-indexed-mode)
      ;; User un-toggled using `vertico-timer-toggle-in-session'
      (call-interactively #'self-insert-command)
    (vertico-timer--start-timer)
    ;; Set prefix-arg and allow for subsequent invocations of `digit-argument'
    (call-interactively #'digit-argument)
    ;; Ensure that we pass the filter in `vertico-indexed'
    ;; We could also add `vertico-timer--first-digit',
    ;; however for some reason the command loop doesn't set
    ;; this-command=vt--first-digit, also first digit can
    ;; only ever call the default action, so that is fine.
    (setq this-command #'vertico-timer--first-digit)))

;;:------------------------
;;; Actions
;;:------------------------

;; Default Actions

(defun vertico-timer-stop-exit ()
  "Stop the timer and exit with default."
  (interactive)
  (vertico-timer--stop-timer)
  (setq vertico-timer--action #'vertico-exit)
  (vertico-timer--run-action))

(push 'vertico-timer-stop-exit vertico-timer--commands)

(keymap-set vertico-timer-ticking-map vertico-timer-exit-action-key
            #'vertico-timer-stop-exit)

(defun vertico-timer-stop-insert ()
  "Stop the timer and insert the candidate."
  (interactive)
  (vertico-timer--stop-timer)
  (setq vertico-timer--action #'vertico-insert)
  (vertico-timer--run-action))

(push 'vertico-timer-stop-insert vertico-timer--commands)

(keymap-set vertico-timer-ticking-map vertico-timer-insert-action-key
            #'vertico-timer-stop-insert)

;; Custom Actions

(defmacro vertico-timer-register-action (key cmd &optional exit name)
  "Bind CMD to KEY in `vertico-timer-ticking-map'.
KEY can be used to invoke CMD on the candidate before the timer runs out.

A non-nil EXIT will cause the action to cancel the timer and run immediately.

If NAME is provided it will be used as the suffix for the name of the
command bound in the map. Otherwise a name will be generated."
  (unless (key-valid-p (eval key))
    (error "KEY must satisfy `key-valid-p'"))
  (unless (or (not name) (stringp name))
    (error "Name must be a string or nil"))
  (let ((body (if exit
                  `((interactive "P")
                    (vertico-timer--stop-timer)
                    (setq vertico-timer--action #'vertico-exit)
                    (vertico-timer--run-action))
                `((interactive "P") ; this shouldn't actually need the prefix
                  (setq vertico-timer--action #',cmd)))))
    (let ((prepare-fn-sym
           (if name (intern
                     (concat "vertico-timer-" (unless exit "prepare-")
                             "action-" name))
             (gensym (concat "vertico-timer-"
                             (unless exit "prepare-") "action-" )))))
      `(progn
         (unless (memq ',prepare-fn-sym vertico-timer--commands)
           (push ',prepare-fn-sym vertico-timer--commands))
         (unless (memq ',prepare-fn-sym vertico-indexed--commands)
           (push ',prepare-fn-sym vertico-indexed--commands))
         (keymap-set vertico-timer-ticking-map ,key
                     (defun ,prepare-fn-sym (&optional arg)
                       ,@body))))))

(defmacro vertico-timer-register-actions (&rest args)
  "Register multiple actions using key-cmd pairs.
ARGS should be an even number of KEY CMD pairs
each of which can be followed by keyword options:

  - ':exit': If option is non-nil CMD will be called right away,
     the timer will be cancelled.

  - ':name': The function bound in `vertico-timer-ticking-map'
     will be suffixed with the value of this option.
     If nil a lambda will be used instead."
  (unless (zerop (% (length args) 2))
    (error "Arguments must be in KEY CMD pairs (even number of arguments)"))
  (let (forms)
    (while args
      (let* ((key (pop args))
             (cmd (pop args))
             exit name)
        (when (and args (eq (car args) :exit))
          (pop args)
          (setq exit (pop args)))
        ;; Check if next argument is :name
        (when (and args (eq (car args) :name))
          (pop args)
          (setq name (pop args)))
        (push `(vertico-timer-register-action ,key ,cmd ,exit ,name) forms)))
    `(progn ,@(nreverse forms))))

;;:------------------------
;;; Global Mode
;;:------------------------

;;;###autoload
(define-minor-mode vertico-timer-mode
  "Select indexed candidates immediately."
  :group 'vertico-timer
  :init-value nil
  :lighter " vt"
  :global t
  (if vertico-timer-mode
      (vertico-timer--setup)
    (vertico-timer--teardown)))

(defvar vertico-timer--original-digit-bindings (make-sparse-keymap)
  "Holds the user's original digit keybindings in `vertico-map' if any.
These keys are restored when `vertico-timer-mode' is disabled.")

(defun vertico-timer--setup ()
  "Hook up timer to digit keys in `vertico-map'."

  ;; Store original digit-keybindings in `vertico-map', if any
  (map-keymap
   (lambda (key def)
     (let ((k (single-key-description key)))
       (when (member k vertico-timer--prefixes)
         (keymap-set vertico-timer--original-digit-bindings
                     k def))))
   vertico-map)

  ;; Ensure `vertico-timer--commands' includes default action,
  ;; even if modifiey by user
  (unless (memq vertico-timer-default-action vertico-timer--commands)
    (push vertico-timer-default-action vertico-timer--commands))

  ;; Ensure all registered actions pass the vertico-indexed filter,
  (dolist (cmd vertico-timer--commands)
    (unless (memq cmd vertico-indexed--commands)
      (push cmd vertico-indexed--commands)))

  ;; Bind digit keys
  (vertico-timer--set-digit-keys vertico-map #'vertico-timer--first-digit))

(defun vertico-timer--teardown ()
  "Revert digit keys in `vertico-map'."

  ;; Unset vertico-timer bindings
  (mapc (lambda (n)
          (keymap-unset vertico-map n 'remove))
        vertico-timer--prefixes)

  ;; Restore previous bindings if any
  (map-keymap
   (lambda (key def) (keymap-set vertico-map (single-key-description key) def))
   vertico-timer--original-digit-bindings)

  ;; Restore vertico-indexed state
  (setq vertico-indexed--commands vertico-timer--vertico-indexed-commands-orig))

;;:------------------------
;;; Utilities
;;:------------------------

(defun vertico-timer-toggle-in-session ()
  "Toggle `vertico-indexed-mode' for a single completion session.
This will also inhibit all funcionality from `vertico-timer-mode'."
  (interactive)
  (if vertico-indexed-mode
      (progn (vertico-indexed-mode -1)
             (add-hook 'minibuffer-exit-hook
                       (lambda () (vertico-indexed-mode 1))
                       nil 'local))
    (vertico-indexed-mode 1)
    (add-hook 'minibuffer-exit-hook
              (lambda () (vertico-indexed-mode -1))
              nil 'local)))

(provide 'vertico-timer)
;;; vertico-timer.el ends here
