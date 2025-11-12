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
;;     to complete. With interaction set to 'none' you have to be fast.
;;  3. 'Compromise': Set timer as in '2.'. But  interaction to 'reset'.
;;     This will make the second digit reset the timer, buying you some
;;     time to override the exit action, but making it a tiny bit slower
;;     to “wait” for the default action on two-digit candidates.
;;     It shouldn't feel like that though, since you spend the same time
;;     idle as after a single-digit candidate selection.
;;
;;  Advice:
;;
;;  - If your keyboard (-layout) allows to access digit keys with one hand
;;    (i.e. if you have a number block) consider binding custom actions to
;;    keys accessible with the other hand.
;;    'Speed' and 'Compromise' workflows greatly benefit from having one
;;    hand access the digit keys and the other access keys for the exit
;;    Action.
;;
;;  Action Hint Display
;;
;;    By default a hint is displayed in the minibuffer informing about the
;;    action the timer is going to run. It is displayed only if the default
;;    action has changed. You can customize that behaviour:

;;    - If you don't like the display of the action-hint in the minibuffer
;;      you can disable it by unsetting `vertico-timer-action-hint-fstring'
;;    - If you want the action-hint even for your default action you can do
;;      (function-put vertico-timer-default-action
;;                    'vertico-timer-action-hint "exit")
;;
;;; Todos:
;;
;;  - TODO Don't depend on the user starting `vertico-indexed-mode'
;;  - TODO Provide Functions that set exit action before selection
;;
;;; Code:

(require 'vertico)
(require 'vertico-indexed)


;;; Variables

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

(defcustom vertico-timer-timeout-seconds 0.5
  "How many seconds to wait before running `vertico-timer--action'."
  :type 'number
  :group 'vertico-timer)

(defcustom vertico-timer-default-action #'vertico-exit
  "Default Action to run after selecting a candidate with digit keys."
  :type 'function
  :group 'vertico-timer)

(defvar vertico-timer--prefixes '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "What to index the candidates with.
Must be \\='(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9).")

(defcustom vertico-timer-key-interaction 'reset
  "What happens to the timer after a key was pressed.
Only affects keys bound in `vertico-timer-ticking-map'.
Default is \\='reset. Resetting the timer makes it easier to change the action
 executed on a candidate that is prefixed with a two-digit index."
  :type '(radio
          (const :tag "Reset Timer" reset)
          (const :tag "None" none))
  :group 'vertico-timer)

(defcustom vertico-timer-action-hint-fstring
  (concat
   (propertize " | " 'face 'shadow)
   (propertize "⏲ " 'face 'font-lock-comment-face)
   (propertize "%s" 'face 'font-lock-constant-face)
   (propertize " | " 'face 'shadow))
  "Format string used for the display of the currently active action.
'%s' will be replaced with the \\='vertico-timer-action-hint property of
the current value of `vertico-timer--action'."
  :type '(radio (const :tag "No Action Hint" nil) (string))
  :group 'vertico-timer)

(defvar vertico-timer--vertico-indexed-commands-orig
  vertico-indexed--commands
  "Helper state for teardown. Original value of `vertico-indexed--commands'.")

(defvar-local vertico-timer--action vertico-timer-default-action
  "Action to run after selecting a candidate with digit keys.")


;;; Timer

(defvar-local vertico-timer--timer nil
  "Stores the current timer.")

(defvar-local vertico-timer--started-at nil
  "When the timer was started.")

(defvar-local vertico-timer--exit-map-fn nil
  "Call this function to exit the transient keymap.")

(define-error 'vertico-timer-no-timer "No timer found")

(defun vertico-timer--start-timer ()
  "Start timer with `vertico-timer-timeout-seconds'.
Timer is stored in `vertico-timer--timer'."
  (setq vertico-timer--started-at (current-time))
  (setq vertico-timer--timer
        (run-at-time vertico-timer-timeout-seconds
                     nil #'vertico-timer--run-action)))

(defun vertico-timer--stop-timer ()
  "Cancel the current timer."
  (funcall vertico-timer--exit-map-fn)
  (condition-case er
      (cancel-timer vertico-timer--timer)
    (wrong-type-argument
     (signal 'vertico-timer-no-timer
             (apply #'list 'cancel-timer vertico-timer--timer er)))))

;; No need to turn `vertico-timer--ticking-mode' off and on
(defun vertico-timer--reset-timer ()
  "Reset the current timer."
  (cancel-timer vertico-timer--timer)
  (setq vertico-timer--timer
        (run-at-time vertico-timer-timeout-seconds
                     nil #'vertico-timer--run-action)))


;;; Digit Keys

(defun vertico-timer--set-digit-keys (map cmd)
  "Set all digit keys in MAP to CMD."
  (mapc (lambda (n)
          (define-key map (vector n) cmd))
        vertico-timer--prefixes))

(defun vertico-timer--digit-argument (arg)
  "Handles ARG like `digit-argument'.
Instead of `universal-argument-map' activates `vertico-timer-ticking-map'."
  (prefix-command-preserve-state)
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (setq prefix-arg (cond ((integerp arg)
                            (+ (* arg 10)
                               (if (< arg 0) (- digit) digit)))
                           ((eq arg '-)
                            ;; Treat -0 as just -, so that -01 will work.
                            (if (zerop digit) '- (- digit)))
                           (t
                            digit))))
  ;; Highlight selected candidate
  (vertico--goto prefix-arg)
  (vertico-timer--ticking-mode))

(defun vertico-timer--successive-digit (arg)
  "Call `digit-argument' with ARG and interact with timer.
Interaction is determined by `vertico-timer-key-interaction'."
  (interactive "P")
  ;; Add to prefix-arg first for it to be accessible from new timer
  (vertico-timer--digit-argument arg)

  ;; Handle timer interaction
  (pcase vertico-timer-key-interaction
    ('reset (vertico-timer--reset-timer))
    ('none nil)
    (_ nil)))

(defvar vertico-timer-ticking-map
  (let ((map (make-sparse-keymap)))
    (vertico-timer--set-digit-keys map #'vertico-timer--successive-digit)
    map)
  "Keymap for `vertico-timer-ticking' minor mode.")

;; NOTE It is tempting to ditch the explicit timer completely and
;; instead use the properties of `set-transient-map':
;; - TIMEOUT allows us to have the basic timer functionality
;; - ON-EXIT can replace `vertico-timer--run-action'
;; - KEEP-PRED allows us to handle `vertico-timer-key-interaction' correctly:
;;   - If the latter is set to 'none' we just don't call `set-transient-map' from
;;     `vertico-timer--successive-digit' again but KEEP-PRED will keep the map active
;;     until the first timer runs out
;;   - If the latter is set to 'reset' we call `set-transient-map' from
;;     `vertico-timer--successive-digit' no need for KEEP-PRED in that case
;; - Lastly we can model cancelling the timer using the return value of
;;   `set-transient-map' which is a function that will terminate the map. Since
;;   ON-EXIT will call `vertico-timer--run-action' the behaviour is the same as it
;;   is now
;; However ONE issue remains: ON-EXIT is always called, hence when we "reset" the
;; timer after a second keypress it would run our action prematurely. We could hack
;; the intended behaviour together by wrapping ON-EXIT in `(if should-call-p)' but
;; having an explicit timer may be cleaner.
(defun vertico-timer--ticking-mode ()
  "Activate `vertico-timer-ticking-map'."
  (prefix-command-update)
  (setq vertico-timer--exit-map-fn
        (set-transient-map vertico-timer-ticking-map)))

(defun vertico-timer--run-action ()
  "Call `vertico-timer--action' on the selection.
Disable `i-vertico/timer-mode' beforehand."
  (prefix-command-preserve-state)

  ;; Since this fn is called by the timer I would've expected
  ;; `cancel-timer' to fail. This is in fact not the case. In
  ;; case that ever changes we swallow the expected error.
  (condition-case _
      (vertico-timer--stop-timer)
    (vertico-timer-no-timer nil))

  (unwind-protect ;; Trigger `vertico--prepare'
      (progn (run-hooks 'pre-command-hook)
             (call-interactively vertico-timer--action)
             (run-hooks 'post-command-hook))
    ;; Reset the default action in case previous one was non-exiting
    ;; Run as unwindform in case the action was cancelled
    (vertico-timer--set-action vertico-timer-default-action)))

(defun vertico-timer--first-digit (arg)
  "Make ARG part of the `prefix-arg' if `vertico-indexed-mode' is non-nil.
Otherwise call `self-insert-command'.
As opposed to `digital-argument' doesn't activate `universal-argument-map' but
`vertico-timer-ticking-map'."
  (interactive "P")
  (if (not vertico-indexed-mode)
      ;; User un-toggled using `vertico-timer-toggle-in-session'
      (call-interactively #'self-insert-command)
    (vertico-timer--digit-argument arg)
    (setq this-command #'vertico-timer--first-digit)
    (vertico-timer--start-timer)))


;;; Actions

;; Don't use the macro for the default actions to prevent naming conflicts

(defun vertico-timer-stop-exit ()
  "Stop the timer and exit with default."
  (interactive)
  (vertico-timer--set-action vertico-timer-default-action)
  (vertico-timer--run-action))

(defun vertico-timer-stop-insert ()
  "Stop the timer and insert the candidate."
  (interactive)
  (vertico-timer--set-action #'vertico-insert)
  (vertico-timer--run-action))

(keymap-set vertico-timer-ticking-map vertico-timer-exit-action-key
            #'vertico-timer-stop-exit)
(keymap-set vertico-timer-ticking-map vertico-timer-insert-action-key
            #'vertico-timer-stop-insert)

(function-put 'vertico-insert 'vertico-timer-action-hint "insert")

;; User-defined Actions

(defmacro vertico-timer-register-action (key cmd &optional name)
  "Bind CMD to KEY in `vertico-timer-ticking-map'.
KEY can be used to invoke CMD on the candidate before the timer runs out.

If NAME is provided it will be used as the suffix for the name of the
command bound in the map. Otherwise a name will be generated.
NAME will also show up in the action-hint overlay."
  (unless (key-valid-p (eval key))
    (error "KEY must satisfy `key-valid-p'"))
  (unless (or (not name) (stringp name))
    (error "Name must be a string or nil"))
  (let ((body `((interactive)
                (prefix-command-preserve-state)
                (vertico-timer--set-action #',cmd)
                (vertico-timer--run-action))))
    (let* ((suffix (or name (symbol-name (gensym))))
           (action-fn-sym
            (intern (concat "vertico-timer-action-" suffix))))
      `(progn
         (defun ,action-fn-sym ()
           ,@body)
         (function-put ',cmd 'vertico-timer-action-hint
                       ,(or name (concat "cust-" suffix)))
         (keymap-set vertico-timer-ticking-map ,key
                     #',action-fn-sym)))))

(defmacro vertico-timer-register-actions (&rest args)
  "Register multiple actions using key-cmd pairs.
ARGS should be an even number of KEY CMD pairs
each of which can be followed by keyword options:

  - ':name': The function bound in `vertico-timer-ticking-map'
     will be suffixed with the value of this option.
     If nil a lambda will be used instead."
  (unless (zerop (% (length args) 2))
    (error "Arguments must be in KEY CMD pairs (even number of arguments)"))
  (let (forms)
    (while args
      (let* ((key (pop args))
             (cmd (pop args))
             name)
        ;; Check if next argument is :name
        (when (and args (eq (car args) :name))
          (pop args)
          (setq name (pop args)))
        (push `(vertico-timer-register-action ,key ,cmd ,name) forms)))
    `(progn ,@(nreverse forms))))


;;; Mode

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

  ;; Ensure `vertico-timer--first-digit' passes the vertico-indexed filter
  (unless (memq 'vertico-timer--first-digit vertico-indexed--commands)
    (push 'vertico-timer--first-digit vertico-indexed--commands))

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
   (lambda (key def) (define-key vertico-map (vector key) def))
   vertico-timer--original-digit-bindings)

  ;; Restore vertico-indexed state
  (setq vertico-indexed--commands vertico-timer--vertico-indexed-commands-orig))


;;; Display Hint

;; The post-command-hook is only run after vertico-timer--run-action finishes.
;; Even if the minibuffer stays active then, the action will already be reset.
;; Hence perform the update of the action hint whenever it is set.
(defun vertico-timer--set-action (fn)
  "Change action to be executed to FN.
Updates action hint if enabled."
  (setq vertico-timer--action fn)
  (vertico-timer--exhibit))

;; Ways to render hint:
;; - Make `vertico-count-format' buffer local and change it to include
;;   propertized string
;; - Reuse vertico--count-ov with 'after-string prop
;;   This seems to be cleanest, however the ov in my case inludes an
;;   invisible "-6s" which adds an unweildly space
;; - Also if another package uses this property there'll be conflicts
(defun vertico-timer--update-action-hint ()
  "Indicate the current value of `vertico-timer--action' in the minibuffer."
  (when (and (bound-and-true-p vertico-timer--action)
             (bound-and-true-p vertico-timer-action-hint-fstring)
             (overlayp vertico--count-ov))
    (if-let ((hint (function-get vertico-timer--action
                                 'vertico-timer-action-hint)))
        (overlay-put vertico--count-ov 'after-string
                     (format vertico-timer-action-hint-fstring
                             hint))
      ;; Unset hint when default action has none
      (when (overlay-get vertico--count-ov 'after-string)
        (overlay-put vertico--count-ov 'after-string
                     nil)))))

(defun vertico-timer--exhibit ()
  "Update the action hint with `vertico--protect'."
  (vertico--protect
   (lambda ()
     (let ((buffer-undo-list t)) ;; Overlays affect point position and undo list!
       (vertico-timer--update-action-hint)))))

(defun vertico-timer--enable-update-hint-h ()
  "Hook to set up the action hint once."
  (vertico-timer--exhibit)
  (remove-hook 'post-command-hook #'vertico-timer--enable-update-hint-h 'local))

(cl-defmethod vertico--setup :after
  (&context (vertico-timer-mode (eql t) vertico-indexed-mode (eql t)))
  "Setup the action hint display."
  ;; Vertico's overlay should be present already so we hook in after
  ;; Since updates to the overlay are done jit the hook will remove itself.
  (add-hook 'post-command-hook #'vertico-timer--enable-update-hint-h 1 'local))


;;; Utilities

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
