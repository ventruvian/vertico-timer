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
;; Homepage: https://github.com/VitruvianVice/vertico-timer.git
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
;;  Originally intended for speed an emergent feature is the ability
;;  to change the exit action (like `embark-act') _after_ candidate
;;  selection with very simple keys. Since the selected action is not
;;  executed before the timer runs out (unless you intervene) an
;;  empty keymap is available where no keys are needed for insertion.
;;
;;  You use digits to select a candidate, then choose the exit action.
;;  With this usage pattern there there are 3 main workflows which
;;  depend on the following settings. Additionally there is a way of
;;  using vertico-timer with the “wrong” foot first, goofy.
;;
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
;;  4. 'Goofy': Choose the action before you start the timer by pressing
;;     a digit key. You probably have to make do with keychords now since
;;     letter keys are needed for input, but sometimes it may be a better
;;     mental model of the operation at hand, when you know already what
;;     you want to do, but not to whom spotted the candidate yet. I.e.
;;     you may already know you want to `embark-become' into another
;;     command but haven't spotted the candidate yet. This flow is of
;;     course compatible with all the others. You can always go goofy.
;;
;;  Advice:
;;
;;  - You may want to ignore custom exit actions at first and only quick
;;    select candidates with the default action, this is, after all, the
;;    main use-case and getting keys for exit actions right isn't trivial.
;;  - That said if you do get into exit actions: Take some time to consider
;;    the keys you bind. The value of this package drastically depends on
;;    how comfortably you can type a key succesion. One tip regarding that:
;;  - If your keyboard (-layout) allows accessing digit keys with one hand
;;    (i.e. if you have a number block) consider binding custom actions to
;;    keys accessible with the other hand.
;;    'Speed' and 'Compromise' workflows greatly benefit from having one
;;    hand access the digit keys and the other access keys for the exit
;;    Action.
;;
;;  Action Hint Display
;;
;;    By default a hint is displayed in the minibuffer informing about the
;;    action the timer is going to run. You can customize that behaviour:

;;    - If you don't like the display of the action-hint in the minibuffer
;;      you can disable it by unsetting `vertico-timer-action-hint-fstring'
;;      When you cycle the available actions it will be shown regardless.
;;    - If you want the hint displayed only if the default action has
;;      changed unset `vertico-timer-default-action-hint'
;;
;;  Cycle the Active Action:
;;
;;    You can cycle the active action with the commands:
;;    `vertico-timer-cycle-actions-forward'
;;    `vertico-timer-cycle-actions-backward'
;;    This is intended to be done before selecting a candidate, but it
;;    also works after. Cycling will force the display of the action hint
;;    for the completion session even if it is disabled otherwise.
;;
;;; Todos:
;;
;;  - TODO Don't depend on the user starting `vertico-indexed-mode'
;;
;;; Code:

(require 'vertico)
(require 'vertico-indexed)
(require 'ring)


;;; Variables

(defgroup vertico-timer nil
  "Select indexed candidates immediately."
  :group 'external)

(defcustom vertico-timer-timeout-seconds 0.5
  "How many seconds to wait before running `vertico-timer--action'."
  :type 'number
  :group 'vertico-timer)

(defcustom vertico-timer-default-action #'vertico-exit
  "Default Action to run after selecting a candidate with digit keys."
  :type 'function
  :group 'vertico-timer)

(defcustom vertico-timer-default-action-hint "default"
  "Hint to display in the action hint overlay when default action is active."
  :type '(radio
          (const :tag "Hide hint when default action is active")
          string)
  :group 'vertico-timer)

(defcustom vertico-timer-insert-action-hint "insert"
  "Hint to display in the action hint overlay when default action is active."
  :type '(radio
          (const :tag "Hide hint when insert action is active")
          string)
  :group 'vertico-timer)

(defcustom vertico-timer-key-interaction 'reset
  "What happens to the timer after a key was pressed.
Only affects keys bound in `vertico-timer-ticking-map'.
Default is \\='reset. Resetting the timer makes it easier to change the action
 executed on a candidate that is prefixed with a two-digit index."
  :type '(radio
          (const :tag "Reset Timer" reset)
          (const :tag "None" none))
  :group 'vertico-timer)

(defvar vertico-timer--prefixes '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  "What to index the candidates with.
Must be \\='(\"0\" \"1\" \"2\" \"3\" \"4\" \"5\" \"6\" \"7\" \"8\" \"9\").")

(defcustom vertico-timer-action-hint-fstring nil
  "Format string used for the display of the currently active action.
'%s' will be replaced with the \\='vertico-timer-action-hint property of
the current value of `vertico-timer--action'."
  :type '(radio (const :tag "No Action Hint" nil) string)
  :group 'vertico-timer)

(defvar vertico-timer--action-hint-default-fstring
  (concat
   (propertize " | " 'face 'shadow)
   (propertize "⏲ " 'face 'font-lock-comment-face)
   (propertize "%s" 'face 'font-lock-constant-face)
   (propertize " | " 'face 'shadow))
  "Default action hint format.
Fallback for when the user has disabled the hint but wants to cycle actions.
Will be set during `vertico-timer--setup'.")

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
          (keymap-set map (single-key-description n) cmd))
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
    (keymap-set map "RET" #'vertico-timer-stop-exit)
    (keymap-set map "TAB" #'vertico-timer-stop-insert)
    (keymap-set map "i" #'vertico-timer-stop-insert)
    map)
  "Keymap for `vertico-timer-ticking' minor mode.")

(defvar-local vertico-timer-ticking-mode nil
  "Non-nil if timer is running.")

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
  (setq vertico-timer-ticking-mode t
        vertico-timer--exit-map-fn
        (set-transient-map vertico-timer-ticking-map
                           ;; KEEP-PRED
                           nil
                           ;; ON-EXIT
                           (lambda () (setq vertico-timer-ticking-mode nil)))))

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

(defun vertico-timer-prep-default-action ()
  "Prepare `vertico-timer--action' for the default action."
  (interactive)
  (vertico-timer--set-action vertico-timer-default-action))

(defun vertico-timer-stop-exit ()
  "Stop the timer and exit with default."
  (interactive)
  (vertico-timer-prep-default-action)
  (vertico-timer--run-action))

(defun vertico-timer-prep-insert-action ()
  "Prepare `vertico-timer--action' for the insert action."
  (interactive)
  (vertico-timer--set-action #'vertico-insert))

(defun vertico-timer-stop-insert ()
  "Stop the timer and insert the candidate."
  (interactive)
  (vertico-timer-prep-insert-action)
  (vertico-timer--run-action))

;; User-defined Actions

(defvar vertico-timer--user-registered-actions nil
  "Actions registered via `vertico-timer-register-action'.")

(defmacro vertico-timer-register-action (key cmd &optional name prep-key)
  "Bind CMD to KEY in `vertico-timer-ticking-map'.
KEY can be used to invoke CMD on the candidate before the timer runs out.

If NAME is provided it will be used as the suffix for the name of the
command bound in the map. Otherwise a name will be generated.
NAME will also show up in the action-hint overlay.

If PREP-KEY is provided an additional command will be bound `vertico-map'.
Invoking that command will set `vertico-timer--action' ahead of keypresses.

Use `vertico-timer-register-actions' to register multiple actions at once."
  (unless (key-valid-p (eval key))
    (error "KEY %s doesn't satisfy `key-valid-p'" key))
  (unless (or (not prep-key) (key-valid-p (eval prep-key)))
    (error "KEY %s doesn't satisfy `key-valid-p'" prep-key))
  (unless (or (not name) (stringp name))
    (error "Name must be a string or nil"))
  (let ((body `((interactive)
                (prefix-command-preserve-state)
                (vertico-timer--set-action #',cmd)
                (vertico-timer--run-action))))
    (let* ((suffix (or name (symbol-name (gensym))))
           (action-fn-sym
            (intern (concat "vertico-timer-action-" suffix)))
           (prep-fn-sym
            (intern (concat "vertico-timer-prep-action-" suffix))))
      `(progn
         (unless (memq #',cmd vertico-timer--user-registered-actions)
           (push #',cmd vertico-timer--user-registered-actions))
         ,(when prep-key
            `(defun ,prep-fn-sym ()
               (interactive)
               (vertico-timer--set-action #',cmd)))
         (defun ,action-fn-sym ()
           ,@body)
         (function-put ',cmd 'vertico-timer-action-hint
                       ,(or name (concat "cust-" suffix)))
         (keymap-set vertico-timer-ticking-map ,key
                     #',action-fn-sym)
         ,(when prep-key
            `(keymap-set vertico-map ,prep-key #',prep-fn-sym))))))

(defmacro vertico-timer-register-actions (&rest args)
  "Register multiple actions using key-cmd pairs.
ARGS should be an even number of KEY CMD pairs
each of which can be followed by keyword options:

  - ':name': The function bound in `vertico-timer-ticking-map'
     will be suffixed with the value of this option.
     If nil a lambda will be used instead. This is also the name
     to appear in the action hint.
  - ':prep-key': An additional command will be bound to this key
     in `vertico-map'. This command will allow to set
     `vertico-timer--action' before the timer starts."
  (unless (zerop (% (length args) 2))
    (error "Arguments must be in KEY CMD pairs (even number of arguments)"))
  (let (forms)
    (while args
      (let* ((key (pop args))
             (cmd (pop args))
             name prep-key)
        ;; Check if next argument is :name
        (when (and args (eq (car args) :name))
          (pop args) (setq name (pop args)))
        ;; Check if next argument is :prep-key
        (when (and args (eq (car args) :prep-key))
          (pop args) (setq prep-key (pop args)))
        (push `(vertico-timer-register-action ,key ,cmd ,name ,prep-key) forms)))
    `(progn ,@(nreverse forms))))


;;; Restore State

(defvar vertico-timer--state-to-restore nil
  "Alist holding original state to restore on teardown.
- `:vertico-indexed--commands': Original `vertico-indexed--commands'.
- `:vertico-indexed-mode': The orginal state of `vertico-indexed-mode'.
- `:vertico-map': Keymap with original bindings for keys we override.")

(defun vertico-timer--state-store ()
  "Prepare `vertico-timer--state-to-restore'."
  ;; Store original commands allowed by vertico-indexed-mode
  (push `(:vertico-indexed--commands . ,vertico-indexed--commands)
        vertico-timer--state-to-restore)
  ;; If original digit keybindings exist in vertico-map remeber it
  (dolist (prefix vertico-timer--prefixes)
    (when-let ((def (lookup-key vertico-map prefix)))
      (push (cons prefix def)
            (alist-get :vertico-map vertico-timer--state-to-restore))))
  ;; Store whether vertico-indexed-mode is active
  (setf (alist-get :vertico-indexed-mode vertico-timer--state-to-restore)
        vertico-indexed-mode))

(defun vertico-timer--state-restore ()
  "Restore state from `vertico-timer--state-to-restore'."
  ;; Restore vertico-indexed state
  (setq vertico-indexed--commands
        (alist-get :vertico-indexed--commands
                   vertico-timer--state-to-restore
                   '(vertico-exit vertico-directory-enter
                     vertico-insert)))
  ;; Unset vertico-timer bindings
  (mapc (lambda (prefix)
          (keymap-unset vertico-map prefix 'remove))
        vertico-timer--prefixes)
  ;; Restore previous bindings for digit keys if any
  (mapc
   (lambda (binding)
     (let ((key (car binding)) (def (cdr binding)))
       (keymap-set vertico-map key def)))
   (alist-get :vertico-map vertico-timer--state-to-restore))
  ;; Restore state of vertico-indexed-mode
  (unless (alist-get :vertico-indexed-mode vertico-timer--state-to-restore)
    (vertico-indexed-mode -1)))


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

(defun vertico-timer--setup ()
  "Hook up timer to digit keys in `vertico-map'."

  ;; Store relevant state
  (vertico-timer--state-store)

  ;; Ensure vertico-indexed is running
  (unless vertico-indexed-mode
    (vertico-indexed-mode 1))

  ;; Init Default Variables
  (unless vertico-timer-action-hint-fstring
    (setopt vertico-timer-action-hint-fstring
            vertico-timer--action-hint-default-fstring))

  ;; Ensure vertico-timer--first-digit passes vertico-indexed's filter
  (unless (memq 'vertico-timer--first-digit vertico-indexed--commands)
    (push 'vertico-timer--first-digit vertico-indexed--commands))

  ;; Set display hint for default actions
  (function-put vertico-timer-default-action
                'vertico-timer-action-hint vertico-timer-default-action-hint)
  (function-put 'vertico-insert
                'vertico-timer-action-hint vertico-timer-insert-action-hint)

  ;; Bind digit keys
  (vertico-timer--set-digit-keys vertico-map #'vertico-timer--first-digit))


(defun vertico-timer--teardown ()
  "Revert digit keys in `vertico-map'."

  (vertico-timer--state-restore)

  ;; Unset symbol properties
  (function-put vertico-timer-default-action
                'vertico-timer-action-hint nil)
  (function-put 'vertico-insert
                'vertico-timer-action-hint nil))


;;; Display Hint

;; The post-command-hook is only run after vertico-timer--run-action finishes.
;; Even if the minibuffer stays active then, the action will already be reset.
;; Hence perform the update of the action hint whenever it is set.
(defun vertico-timer--set-action (fn)
  "Change action to be executed to FN.
Updates action hint if enabled."
  (setq vertico-timer--action fn)
  (vertico-timer--exhibit))

(defvar-local vertico-timer--action-hint-fallbacks nil
  "Alist that provides fallbacks for the display of action hints.
Keys are symbols of actions, values strings.")

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
    (if-let ((hint (or (function-get vertico-timer--action
                                     'vertico-timer-action-hint)
                       ;; User cycles actions but has some hints disabled
                       (alist-get vertico-timer--action
                                  vertico-timer--action-hint-fallbacks))))
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

(defvar-local vertico-timer--actions nil
  "A ring with all actions available during cycling.")

(defun vertico-timer--actions-collect ()
  "Return ring of available actions."
  (let ((ring (make-ring (+ 2 (length vertico-timer--user-registered-actions)))))
    (ring-insert ring vertico-timer-default-action)
    (ring-insert ring #'vertico-insert)
    (dolist (action vertico-timer--user-registered-actions)
      (ring-insert ring action))
    ring))

(defun vertico-timer--cycle-actions (&optional reverse)
  "Cycle through registered actions starting from `vertico-timer--action'.
If REVERSE is non-nil reverse the direction."
  ;; Ensure action hint is shown even if user disabled it
  (setq-local vertico-timer-action-hint-fstring
              (or vertico-timer-action-hint-fstring
                  vertico-timer--action-hint-default-fstring))

  ;; Prepare fallback action hints in case the user disabled them
  (setq vertico-timer--action-hint-fallbacks
        `((,vertico-timer-default-action . "default")
          (vertico-insert . "insert")))

  ;; Unless successive cycle operation, collect all actions
  (unless vertico-timer--actions
    (setq vertico-timer--actions (vertico-timer--actions-collect)))

  (vertico-timer--set-action
   (if reverse
       (ring-previous vertico-timer--actions vertico-timer--action)
     (ring-next vertico-timer--actions vertico-timer--action))))

(defun vertico-timer-cycle-actions-forward ()
  "Cycle forward through registered actions."
  (interactive)
  (when vertico-timer-ticking-mode
    (prefix-command-preserve-state))
  (vertico-timer--cycle-actions))

(defun vertico-timer-cycle-actions-backward ()
  "Cycle backward through registered actions."
  (interactive)
  (when vertico-timer-ticking-mode
    (prefix-command-preserve-state))
  (vertico-timer--cycle-actions 'reverse))

(provide 'vertico-timer)
;;; vertico-timer.el ends here
