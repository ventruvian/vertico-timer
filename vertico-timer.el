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
;;  select candidates 'without' prefix arguments but digits directly.
;;  This is designed to be a faster alternative to be faster alternative
;;  to selecting a candidate with `vertico-next' and `vertico-previous'.
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

(defcustom vertico-timer-directory-enter-action-key "d"
  "The key to press to trigger `vertico-directory-enter'.
Defaults to \\='d\\='."
  :type 'key
  :group 'vertico-timer)

(defcustom vertico-timer-timeout-seconds 0.25
  "How many seconds to wait before running `vertico-timer--indexed-action'."
  :type 'number
  :group 'vertico-timer)

(defvar vertico-timer--prefixes '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  "What to index the candidates with.
Must be \\='(\"0\" \"1\" \"2\" \"3\" \"4\" \"5\" \"6\" \"7\" \"8\" \"9\").")

(defvar-local vertico-timer--indexed-action #'vertico-exit
  "Action to run after selecting a candidate with digit keys.")

(defvar vertico-timer--vertico-indexed-default-commands
  '(vertico-insert vertico-exit vertico-directory-enter)
  "Helper state for teardown. Same as `vertico-indexed-commands'.")

(defun vertico-timer--set-digit-keys (map cmd)
  "Set all digit keys in MAP to CMD."
  (mapc (lambda (n)
          (keymap-set map n cmd))
        vertico-timer--prefixes))

;;;###autoload
(define-minor-mode vertico-timer-mode
  "A temporary minor mode which restores the original bindings of digit keys."
  :keymap (let ((map (make-sparse-keymap)))
            (vertico-timer--set-digit-keys map #'digit-argument)
            map))

(defun vertico-timer--run-indexed-action ()
  "Call `vertico-timer--indexed-action' on the selection.
Disable `i-vertico/timer-mode' beforehand."
  (vertico-timer-mode -1)
  (setq last-command this-command
        this-command vertico-timer--indexed-action)
  ;; Run `vertico--prepare'
  (run-hooks 'pre-command-hook)
  (call-interactively vertico-timer--indexed-action)
  (run-hooks 'post-command-hook))

;; Marked with "vertico-" to be recognized by `vertico--prepare'.
(defun vertico-timer--select-index (&optional _)
  "In `vertico-indexed-mode' quick select a candidate.
Must be callled interactively with `digit-argument'."
  (interactive "P")
  (if (not vertico-indexed-mode)
      (call-interactively #'self-insert-command)
    (vertico-timer-mode 1)
    ;; Set prefix-arg and allow for subsequent invocations of `digit-argument'
    (call-interactively #'digit-argument)
    (run-at-time vertico-timer-timeout-seconds nil
                 #'vertico-timer--run-indexed-action)))

;;; Actions

(defmacro vertico-timer-register-action (key cmd &optional name)
  "Bind CMD to KEY in `vertico-timer-mode-map'.
KEY can be used to invoke CMD on the candidate before the timer runs out.

If NAME is provided it will be prefixed with `vertico-timer-prepare-action-'
and used as the command bound in the map. Otherwise a lambda function is used."
  (unless (key-valid-p (eval key))
    (error "KEY must satisfy `key-valid-p'"))
  (let ((body `((interactive)
                (setq vertico-timer--indexed-action #',cmd
                      prefix-arg current-prefix-arg
                      this-command 'repeat))))
    `(progn
       (unless (memq ',cmd vertico-indexed--commands) (push ',cmd vertico-indexed--commands))
       (keymap-set vertico-timer-mode-map ,key
                   ,(if name
                        `(defun ,(intern (format "vertico-timer-prepare-action-%s" name)) ()
                           ,@body)
                      `(lambda () ,@body))))))

(defmacro vertico-timer-register-actions (&rest args)
  "Register multiple actions using key-cmd pairs.
ARGS should be an even number of KEY CMD pairs.

If \\=':name name\\=' follows a CMD, a function named
\(format \"vertico-timer-prepare-action-%s\" name\) will be
bound to `vertico-timer-mode-map' otherwise a lambda is used."
  (unless (zerop (% (length args) 2))
    (error "Arguments must be in KEY CMD pairs (even number of arguments)"))
  (let (forms)
    (while args
      (let* ((key (pop args))
             (cmd (pop args))
             (name nil))
        ;; Check if next argument is :name
        (when (and args (eq (car args) :name))
          (pop args)
          (setq name (pop args)))
        (push `(vertico-timer-register-action ,key ,cmd ,name) forms)))
    `(progn ,@(nreverse forms))))

(vertico-timer-register-actions
 vertico-timer-exit-action-key vertico-exit :name "exit"
 vertico-timer-insert-action-key vertico-insert :name "insert"
 vertico-timer-directory-enter-action-key vertico-directory-enter :name "dir")

;;; Session Toggle

(defun vertico-timer-toggle-session ()
  "Toggle `vertico-indexed-mode' with timer for a single completion session."
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

;;; Global Mode

(defvar vertico-timer--original-digit-bindings (make-sparse-keymap)
  "Holds the user's original digit keybindings in `vertico-map' if any.
These keys are restored when `vertico-timer-global-mode' is disabled.")

(defun vertico-timer--setup ()
  "Hook up timer to digit keys in `vertico-map'."
  (map-keymap
   (lambda (key def)
     (let ((k (single-key-description key)))
       (when (member k vertico-timer--prefixes)
         (keymap-set vertico-timer--original-digit-bindings
                     k def))))
   vertico-map)
  (vertico-timer--set-digit-keys vertico-map #'vertico-timer--select-index))

(defun vertico-timer--teardown ()
  "Revert digit keys in `vertico-map'."
  ;; unset vertico-timer bindings
  (mapc (lambda (n)
          (keymap-unset vertico-map n 'remove))
        vertico-timer--prefixes)
  ;; restore previous bindings if any
  (map-keymap
   (lambda (key def) (keymap-set vertico-map (single-key-description key) def))
   vertico-timer--original-digit-bindings)
  ;; restore vertico-indexed functionality
  (setq vertico-indexed--commands vertico-timer--vertico-indexed-default-commands))

;;;###autoload
(define-minor-mode vertico-timer-global-mode
  "Select indexed candidates immediately."
  :group 'vertico-timer
  :init-value nil
  :lighter " vt"
  :global t
  (if vertico-timer-global-mode
      (vertico-timer--setup)
    (vertico-timer--teardown)))

(provide 'vertico-timer)
;;; vertico-timer.el ends here
