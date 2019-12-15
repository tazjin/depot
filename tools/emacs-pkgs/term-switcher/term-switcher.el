;;; term-switcher.el --- Easily switch between open X11 terminals
;;
;; Copyright (C) 2019 Google Inc.
;;
;; Author: Vincent Ambo <tazjin@google.com>
;; Version: 1.0
;; Package-Requires: (dash ivy s)
;;
;;; Commentary:
;;
;; This package adds a function that lets users quickly switch between
;; different open X11 terminals using ivy.
;;
;; It is primarily intended to be used by EXWM users who use graphical
;; terminals inside of Emacs.
;;
;; Users MUST configure the group `term-switcher' and can then bind
;; `ts/switch-to-terminal' to an appropriate key.

(require 'dash)
(require 'ivy)
(require 's)

(defgroup term-switcher nil
  "Customization options for configuring `term-switcher' with the
  user's terminal emulator of choice.")

(defcustom term-switcher-program "gnome-terminal"
  "X11 terminal application to use."
  :type '(string)
  :group 'term-switcher)

(defcustom term-switcher-buffer-prefix "Term"
  "String prefix for X11 terminal buffers. For example, if your
  EXWM configuration renames X11 terminal buffers to
  `Term</foo/bar>' you might want to use `Term' as the matching
  prefix."
  :type '(string)
  :group 'term-switcher)

(defun ts/run-terminal-program ()
  (message "Starting %s..." term-switcher-program)
  (start-process-shell-command term-switcher-program nil term-switcher-program))

(defun ts/open-or-create-terminal-buffer (buffer-name)
  "Switch to the buffer with BUFFER-NAME or create a new buffer
  running the configured X11 terminal."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (ts/run-terminal-program)
      (switch-to-buffer buffer))))

(defun ts/is-terminal-buffer (buffer)
  "Determine whether BUFFER runs an X11 terminal."
  (and (equal 'exwm-mode (buffer-local-value 'major-mode buffer))
       (s-starts-with? term-switcher-buffer-prefix (buffer-name buffer))))

(defun ts/switch-to-terminal ()
  "Switch to an X11 terminal buffer, or create a new one."
  (interactive)
  (let ((terms (-map #'buffer-name
                     (-filter #'ts/is-terminal-buffer (buffer-list)))))
    (if terms
        (ivy-read "Switch to terminal buffer: "
                  (cons "New terminal" terms)
                  :caller 'ts/switch-to-terminal
                  :preselect (s-concat "^" term-switcher-buffer-prefix)
                  :require-match t
                  :action #'ts/open-or-create-terminal-buffer)
      (ts/run-terminal-program))))

(provide 'term-switcher)
