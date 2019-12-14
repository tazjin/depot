;; Utilities for X11 terminal buffers.

(defvar x11-terminal-program "gnome-terminal"
  "Which X11 terminal application to use.")

(defvar x11-terminal-buffer-prefix "Term"
  "String prefix for X11 terminal buffer names.")

(defun open-or-create-terminal-buffer (buffer-name)
  "Switch to the buffer with BUFFER-NAME or create a new buffer
  running the configured X11 terminal."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (run-external-command x11-terminal-program)
      (switch-to-buffer buffer))))

(defun is-terminal-buffer (buffer)
  "Determine whether BUFFER runs an X11 terminal."
  (and (equal 'exwm-mode (buffer-local-value 'major-mode buffer))
       (s-starts-with? x11-terminal-buffer-prefix (buffer-name buffer))))

(defun counsel-switch-to-terminal ()
  "Switch to an X11 terminal buffer, or create a new one."
  (interactive)
  (let ((terms (-map #'buffer-name
                     (-filter #'is-terminal-buffer (buffer-list)))))
    (if terms
        (ivy-read "Switch to terminal buffer: "
                  (cons "New terminal" terms)
                  :caller 'counsel-switch-to-terminal
                  :preselect (s-concat "^" x11-terminal-buffer-prefix)
                  :require-match t
                  :action #'open-or-create-terminal-buffer)
      (run-external-command x11-terminal-program))))

(provide 'term-setup)
