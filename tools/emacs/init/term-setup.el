;; Utilities for Alacritty buffers.

(defun open-or-create-alacritty-buffer (buffer-name)
  "Switch to the buffer with BUFFER-NAME or create a
  new buffer running Alacritty."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (run-external-command "alacritty")
      (switch-to-buffer buffer))))

(defun is-alacritty-buffer (buffer)
  "Determine whether BUFFER runs Alacritty."
  (and (equal 'exwm-mode (buffer-local-value 'major-mode buffer))
       (s-starts-with? "Alacritty" (buffer-name buffer))))

(defun counsel-switch-to-alacritty ()
  "Switch to a (multi-)term buffer or create one."
  (interactive)
  (let ((terms (-map #'buffer-name
                     (-filter #'is-alacritty-buffer (buffer-list)))))
    (if terms
        (ivy-read "Switch to Alacritty buffer: "
                  (cons "New terminal" terms)
                  :caller 'counsel-switch-to-alacritty
                  :require-match t
                  :action #'open-or-create-alacritty-buffer)
      (run-external-command "alacritty"))))

(defun alacritty-rename ()
  "Rename the current terminal buffer."
  (interactive)
  (let* ((buffer (get-buffer (buffer-name))))
    (if (is-alacritty-buffer buffer)
        (rename-buffer (format "Alacritty<%s>" (read-string "New terminal name: ")))
      (error "This function is only intended to rename Alacritty buffers."))))

(provide 'term-setup)
