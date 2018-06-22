;; Utilities for term-mode

(defun open-or-create-term-buffer (buffer-name)
  "Switch to the buffer with BUFFER-NAME or create a
  new (multi-)term-mode buffer."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (multi-term)
      (switch-to-buffer buffer))))

(defun counsel-switch-to-term ()
  "Switch to a (multi-)term buffer or create one."
  (interactive)
  (let ((terms (counsel-list-buffers-with-mode 'term-mode)))
    (if terms
        (ivy-read "Switch to term buffer: "
                  (cons "New terminal" terms)
                  :caller 'counsel-switch-to-term
                  :require-match t
                  :action #'open-or-create-term-buffer)
      (multi-term))))

(defun term-rename ()
  "Rename the current terminal buffer."
  (interactive)
  (let* ((buffer (get-buffer (buffer-name)))
         (mode (buffer-local-value 'major-mode buffer)))
    (if (equal 'term-mode mode)
        (rename-buffer (format "*terminal<%s>*" (read-string "New terminal name: ")))
      (error "This function is only intended to rename terminal buffers."))))

(provide 'term-setup)
