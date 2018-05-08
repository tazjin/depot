;; Configuration for multi-term mode:

(require 'multi-term)

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

(global-set-key (kbd "C-x t") #'counsel-switch-to-term)

;; term-mode's attempt to use isearch is not my favourite thing in the
;; world.
(delete '("C-r" . isearch-backward) term-bind-key-alist)
(delete '("C-s" . isearch-forward) term-bind-key-alist)

(add-to-list 'term-bind-key-alist '("C-r" . term-send-reverse-search-history))
(add-to-list 'term-bind-key-alist '("C-c C-l" . term-line-mode))
(add-to-list 'term-bind-key-alist '("C-s" . swiper))
(add-to-list 'term-bind-key-alist '("C-c C-r" . term-rename))

(provide 'term-setup)
