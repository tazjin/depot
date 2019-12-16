;;; term-switcher.el --- Easily switch between open vterms
;;
;; Copyright (C) 2019 Google Inc.
;;
;; Author: Vincent Ambo <tazjin@google.com>
;; Version: 1.1
;; Package-Requires: (dash ivy s)
;;
;;; Commentary:
;;
;; This package adds a function that lets users quickly switch between
;; different open vterms via ivy.

(require 'dash)
(require 'ivy)
(require 's)

(defgroup term-switcher nil
  "Customization options `term-switcher'.")

(defcustom term-switcher-buffer-prefix "vterm<"
  "String prefix for vterm terminal buffers. For example, if you
  set your titles to match `vterm<...>' a useful prefix might be
  `vterm<'."
  :type '(string)
  :group 'term-switcher)

(defun ts/open-or-create-vterm (buffer-name)
  "Switch to the buffer with BUFFER-NAME or create a new vterm
  buffer."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (vterm)
      (switch-to-buffer buffer))))

(defun ts/is-vterm-buffer (buffer)
  "Determine whether BUFFER runs a vterm."
  (equal 'vterm-mode (buffer-local-value 'major-mode buffer)))

(defun ts/switch-to-terminal ()
  "Switch to an existing vterm buffer or create a new one."

  (interactive)
  (let ((terms (-map #'buffer-name
                     (-filter #'ts/is-vterm-buffer (buffer-list)))))
    (if terms
        (ivy-read "Switch to vterm: "
                  (cons "New vterm" terms)
                  :caller 'ts/switch-to-terminal
                  :preselect (s-concat "^" term-switcher-buffer-prefix)
                  :require-match t
                  :action #'ts/open-or-create-vterm)
      (vterm))))

(provide 'term-switcher)
