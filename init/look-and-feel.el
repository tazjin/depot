;;; -*- lexical-binding: t; -*-

;; Hide those ugly tool bars:
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; Don't do any annoying things:
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")

;; Usually emacs will run as a proper GUI application, in which case a few
;; extra settings are nice-to-have:
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Configure editor fonts
(setq default-frame-alist '((font-backend . "xft")
                            (font . "Input Mono-12")))

(set-frame-font "Input Mono 12" t t)

(defun configure-new-frame (frame)
  "Configuration settings to run whenever a new frame is created."
  (scroll-bar-mode 0) ; Disable visual scroll bar (ugh!)
  (set-frame-parameter frame 'alpha '(97 . 95))) ; Transparent emacs!

(add-hook 'after-make-frame-functions 'configure-new-frame)

;; If this file is evaluating there may also be a new (initial) frame:
(configure-new-frame (selected-frame))

;; Configure smart mode line
(sml/setup)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Use clipboard properly
(setq select-enable-clipboard t)

;; Show in-progress chords in minibuffer
(setq echo-keystrokes 0.1)

;; Show column numbers in all buffers
(column-number-mode t)

;; Highlight currently active line
(global-hl-line-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; Style line numbers (shown with M-g g)
(setq linum-format
      (lambda (line)
        (propertize
         (format (concat " %"
                         (number-to-string
                          (length (number-to-string
                                   (line-number-at-pos (point-max)))))
                         "d ")
                 line)
         'face 'linum)))

;; Display tabs as 2 spaces
(setq tab-width 2)

;; Don't wrap around when moving between buffers
(setq windmove-wrap-around nil)

(provide 'look-and-feel)

