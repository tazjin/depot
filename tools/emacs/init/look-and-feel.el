;;; -*- lexical-binding: t; -*-

;; Hide those ugly tool bars:
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(add-hook 'after-make-frame-functions
          (lambda (frame) (scroll-bar-mode 0)))

;; Don't do any annoying things:
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")

;; Remember layout changes
(winner-mode 1)

;; Usually emacs will run as a proper GUI application, in which case a few
;; extra settings are nice-to-have:
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Configure editor fonts
(let ((font (format "Input Mono-%d" 12)))
  (setq default-frame-alist `((font-backend . "xft")
                              (font . ,font)))
  (set-frame-font font t t))

;; Display battery in mode-line's misc section on adho:
(when (equal "adho" (system-name))
  (setq battery-mode-line-format " %b%p%%")
  (display-battery-mode))

;; Configure telephone-line
(defun telephone-misc-if-last-window ()
  "Renders the mode-line-misc-info string for display in the
  mode-line if the currently active window is the last one in the
  frame.

  The idea is to not display information like the current time,
  load, battery levels in all buffers."

  (when (bottom-right-window-p)
      (telephone-line-raw mode-line-misc-info t)))

(defun telephone-line-setup ()
  (telephone-line-defsegment telephone-line-last-window-segment ()
    (telephone-misc-if-last-window))

  ;; Display the current EXWM workspace index in the mode-line
  (telephone-line-defsegment telephone-line-exwm-workspace-index ()
    (when (bottom-right-window-p)
      (format "[%s]" exwm-workspace-current-index)))

  ;; Define a highlight font for ~ important ~ information in the last
  ;; window.
  (defface special-highlight '((t (:foreground "white" :background "#5f627f"))) "")
  (add-to-list 'telephone-line-faces
               '(highlight . (special-highlight . special-highlight)))

  (setq telephone-line-lhs
        '((nil . (telephone-line-position-segment))
          (accent . (telephone-line-buffer-segment))))

  (setq telephone-line-rhs
        '((accent . (telephone-line-major-mode-segment))
          (nil . (telephone-line-last-window-segment
                  telephone-line-exwm-workspace-index))
          (highlight . (telephone-line-notmuch-counts))))

  (setq telephone-line-primary-left-separator 'telephone-line-tan-left
        telephone-line-primary-right-separator 'telephone-line-tan-right
        telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
        telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)

  (telephone-line-mode 1))

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
