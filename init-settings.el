(require 'uniquify)
;; ## Generic settings ##

(tool-bar-mode -1)
(scroll-bar-mode -1)

(flx-ido-mode 1)
(setq ido-use-faces nil)
(setq gc-cons-threshold 20000000)

(setq uniquify-buffer-name-style 'forward)

;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path "/Applications/Racket/bin")


(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      fill-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

(set-default 'indent-tabs-mode nil)

;; ido-mode is like magic pixie dust!
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; Swedish!
(set-language-environment 'Swedish)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(require 'ffap)
(defvar ffap-c-commment-regexp "^/\\*+"
  "Matches an opening C-style comment, like \"/***\".")

(defadvice ffap-file-at-point (after avoid-c-comments activate)
  "Don't return paths like \"/******\" unless they actually exist.

This fixes the bug where ido would try to suggest a C-style
comment as a filename."
  (ignore-errors
    (when (and ad-return-value
               (string-match-p ffap-c-commment-regexp
                               ad-return-value)
               (not (ffap-file-exists-string ad-return-value)))
      (setq ad-return-value nil))))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; Hippie expand: at times perhaps too hip
(eval-after-load 'hippie-exp
  '(progn
     (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
       (delete f hippie-expand-try-functions-list))
     
     ;; Add this back in at the end of the list.
     (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)))

;; ## Look and feel ##

;; Themes! I download and install the ones I like and default the one
;; I currently like most. This changes a lot because I hate
;; everything. (It's in my nature, don't judge)
(custom-download-theme "https://raw.github.com/owainlewis/emacs-color-themes/master/themes/hickey-theme.el"
                       "hickey-theme.el")

(custom-download-theme "https://raw.github.com/rexim/gruber-darker-theme/master/gruber-darker-theme.el"
                       "gruber-darker-theme.el")

(load-theme 'gruber-darker t)

(global-hl-line-mode -1)

(set-default-font "Source Code Pro 13")

;; Don't make the nyan cat too long ... I have other stuff in the mode
;; bar as well!
(set-variable 'nyan-bar-length 15)
;; Not the real deal without this ...
(set-variable 'nyan-wavy-trail t)

;; Style line numbers (shown with M-g g)
(setq linum-format (lambda (line)
                     (propertize
                      (format (concat " %"
                                      (number-to-string
                                       (length (number-to-string
                                                (line-number-at-pos (point-max)))))
                                      "d ")
                              line)
                      'face 'linum)))

;; Hiding JOIN, QUIT, PART
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;; ## Mac specific settings ##

;; Enable mouse support on OS X
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))

  (setq mouse-sel-mode t)
)

;; Use clipboard properly
(setq x-select-enable-clipboard t)

;; Settings for Emacs.app (Cocoa Emacs)
;; Menu bar doesn't take up additional space, so lets use it.
(menu-bar-mode 1)

;; Don't use Apple's native fullscreen (FIXME: Change with Mavericks)
(setq ns-use-native-fullscreen nil)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Make emacs behave sanely (overwrite selected text)
(delete-selection-mode 1)

(defun toggle-native-fullscreen ()
  "Toggles between native and non-native OS X fullscreen"
  (interactive)
  (setq ns-use-native-fullscreen (not ns-use-native-fullscreen)))

;; ## Navigation and key bindings ##

(setq windmove-wrap-around t)

;; Load ace-jump-mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  )

(define-key global-map [?] 'ace-jump-mode)

;; Quick jump back 
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  )

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x ö") 'ace-jump-mode-pop-mark)

;; Keep your backup files in tmp, emacs!
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Eshell
;; Start/join
(global-set-key (kbd "C-x m") 'eshell)
;; Always start
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Git
(global-set-key (kbd "C-c g") 'magit-status)

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(provide 'init-settings)
