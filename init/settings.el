(require 'uniquify)

; ## Generic settings ##

;; Make Helm go!
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

; Hide those ugly tool bars
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

(defun disable-scroll-bar ()
  (scroll-bar-mode 0))

; And remember to do it if I create a new frame.
(add-hook 'before-make-frame-hook 'disable-scroll-bar)

;; Don't make any noises, don't flash, just leave me alone
(setq ring-bell-function 'ignore)

;; Go away go away
(setq initial-scratch-message "")

(setq gc-cons-threshold 20000000)

(setq uniquify-buffer-name-style 'forward)

;; Give me column numbers
(column-number-mode t)

;; Bash is the reliable one here
(setq multi-term-program "/bin/bash")

;; Use the GPG-provided SSH agent
(setenv "SSH_AUTH_SOCK" (concat (getenv "HOME") "/.gnupg/S.gpg-agent.ssh"))

;;; Code:

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/bin"))
;; Stack installs here:
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

; Fix some defaults
(setq visible-bell nil
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      default-directory "~"
      fill-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `((,(concat user-emacs-directory "backups")))
      diff-switches "-u")

;; Fix keys on Linux
(if is-linux
    (setq x-super-keysym 'meta
          x-alt-keysym 'alt))

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

(set-default 'indent-tabs-mode nil)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(require 'ffap)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; ## Look and feel ##

;; Themes!
(global-hl-line-mode -1)

(setq default-frame-alist '((font-backend . "xft")
                            (font . "Input Mono-12")))

(set-default-font "Input Mono 12")

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

;; Use clipboard properly
(setq x-select-enable-clipboard t)

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

;; Display tabs as 4 spaces
(setq default-tab-width 4)

;; Set up Java home & path
(setenv "JAVA_HOME" "/usr/lib/jvm/default")

;; Use CUPS
(setq lpr-command "xpp")

;; Allow same window in two frames
(setq ido-default-buffer-method 'selected-window)

(provide 'settings)
