;; ## Generic settings ##

(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Code:

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; can't do it at launch or emacsclient won't always honor it
(add-hook 'before-make-frame-hook 'esk-turn-off-tool-bar)

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

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

;; Theme!
(custom-download-theme "https://raw.github.com/owainlewis/emacs-color-themes/master/themes/hickey-theme.el"
                       "hickey-theme.el")

(load-theme 'hickey t)

;; Hiding JOIN, QUIT, PART
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Enable projectile for all things programming
(add-hook 'prog-mode-hook 'projectile-on)

;; Enable rainbow-delimiters for all things programming
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Enable paredit in all Lisps
(add-hook 'lisp-mode-hook 'paredit-mode)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;; ## Mac specific settings ##

(setq browse-url-browser-function 'browse-default-macosx-browser)

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

;; ... and then enable fullscreen. (This requires a nightly build of
;; Emacs for OS X)
;;(toggle-frame-fullscreen)

;; ## Navigation and key bindings ##

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
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

;; Eshell
;; Start/join
(global-set-key (kbd "C-x m") 'eshell)
;; Always start
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Git
(global-set-key (kbd "C-c g") 'magit-status)

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
