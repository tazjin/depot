(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Important packages
(defvar my-pkgs '(starter-kit starter-kit-bindings haskell-mode magit color-theme-solarized)
  "A list of packages to install at launch.")

(dolist (p my-pkgs)
  (when (not (package-installed-p p))
    (package-install p)))

;; Set solarized theme
(load-theme 'solarized-dark t)

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

;; Configure haskell-mode
;; Enable semi-automatic indentation and font-locking
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)

;; Add keybindings to move nested blocks with C-, rsp. C-.
(define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
(define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
