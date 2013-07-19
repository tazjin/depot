;; Configure package manager
(require 'package)

;; Add Marmalade repo
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; ... and melpa. Melpa packages that exist on marmalade will have
;; precendence.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Important defvar
(defvar my-pkgs '(haskell-mode
		  idle-highlight-mode
		  ido-ubiquitous
		  magit
		  paredit
		  smex
		  ace-jump-mode
		  clojure-mode
		  leuven-theme
		  magit
		  markdown-mode
		  nrepl
		  projectile
		  rainbow-delimiters
                  geiser
                  quack
		  )

  "A list of packages to install at launch.")

(dolist (p my-pkgs)
  (when (not (package-installed-p p))
    (package-install p)))


(load "~/.emacs.d/init-functions.el")

(add-to-list 'load-path "~/.emacs.d/scripts/")

(setq custom-file "~/.emacs.d/init-custom.el")
(load custom-file)

;; Other packages that need manual installation
(custom-download-script "https://raw.github.com/dimitri/switch-window/master/switch-window.el"
                        "switch-window.el")

(custom-download-script "https://raw.github.com/doitian/iy-go-to-char/master/iy-go-to-char.el"
                        "goto-char.el")

;; NYAN CAT!
(custom-clone-git "https://github.com/TeMPOraL/nyan-mode" "nyan-mode")
(load "~/.emacs.d/nyan-mode/nyan-mode.el")

(load "~/.emacs.d/init-settings.el")
(load "~/.emacs.d/init-modes.el")
(load "~/.emacs.d/init-bindings.el")

;; IRC configuration (erc)
;; Actual servers and such are loaded from irc.el
(load "~/.emacs.d/irc")

;; Seed RNG
(random t)

;; Start server for emacsclient
(server-start)
