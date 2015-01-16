;; Configure package manager
(require 'package)

;; Add Marmalade repo
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; ... and melpa. Melpa packages that exist on marmalade will have
;; precendence.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; And load things!
(package-refresh-contents)
(package-initialize)

(defvar my-pkgs
  '(;; All packages
    ac-cider-compliment
    ace-jump-mode
    ack-and-a-half
    browse-kill-ring
    cider
    clojure-mode
    confluence
    dash
    dockerfile-mode
    erlang
    flx-ido
    flycheck
    go-mode
    haskell-mode
    hi2
    idle-highlight-mode
    ido-ubiquitous
    iy-go-to-char
    magit
    markdown-mode+
    multiple-cursors
    mvn
    nyan-mode
    paredit
    password-store
    pkgbuild-mode
    projectile
    puppet-mode
    rainbow-delimiters
    rainbow-mode
    rust-mode
    s
    smart-mode-line
    smex
    switch-window
    undo-tree
    yaml-mode
    )
  "A list of packages to install at launch.")

(dolist (p my-pkgs)
  (when (not (package-installed-p p))
    (package-install p)))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Or on Linux?
(setq is-linux (equal system-type 'gnu/linux))

;; What's the home folder?
(defvar home-dir)
(setq home-dir (expand-file-name "~"))

(add-to-list 'load-path (concat user-emacs-directory "init"))

(mapc 'require '(functions
                 settings
                 modes
                 bindings
                 eshell-setup
                 clojure
                 haskell-setup
                 ))

(add-to-list 'load-path (concat user-emacs-directory "scripts"))

(setq custom-file (concat user-emacs-directory "init/custom.el"))
(load custom-file)

;; Load magnars' string manipulation library
(require 's)

(require 'ack-and-a-half)

;; Seed RNG
(random t)

;; SML should respect theme colours
;; (setq sml/theme 'black)
(sml/setup)
