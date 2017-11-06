;; Configure package manager
(require 'package)
(package-initialize)

;; Add Marmalade repo
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

;; ... and melpa. Melpa packages that exist on marmalade will have
;; precendence.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; And load things!
(package-refresh-contents)

(defvar my-pkgs
  '(;; All packages
    ace-jump-mode
    ag
    browse-kill-ring
    cargo
    confluence
    dash
    dockerfile-mode
    erlang
    flycheck
    go-mode
    gruber-darker-theme
    haskell-mode
    helm
    hi2
    idle-highlight-mode
    iy-go-to-char
    magit
    markdown-mode+
    multiple-cursors
    multi-term
    nix-mode
    paredit
    password-store
    pkgbuild-mode
    puppet-mode
    racer
    rainbow-delimiters
    rainbow-mode
    rust-mode
    smart-mode-line
    switch-window
    terraform-mode
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
                 haskell-setup
                 ))

(add-to-list 'load-path (concat user-emacs-directory "scripts"))

(setq custom-file (concat user-emacs-directory "init/custom.el"))
(load custom-file)

;; Local configuration
(load-file-if-exists "~/.emacs.d/init/local.el")

;; Load magnars' string manipulation library
(require 's)

;; Seed RNG
(random t)

(put 'upcase-region 'disabled nil)

;; Configure smart mode line
(sml/setup)
