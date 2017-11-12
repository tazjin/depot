;;; init.el --- Package bootstrapping. -*- lexical-binding: t; -*-

;; This file bootstraps the Emacs setup by going through package installations.
;; After all packages are installed, local configuration is loaded.

(require 'package)
(require 'seq)

;; Configure Marmalade and MELPA repositories. Packages available on Marmalade
;; will have precedence.
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; This variable controls all packages that should be installed.
(setq-local desired-packages
  '(;; elisp libraries
    dash
    dash-functional
    ht
    s

    ;; editor packages
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
    magit
    markdown-mode+
    multi-term
    multiple-cursors
    nix-mode
    paredit
    password-store
    pg
    pkgbuild-mode
    puppet-mode
    racer
    rainbow-delimiters
    rainbow-mode
    restclient
    rust-mode
    smart-mode-line
    switch-window
    terraform-mode
    undo-tree
    uuidgen
    yaml-mode
    ))

(defun installable-packages (pkg-list)
  "Filter out not-yet installed packages from package list."
  (seq-filter (lambda (p) (not (package-installed-p p))) pkg-list))

(defun install-needed-packages (pkg-list)
  (let ((to-install (installable-packages pkg-list)))
    (if (< 0 (length to-install))
        (progn (package-refresh-contents)
               (mapcar #'package-install to-install))
      (message "No new packages to install."))))

;; Run package installation!
(install-needed-packages desired-packages)

;; Configure a few basics before moving on to package-specific initialisation.
(setq custom-file (concat user-emacs-directory "init/custom.el"))
(load custom-file)

(defvar home-dir)
(setq home-dir (expand-file-name "~"))

;; Seed RNG
(random t)

;; Add 'init' folder that contains other settings to load.
(add-to-list 'load-path (concat user-emacs-directory "init"))

;; Load configuration that makes use of installed packages:


;; Emacs will automatically initialise all installed packages.
;; After initialisation, proceed to load configuration that requires packages:
(defun load-other-settings ()
  (mapc 'require '(look-and-feel
                   functions
                   settings
                   modes
                   bindings
                   eshell-setup
                   haskell-setup
                   rust-setup
                   )))

(add-hook 'after-init-hook 'load-other-settings)
