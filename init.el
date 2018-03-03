;;; init.el --- Package bootstrapping. -*- lexical-binding: t; -*-

;; Packages are installed via Nix configuration, this file only
;; initialises the newly loaded packages.

(require 'package)
(require 'seq)

(package-initialize)

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
  (mapc 'require '(nixos
                   look-and-feel
                   functions
                   settings
                   modes
                   bindings
                   eshell-setup
                   haskell-setup
                   rust-setup
                   lisp-setup
                   )))

(add-hook 'after-init-hook 'load-other-settings)
(put 'narrow-to-region 'disabled nil)
