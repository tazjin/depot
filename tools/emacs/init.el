;;; init.el --- Package bootstrapping. -*- lexical-binding: t; -*-

;; Packages are installed via Nix configuration, this file only
;; initialises the newly loaded packages.

(require 'use-package)
(require 'seq)

(package-initialize)

;; Add 'init' folder that contains other settings to load.
(add-to-list 'load-path (concat user-emacs-directory "init"))

;; Initialise all packages installed via Nix.
;;
;; TODO: Generate this section in Nix for all packages that do not
;; require special configuration.

;;
;; Packages providing generic functionality.
;;

(use-package ace-window
  :bind (("C-x o" . ace-window))
  :init
  (setq aw-keys '(?f ?j ?d ?k ?s ?l ?a)
        aw-scope 'frame))

(use-package auth-source-pass :init (auth-source-pass-enable))

(use-package avy
  :bind (("M-j" . avy-goto-char)
         ("M-p" . avy-pop-mark)
         ("M-g g" . avy-goto-line)))

(use-package browse-kill-ring)

(use-package company
  :hook ((prog-mode . company-mode))
  :bind (:map rust-mode-map ("<tab>" . company-indent-or-complete-common)
         :map lisp-mode-map ("<tab>" . company-indent-or-complete-common))
  :init (setq company-tooltip-align-annotations t))

(use-package dash)
(use-package dash-functional)
(use-package edit-server :init (edit-server-start))
(use-package gruber-darker-theme)
(use-package ht)
(use-package hydra)
(use-package idle-highlight-mode :hook ((prog-mode . idle-highlight-mode)))
(use-package paredit :hook ((lisp-mode . paredit-mode)
                            (emacs-lisp-mode . paredit-mode)))
(use-package multiple-cursors)
(use-package pinentry
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode)
(use-package s)
(use-package smartparens :init (smartparens-global-mode))
(use-package string-edit)
(use-package telephone-line) ;; configuration happens outside of use-package
(use-package undo-tree :init (global-undo-tree-mode))
(use-package uuidgen)
(use-package which-key :init (which-key-mode t))

;;
;; Applications in emacs
;;

(use-package magit
  :bind ("C-c g" . magit-status)
  :init (setq magit-repository-directories '(("/home/vincent/projects" . 2))))

(use-package password-store)
(use-package pg)
(use-package restclient)

;;
;; Packages providing language-specific functionality
;;

(use-package cargo
  :hook ((rust-mode . cargo-minor-mode)
         (cargo-process-mode . visual-line-mode))
  :bind (:map cargo-minor-mode-map ("C-c C-c C-l" . ignore)))

(use-package dockerfile-mode)

(use-package eglot
  :init (defvar rust-eglot-initialized nil)
  :hook ((rust-mode . (lambda ()
                        (unless rust-eglot-initialized
                          (call-interactively #'eglot)
                          (setq rust-eglot-initialized t))))))

(use-package erlang
  :hook ((erlang-mode . (lambda ()
                          ;; Don't indent after '>' while I'm writing
                          (local-set-key ">" 'self-insert-command)))))

(use-package go-mode)
(use-package haskell-mode)

(use-package jq-mode
  :init (add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode)))

(use-package kotlin-mode
  :bind (:map kotlin-mode-map ("<tab>" . indent-relative)))

(use-package markdown-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package markdown-toc)

(use-package nix-mode
  :bind (:map nix-mode-map ("<tab>" . nix-indent-line)))

(use-package nginx-mode)
(use-package rust-mode)
(use-package terraform-mode)
(use-package toml-mode)
(use-package web-mode)
(use-package yaml-mode)

;;
;; EXWM / NixOS related packages
;;

;; Configure a few basics before moving on to package-specific initialisation.
(setq custom-file (concat user-emacs-directory "init/custom.el"))
(load custom-file)

(defvar home-dir (expand-file-name "~"))

;; Seed RNG
(random t)

(defun load-other-settings ()
  (mapc 'require '(nixos
		   mail-setup
                   look-and-feel
                   functions
                   settings
                   modes
                   bindings
                   term-setup
                   eshell-setup))
  (telephone-line-setup)
  (ace-window-display-mode)

  (use-package sly
    :init (setq inferior-lisp-program (concat (nix-store-path "sbcl") "/bin/sbcl"))
    ;;(add-to-list 'company-backends 'sly-company)
    ))


;; Some packages can only be initialised after the rest of the
;; settings has been applied:

(add-hook 'after-init-hook 'load-other-settings)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
