;;; init.el --- Package bootstrapping. -*- lexical-binding: t; -*-

;; Packages are installed via Nix configuration, this file only
;; initialises the newly loaded packages.

(require 'use-package)
(require 'seq)

(package-initialize)

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
  :init (setq company-tooltip-align-annotations t))

(use-package dash)
(use-package dash-functional)
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

(use-package erlang
  :hook ((erlang-mode . (lambda ()
                          ;; Don't indent after '>' while I'm writing
                          (local-set-key ">" 'self-insert-command)))))

(use-package f)

(use-package go-mode
  :bind (:map go-mode-map ("C-c C-r" . recompile))
  :hook ((go-mode . (lambda ()
                      (setq tab-width 2)
                      (setq-local compile-command
                                  (concat "go build " buffer-file-name))))))

(use-package haskell-mode)

(use-package jq-mode
  :init (add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode)))

(use-package kotlin-mode
  :hook ((kotlin-mode . (lambda ()
                          (setq indent-line-function #'indent-relative)))))

(use-package lsp-mode)

(use-package markdown-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package markdown-toc)

(use-package nix-mode
  :hook ((nix-mode . (lambda ()
                       (setq indent-line-function #'nix-indent-line)))))

(use-package nginx-mode)
(use-package rust-mode)
(use-package terraform-mode)
(use-package toml-mode)
(use-package web-mode)
(use-package yaml-mode)

;; Configuration changes in `customize` can not actually be persisted
;; to the customise file that Emacs is currently using (since it comes
;; from the Nix store).
;;
;; The way this will work for now is that Emacs will *write*
;; configuration to the file tracked in my repository, while not
;; actually *reading* it from there (unless Emacs is rebuilt).
(setq custom-file (expand-file-name "~/depot/tools/emacs/config/custom.el"))
(load-library "custom")

(defvar home-dir (expand-file-name "~"))

;; Seed RNG
(random t)

;; Load all other Emacs configuration. These configurations are
;; added to `load-path' by Nix.
(mapc 'require '(desktop
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

;; If a local configuration file exists, it should be loaded. No
;; other configuration comes from `user-emacs-directory'.
(let ((local-file (expand-file-name (f-join user-emacs-directory "local.el"))))
  (when (f-exists? local-file)
    (load local-file)))

(provide 'init)
