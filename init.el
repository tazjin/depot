;; Emacs 24 or higher!
(when (< emacs-major-version 24)
  (error "This setup requires Emacs v24, or higher. You have: v%d" emacs-major-version))

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
  '(; Basic functionality
    ace-jump-mode
    ack-and-a-half
    browse-kill-ring
    confluence
    dash
    flx-ido
    flycheck
    idle-highlight-mode
    ido-ubiquitous
    iy-go-to-char
    magit
    multiple-cursors
    nyan-mode
    paredit
    projectile
    puppet-mode
    rainbow-delimiters
    rainbow-mode
    s
    smex
    smart-mode-line
    switch-window
    undo-tree

    ; Clojure
    ac-cider-compliment
    cider
    clojure-mode
)
  "A list of packages to install at launch.")

(defvar evil-pkgs
  '(evil
    evil-leader
    evil-tabs
    evil-paredit
    key-chord
    surround)
  "Evil related packages")

(dolist (p my-pkgs)
  (when (not (package-installed-p p))
    (package-install p)))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Or on Linux?
(setq is-linux (equal system-type 'gnu/linux))

;; Is this being used by a vim user?
(setq is-vim-mode nil)

;; What's the home folder?
(defvar home-dir)
(setq home-dir (expand-file-name "~"))

(when is-vim-mode
  (dolist (p evil-pkgs)
    (when (not (package-installed-p p))
      (package-install p))))

(add-to-list 'load-path user-emacs-directory)

(mapc 'require '(init-functions
                 init-settings
                 init-modes
                 init-bindings
                 init-eshell))

(when is-vim-mode
  (require 'init-evil))

(add-to-list 'load-path "~/.emacs.d/scripts/")

(setq custom-file "~/.emacs.d/init-custom.el")
(load custom-file)

;; A file with machine specific settings.
;(load-file-if-exists (concat home-dir "/.emacs.d/init-local.el"))

;; IRC configuration
;; Actual servers and such are loaded from irc.el
; (load-file-if-exists (concat home-dir "/.emacs.d/init-irc.el"))

;; Load magnars' string manipulation library
(require 's)

(require 'ack-and-a-half)

;; Seed RNG
(random t)

;; Start server for emacsclient
;; (server-start)
