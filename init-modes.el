(mapc 'require '(projectile hi2))
;; Initializes modes I use.

(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)
(add-hook 'prog-mode-hook 'idle-highlight-mode)

;; Configure markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Add keybindings to move nested blocks with C-, rsp. C-.
(define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
(define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)

;; Configure nrepl (Clojure REPL) and clojure-mode
;; Paredit in clojure

(add-hook 'clojure-mode-hook 'paredit-mode)

;; eldoc in clojure
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

;; Don't annoy me
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces nil)

;; Enable projectile for all things programming
(add-hook 'prog-mode-hook 'projectile-on)

;; Enable rainbow-delimiters for all things programming
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Enable Paredit in Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; Always highlight matching brackets
(show-paren-mode 1)

;; Undo-Tree at all times!
(undo-tree-mode)

;; Keep track of recent files
(recentf-mode)

;; Enable Nyan mode
(nyan-mode 1)
