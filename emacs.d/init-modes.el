(mapc 'require '(projectile))
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

;; Configure haskell-mode
;; Enable semi-automatic indentation and font-locking
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)

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

;; Enable paredit in all programming buffers
(add-hook 'prog-mode-hook 'paredit-mode)
