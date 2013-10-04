(mapc 'require '(projectile hi2 ac-nrepl))
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

;; Use auto-complete as completion at point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)


;; Configure nrepl (Clojure REPL) and clojure-mode

(defun nrepl-mode-setup ()
  "Activates paredit, rainbow delimiters and ac-nrepl"
  (ac-nrepl-setup)
  (rainbow-delimiters-mode)
  (paredit-mode))

;; Use ac-nrepl for completion
(add-hook 'nrepl-mode-hook 'nrepl-mode-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

;; I want history up/down without modifiers
(define-key nrepl-repl-mode-map (kbd "<up>") 'nrepl-backward-input)
(define-key nrepl-repl-mode-map (kbd "<down>") 'nrepl-forward-input)
(define-key nrepl-repl-mode-map (kbd "C-<up>") 'previous-line)
(define-key nrepl-repl-mode-map (kbd "C-<down>") 'next-line)
(define-key nrepl-repl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
(define-key nrepl-interaction-mode-map (kbd "C-c D") 'nrepl-doc)


(eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'nrepl-mode))

;; Paredit in clojure
(add-hook 'clojure-mode-hook 'paredit-mode)

;; eldoc in clojure
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)

(add-hook 'nrepl-interaction-mode-hook
          'paredit-mode)

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

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Keep track of recent files
(recentf-mode)

;; Enable Nyan mode
(nyan-mode 1)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Transparently open compressed files
(auto-compression-mode t)

(provide 'init-modes)
