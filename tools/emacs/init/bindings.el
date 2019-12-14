;; Various keybindings, most of them taken from starter-kit-bindings

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Counsel stuff:
(global-set-key (kbd "C-c r g") 'counsel-rg)

;; imenu instead of insert-file
(global-set-key (kbd "C-x i") 'imenu)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(global-set-key (kbd "C-x p") 'ivy-browse-repositories)
(global-set-key (kbd "M-g M-g") 'goto-line-with-feedback)

(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c a") 'align-regexp)

;; Browse URLs (very useful for Gitlab's SSH output!)
(global-set-key (kbd "C-c b p") 'browse-url-at-point)
(global-set-key (kbd "C-c b b") 'browse-url)

;; Goodness from @magnars
;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Open Fefes Blog
(global-set-key (kbd "C-c C-f") 'fefes-blog)

;; Open a file in project:
(global-set-key (kbd "C-c f") 'project-find-file)

;; Use swiper instead of isearch
(global-set-key "\C-s" 'swiper)

(provide 'bindings)
