;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; What does <tab> do? Well, it depends ...
(define-key prog-mode-map (kbd "<tab>") #'company-indent-or-complete-common)

;; imenu instead of insert-file
(global-set-key (kbd "C-x i") 'imenu)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x C-p") 'ivy-browse-repositories)
(global-set-key (kbd "M-g M-g") 'goto-line-with-feedback)

;; Miscellaneous editing commands
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "C-c m") 'mc/mark-dwim)

;; Browse URLs (very useful for Gitlab's SSH output!)
(global-set-key (kbd "C-c b p") 'browse-url-at-point)
(global-set-key (kbd "C-c b b") 'browse-url)

;; C-x REALLY QUIT (idea by @magnars)
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'ignore)

;; Open Fefes Blog
(global-set-key (kbd "C-c C-f") 'fefes-blog)

;; Open a file in project:
(global-set-key (kbd "C-c f") 'project-find-file)

(provide 'bindings)
