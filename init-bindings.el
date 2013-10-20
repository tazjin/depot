;; Various keybindings, most of them taken from starter-kit-bindings

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-i") 'imenu)

;; Ace-jump-mode
(global-set-key (kbd "M-j") 'ace-jump-word-mode)

;; Jump to next occurence of char
(global-set-key (kbd "C-c f") 'iy-go-to-char)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Eval sexp and replace it with result
(global-set-key (kbd "C-c e") 'esk-eval-and-replace)

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x C-m") 'shell)

;; So good!
(global-set-key (kbd "C-c g") 'magit-status)

;; Open project drawer
(global-set-key (kbd "M-p") 'project-explorer-open)

;; Add a fullscreen toggle - TODO: reenable in next Emacs release
; (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

;; Replace standard goto-line with goto-line-with-feedback
(global-set-key (kbd "M-g g") 'goto-line-with-feedback)

;; Goodness from @magnars
;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Cider/nrepl stuff
;; I want history up/down without modifiers
(define-key cider-repl-mode-map (kbd "<up>") 'cider-backward-input)
(define-key cider-repl-mode-map (kbd "<down>") 'cider-forward-input)
(define-key cider-repl-mode-map (kbd "C-<up>") 'previous-line)
(define-key cider-repl-mode-map (kbd "C-<down>") 'next-line)

;; ... and ac-cider with C-c C-d
(define-key cider-repl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
(define-key cider-mode-map (kbd "C-c D") 'cider-doc)


(provide 'init-bindings)
