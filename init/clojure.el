(require 'cider)
(require 'ac-cider-compliment)

;; Configure CIDER (Clojure REPL) and clojure-mode

(defun cider-mode-setup ()
  "Activates paredit, rainbow delimiters and ac-nrepl"
  (ac-cider-compliment-setup)
  (ac-flyspell-workaround)
  (paredit-mode))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes cider-mode))

(add-hook 'cider-repl-mode-hook 'cider-mode-setup)
(add-hook 'cider-interaction-mode-hook 'cider-mode-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; Paredit in clojure
(add-hook 'clojure-mode-hook 'paredit-mode)

;; eldoc in clojure
(add-hook 'cider-interaction-mode-hook
          'cider-turn-on-eldoc-mode)

;; Don't annoy me
(setq cider-hide-special-buffers t)
(setq cider-popup-stacktraces nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-repl-popup-stacktraces t)

;; I want history up/down without modifiers
(define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-backward-input)
(define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-forward-input)
(define-key cider-repl-mode-map (kbd "C-<up>") 'previous-line)
(define-key cider-repl-mode-map (kbd "C-<down>") 'next-line)

;; ... and ac-cider with C-c C-d
(define-key cider-repl-mode-map (kbd "C-c C-d") 'ac-cider-compliment-popup-doc)
(define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-compliment-popup-doc)
(define-key cider-mode-map (kbd "C-c D") 'cider-doc)


(provide 'clojure)
