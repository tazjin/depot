(require 'haskell-mode)

;; Setup for Haskell mode

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'hi2-mode)

;; Bindings

(defun haskell-mode-binding-setup ()
  (interactive)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  (define-key haskell-mode-map (kbd "C-r") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space))

(eval-after-load "haskell-mode"
  '(haskell-mode-binding-setup))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; Settings

(setq haskell-interactive-popup-errors nil
      haskell-stylish-on-save t
      haskell-tags-on-save t
      haskell-process-suggest-remove-import-types t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-process-type 'ghci
      haskell-process-show-debug-tips nil)

(provide 'haskell-setup)
