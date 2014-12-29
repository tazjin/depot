(require 'haskell-mode)

;; Setup for Haskell mode

;; Bindings

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(add-hook 'haskell-mode-hook 'hi2-mode)

;; Settings

(setq haskell-interactive-popup-errors t
      haskell-stylish-on-save t
      haskell-tags-on-save t
      haskell-process-suggest-remove-import-types t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-process-type 'cabal-repl)

(provide 'haskell)
