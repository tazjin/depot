(require 'company)
(require 'flycheck)
(require 'lsp-mode)
(require 'lsp-rust)
(require 'lsp-ui)
(require 'rust-mode)

;; LSP configuration:
(setq lsp-ui-sideline-delay 0.5)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'rust-mode-hook #'lsp-rust-enable)
(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook 'company-mode)
(push 'company-lsp company-backends)

;; Enable cargo-related (C-c C-c C-...) commands.
(add-hook 'rust-mode-hook #'cargo-minor-mode)

;; Configure autocompletion for rust
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Ensure long compiler errors don't flow out of the screen (very annoying!)
(add-hook 'cargo-process-mode-hook #'visual-line-mode)

(provide 'rust-setup)
