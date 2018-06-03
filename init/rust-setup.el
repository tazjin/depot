(require 'company)
(require 'eglot)
(require 'flycheck)
(require 'rust-mode)

;; LSP configuration:
(defvar rust-eglot-initialized nil)
(add-hook 'rust-mode-hook (lambda ()
                            (unless rust-eglot-initialized
                              (call-interactively #'eglot)
                              (setq rust-eglot-initialized t))))

;; Enable cargo-related (C-c C-c C-...) commands.
(add-hook 'rust-mode-hook #'cargo-minor-mode)

;; Configure autocompletion for rust
(add-hook 'rust-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Ensure long compiler errors don't flow out of the screen (very annoying!)
(add-hook 'cargo-process-mode-hook #'visual-line-mode)

(provide 'rust-setup)
