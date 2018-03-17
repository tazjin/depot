(require 'company)
(require 'rust-mode)

;; Enable cargo-related (C-c C-c C-...) commands.
(add-hook 'rust-mode-hook #'cargo-minor-mode)

;; Configure autocompletion for rust
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Ensure long compiler errors don't flow out of the screen (very annoying!)
(add-hook 'cargo-process-mode-hook #'visual-line-mode)

;; I don't use rustup and I'm not sure which derivation would normally place
;; Rust sources in the nix-store, so I'll continue using my local Rust checkout.
(setq racer-rust-src-path "~/source/rust/src")

(provide 'rust-setup)
