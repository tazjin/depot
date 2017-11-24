;; lisp-settings.el - settings for various Lisp dialects
;; -*- lexical-binding: t; -*-

;; All the lisps:

(add-to-list 'lisp-mode-hook #'paredit-mode)

;; Common Lisp:
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program (concat (nix-store-path "sbcl") "/bin/sbcl"))
(setq slime-contribs '(slime-fancy))

(provide 'lisp-setup)
