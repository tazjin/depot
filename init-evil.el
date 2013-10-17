; Basic vim emulation

(evil-mode t)
; (global-evil-tabs-mode 1)

(evil-ex-define-cmd "Exp[lore]" 'dired-jump)
(evil-ex-define-cmd "color[scheme]" 'customize-themes)

(evil-define-key 'normal global-map
  "\\\\w" 'evil-ace-jump-word-mode)

(evil-define-key 'normal clojure-mode-map
  "\M-q" 'paredit-reindent-defun
  "gK" 'nrepl-src
  "K"  'ac-nrepl-popup-doc)

(setq evil-default-cursor '("#ffdd33"))

;;; Uncomment these key-chord lines if you like that "remap 'jk' to ESC" trick.
;; (key-chord-mode t)
;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(provide 'init-evil)
