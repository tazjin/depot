;; Configure additional settings if this is one of my NixOS machines
;; (i.e. if ExWM is required)
;; -*- lexical-binding: t; -*-

(require 's)
(require 'f)

(defvar is-nixos
  (let ((os-f "/etc/os-release"))
    (s-contains?
     "NixOS" (if (f-file? os-f) (f-read os-f)))))

(if is-nixos
    (progn
      (message "Running on NixOS, configuring ExWM.")
      (require 'exwm)
      (require 'exwm-config)

      (fringe-mode 3)

      (setq exwm-workspace-number 2)
      ;; Make class name the buffer name
      (add-hook 'exwm-update-class-hook
                (lambda ()
                  (exwm-workspace-rename-buffer exwm-class-name)))

      ;; 's-r': Reset
      (exwm-input-set-key (kbd "s-r") #'exwm-reset)
      ;; 's-w': Switch workspace
      (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
      ;; 's-N': Switch to certain workspace
      (dotimes (i 10)
        (exwm-input-set-key (kbd (format "s-%d" i))
                            `(lambda ()
                               (interactive)
                               (exwm-workspace-switch-create ,i))))

      ;; Launch applications with completion (dmenu style!)
      (exwm-input-set-key (kbd "s-p") #'helm-run-external-command)

      ;; Toggle between line-mode / char-mode
      (exwm-input-set-key (kbd "C-c C-t C-t") #'exwm-input-toggle-keyboard)

      ;; Line-editing shortcuts
      (exwm-input-set-simulation-keys
       '(([?\C-b] . left)
         ([?\C-f] . right)
         ([?\C-p] . up)
         ([?\C-n] . down)
         ([?\C-a] . home)
         ([?\C-e] . end)
         ([?\M-v] . prior)
         ([?\C-v] . next)
         ([?\C-d] . delete)
         ([?\C-k] . (S-end delete))))

      ;; Enable EXWM
      (exwm-enable)

(provide 'nixos)
