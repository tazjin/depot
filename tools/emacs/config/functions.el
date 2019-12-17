(defun load-file-if-exists (filename)
  (if (file-exists-p filename)
      (load filename)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (setq-local display-line-numbers t)
        (let ((target (read-number "Goto line: ")))
          (avy-push-mark)
          (goto-line target)))
    (setq-local display-line-numbers nil)))

;; These come from the emacs starter kit

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|DEBUG\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun esk-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Open Fefes blog
(defun fefes-blog ()
  (interactive)
  (eww "https://blog.fefe.de/"))

;; Open the NixOS man page
(defun nixos-man ()
  (interactive)
  (man "configuration.nix"))

;; Open my monorepo in magit
(defun depot-status ()
  (interactive)
  (magit-status "~/depot"))

;; Get the nix store path for a given derivation.
;; If the derivation has not been built before, this will trigger a build.
(defun nix-store-path (derivation)
  (let ((expr (concat "with import <nixos> {}; " derivation)))
    (s-chomp (shell-command-to-string (concat "nix-build -E '" expr "'")))))

(defun insert-nix-store-path ()
  (interactive)
  (let ((derivation (read-string "Derivation name (in <nixos>): ")))
    (insert (nix-store-path derivation))))

(defun toggle-force-newline ()
  "Buffer-local toggle for enforcing final newline on save."
  (interactive)
  (setq-local require-final-newline (not require-final-newline))
  (message "require-final-newline in buffer %s is now %s"
           (buffer-name)
           require-final-newline))

;; Helm includes a command to run external applications, which does
;; not seem to exist in ivy. This implementation uses some of the
;; logic from Helm to provide similar functionality using ivy.
(defun list-external-commands ()
  "Creates a list of all external commands available on $PATH
  while filtering NixOS wrappers."
  (cl-loop
   for dir in (split-string (getenv "PATH") path-separator)
   when (and (file-exists-p dir) (file-accessible-directory-p dir))
   for lsdir = (cl-loop for i in (directory-files dir t)
                        for bn = (file-name-nondirectory i)
                        when (and (not (s-contains? "-wrapped" i))
                                  (not (member bn completions))
                                  (not (file-directory-p i))
                                  (file-executable-p i))
                        collect bn)
   append lsdir into completions
   finally return (sort completions 'string-lessp)))

(defvar external-command-flag-overrides
  '(("google-chrome" . "--force-device-scale-factor=1.4"))

  "This setting lets me add additional flags to specific commands
  that are run interactively via `ivy-run-external-command'.")

(defun run-external-command (cmd)
  "Execute the specified command and notify the user when it
  finishes."
    (let* ((extra-flags (cdr (assoc cmd external-command-flag-overrides)))
           (cmd (if extra-flags (s-join " " (list cmd extra-flags)) cmd)))
      (message "Starting %s..." cmd)
      (set-process-sentinel
       (start-process-shell-command cmd nil cmd)
       (lambda (process event)
         (when (string= event "finished\n")
           (message "%s process finished." process))))))

(defun ivy-run-external-command ()
  "Prompts the user with a list of all installed applications and
  lets them select one to launch."

  (interactive)
  (let ((external-commands-list (list-external-commands)))
    (ivy-read "Command:" external-commands-list
              :require-match t
              :history 'external-commands-history
              :action #'run-external-command)))

(defun ivy-password-store (&optional password-store-dir)
  "Custom version of password-store integration with ivy that
  actually uses the GPG agent correctly."

  (interactive)
  (ivy-read "Copy password of entry: "
            (password-store-list (or password-store-dir (password-store-dir)))
            :require-match t
            :keymap ivy-pass-map
            :action (lambda (entry)
                      (let ((password (auth-source-pass-get 'secret entry)))
                        (password-store-clear)
                        (kill-new password)
                        (setq password-store-kill-ring-pointer kill-ring-yank-pointer)
                        (message "Copied %s to the kill ring. Will clear in %s seconds."
                                 entry (password-store-timeout))
                        (setq password-store-timeout-timer
                              (run-at-time (password-store-timeout)
                                           nil 'password-store-clear))))))

(defun ivy-browse-repositories ()
  "Select a git repository and open its associated magit buffer."

  (interactive)
  (ivy-read "Repository: "
            (magit-list-repos)
            :require-match t
            :sort t
            :action #'magit-status))

(defun warmup-gpg-agent (arg &optional exit)
  "Function used to warm up the GPG agent before use. This is
   useful in cases where there is no easy way to make pinentry run
   in the correct context (such as when sending email)."
  (interactive)
  (message "Warming up GPG agent")
  (epg-sign-string (epg-make-context) "dummy")
  nil)

(defun bottom-right-window-p ()
  "Determines whether the last (i.e. bottom-right) window of the
  active frame is showing the buffer in which this function is
  executed."
  (let* ((frame (selected-frame))
         (right-windows (window-at-side-list frame 'right))
         (bottom-windows (window-at-side-list frame 'bottom))
         (last-window (car (seq-intersection right-windows bottom-windows))))
    (eq (current-buffer) (window-buffer last-window))))

(defhydra mc/mark-more-hydra (:color pink)
  ("<up>" mmlte--up "Mark previous like this")
  ("<down>" mc/mmlte--down "Mark next like this")
  ("<left>" mc/mmlte--left (if (eq mc/mark-more-like-this-extended-direction 'up)
                               "Skip past the cursor furthest up"
                             "Remove the cursor furthest down"))
  ("<right>" mc/mmlte--right (if (eq mc/mark-more-like-this-extended-direction 'up)
                                 "Remove the cursor furthest up"
                               "Skip past the cursor furthest down"))
  ("f" nil "Finish selecting"))

;; Mute the message that mc/mmlte wants to print on its own
(advice-add 'mc/mmlte--message :around (lambda (&rest args) (ignore)))

(defun mc/mark-dwim (arg)
  "Select multiple things, but do what I mean."

  (interactive "p")
  (if (not (region-active-p)) (mc/mark-next-lines arg)
    (if (< 1 (count-lines (region-beginning)
                          (region-end)))
        (mc/edit-lines arg)
      ;; The following is almost identical to `mc/mark-more-like-this-extended',
      ;; but uses a hydra (`mc/mark-more-hydra') instead of a transient key map.
      (mc/mmlte--down)
      (mc/mark-more-hydra/body))))

(defun memespace-region ()
  "Make a meme out of it."

  (interactive)
  (let* ((start (region-beginning))
         (end (region-end))
         (memed
          (message
           (s-trim-right
            (apply #'string
                   (-flatten
                    (nreverse
                     (-reduce-from (lambda (acc x)
                                     (cons (cons x (-repeat (+ 1 (length acc)) 32)) acc))
                                   '()
                                   (string-to-list (buffer-substring-no-properties start end))))))))))

    (save-excursion (delete-region start end)
                    (goto-char start)
                    (insert memed))))

(defun insert-todo-comment (prefix todo)
  "Insert a comment at point with something for me to do."

  (interactive "P\nsWhat needs doing? ")
  (save-excursion
    (move-end-of-line nil)
    (insert (format " %s TODO(%s): %s"
                    comment-start
                    (if prefix (read-string "Who needs to do this? ")
                      (getenv "USER"))
                    todo))))

;; Custom text scale adjustment functions that operate on the entire instance
(defun modify-text-scale (factor)
  (set-face-attribute 'default nil
                      :height (+ (* factor 5) (face-attribute 'default :height))))

(defun increase-default-text-scale (prefix)
  "Increase default text scale in all Emacs frames, or just the
  current frame if PREFIX is set."

  (interactive "P")
  (if prefix (text-scale-increase 1)
    (modify-text-scale 1)))

(defun decrease-default-text-scale (prefix)
  "Increase default text scale in all Emacs frames, or just the
  current frame if PREFIX is set."

  (interactive "P")
  (if prefix (text-scale-decrease 1)
    (modify-text-scale -1)))

(defun set-default-text-scale (prefix &optional to)
  "Set the default text scale to the specified value, or the
  default. Restores current frame's text scale only, if PREFIX is
  set."

  (interactive "P")
  (if prefix (text-scale-adjust 0)
    (set-face-attribute 'default nil :height (or to 120))))

(provide 'functions)
