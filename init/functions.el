(require 's)
;; A few handy functions I use in init.el (or not, but they're nice to
;; have)

(defun custom-download-theme (url filename)
  "Downloads a theme through HTTP and places it in ~/.emacs.d/themes"

  ;; Ensure the directory exists
  (unless (file-exists-p "~/.emacs.d/themes")
    (make-directory "~/.emacs.d/themes"))

  ;; Adds the themes folder to the theme load path (if not already
  ;; there)
  (unless (member "~/.emacs.d/themes" custom-theme-load-path)
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes"))

  ;; Download file if it doesn't exist.

  (let ((file
         (concat "~/.emacs.d/themes/" filename)))
    (unless (file-exists-p file)
      (url-copy-file url file))))

(defun custom-download-script (url filename)
  "Downloads an Elisp script, places it in ~/.emacs/other and then loads it"

  ;; Ensure the directory exists
  (unless (file-exists-p "~/.emacs.d/other")
    (make-directory "~/.emacs.d/other"))

  ;; Download file if it doesn't exist.
  (let ((file
         (concat "~/.emacs.d/other/" filename)))
    (unless (file-exists-p file)
      (url-copy-file url file))

    (load file)))

(defun keychain-password (account &optional keychain)
  "Returns the password for the account, by default it's looked up in the Login.keychain but a
   different keychain can be specified."
  (let ((k (if keychain keychain "Login.keychain")))
    (replace-regexp-in-string
     "\n" ""
     (shell-command-to-string (concat  "security find-generic-password -w -a "
                                       account
                                       " "
                                       k)))))

;; This clones a git repository to 'foldername in .emacs.d
;; if there isn't already a folder with that name
(defun custom-clone-git (url foldername)
  "Clones a git repository to .emacs.d/foldername"
  (let ((fullpath (concat "~/.emacs.d/" foldername)))
    (unless (file-exists-p fullpath)
      (async-shell-command (concat "git clone " url " " fullpath)))))

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


(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

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

;; Open this machines NixOS config
(defun nix-config ()
  (interactive)
  (find-file "/etc/nixos/configuration.nix"))

;; Open the NixOS man page
(defun nixos-man ()
  (interactive)
  (man "configuration.nix"))

;; Open local emacs configuration
(defun emacs-config ()
  (interactive)
  (dired "~/.emacs.d/"))

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

(defun ivy-run-external-command ()
  "Prompts the user with a list of all installed applications and
  lets them select one to launch."

  (interactive)
  (let ((external-commands-list (list-external-commands)))
    (ivy-read "Command:" external-commands-list
              :require-match t
              :history 'external-commands-history
              :action (lambda (cmd)
                        (message "Starting %s..." cmd)
                        (set-process-sentinel
                         (start-process-shell-command cmd nil cmd)
                         (lambda (process event)
                           (when (string= event "finished\n")
                             (message "%s process finished." process))))))))

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

(provide 'functions)
