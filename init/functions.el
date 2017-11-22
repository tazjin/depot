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

;; These come from magnars, he's got some awesome things.

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))


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

(provide 'functions)
