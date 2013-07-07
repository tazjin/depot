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
         (concat "~/.emacs.d/" filename)))
    (unless (file-exists-p file)
      (url-copy-file url file))

    (load file)))


;; These come from the emacs starter kit
(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun esk-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun esk-suck-it (suckee)
  "Insert a comment of appropriate length about what can suck it."
  (interactive "MWhat can suck it? ")
  (let ((prefix (concat ";; " suckee " can s"))
        (postfix "ck it!")
        (col (current-column)))
    (insert prefix)
    (dotimes (_ (- 80 col (length prefix) (length postfix))) (insert "u"))
    (insert postfix)))
