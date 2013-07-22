;; EShell configuration

(require 'eshell)

(defvar home-dir)
(setq home-dir (expand-file-name "~"))

(setq eshell-path-env (concat
		       "/usr/local/bin:"
		       (concat home-dir "/bin:")
		       eshell-path-env))

;; Prompt configuration

(defun clean-pwd (path)
  "Turns a path of the form /foo/bar/baz into /f/b/baz
   (inspired by fish shell)"
  (message path)
  (let* ((current-dir (split-string path "/"))
	 (cdir (last current-dir))
	 (head (butlast current-dir)))
    (concat (mapconcat (lambda (s)
			 (if (string= "" s) nil
			   (substring s 0 1)))
		       head
		       "/")
	    (if head "/" nil)
	    (car cdir))))

(setq eshell-pwd-convert-function
      (lambda  (path)
	(clean-pwd (replace-regexp-in-string
		    home-dir
		    "~"
		    path))))

(defun vcprompt (&optional args)
  "Call the external vcprompt command with optional arguments.
   VCPrompt"
  (replace-regexp-in-string
   "\n" ""
   (shell-command-to-string (concat  "vcprompt" args))))

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun prompt-f ()
  "My EShell prompt displaying VC info and such"
  (concat
   (with-face (concat (eshell/pwd) " ") :foreground  "#96a6c8")
   (with-face (vcprompt " -f \"(%s:%b%a%m) \"") :foreground "#5f627f")
   (if (= 0 (user-uid))
       (with-face "#" :foreground "#f43841")
     (with-face "$" :foreground "#73c936"))
   (with-face " " :foreground "#95a99f")))


(setq eshell-prompt-function 'prompt-f)
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-regexp "^.+? \\((\\(git\\|svn\\|hg\\|darcs\\|cvs\\|bzr\\):.+?) \\)?[$#] ")


;; EShell functions that come in handy

;; clear in eshell
(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
