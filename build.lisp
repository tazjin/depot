;; Build script for use within Nix.

(require :asdf)
(require :sb-posix)
(require :cffi)

(push (format nil "~A/" (sb-posix:getcwd)) asdf:*central-registry*)

;; Quicklisp is configured in the Nix build script
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; OpenSSL linking requires pkg_config on NixOS.
(ql:quickload "inferior-shell")

(defun pkg-config-lib-path (lib)
  "Look up the location of a library using pkg-config."
  (let ((flag (inferior-shell:run `("pkg-config" "--libs-only-L" ,lib)
                                  :output '(:string :stripped t))))
    (concatenate 'string (subseq flag 2) "/")))

(pushnew (pathname (pkg-config-lib-path "openssl"))
         cffi:*foreign-library-directories*
         :test #'equal)

;; cl-prevalence is not in the current Quicklisp -> Nix snapshot
(ql:quickload "cl-prevalence")

;; the $out path should be set in the application to let Hunchentoot serve the
;; correct static files.

(if (sb-posix:getenv "out")
    (defvar *gemma-nix-out-dir* (sb-posix:getenv "out")))

(asdf:operate 'asdf:program-op :gemma)
