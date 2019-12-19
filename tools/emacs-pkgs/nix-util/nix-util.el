;;; nix-util.el --- Utilities for dealing with Nix code. -*- lexical-binding: true; -*-
;;
;; Copyright (C) 2019 Google Inc.
;;
;; Author: Vincent Ambo <tazjin@google.com>
;; Version: 1.0
;; Package-Requires: (json map)
;;
;;; Commentary:
;;
;; This package adds some functionality that I find useful when
;; working in Nix buffers.

(require 'json)
(require 'map)

(defun nix/prefetch-github (owner repo) ; TODO(tazjin): support different branches
  "Fetch the master branch of a GitHub repository and insert the
  call to `fetchFromGitHub' at point."

  (interactive "sOwner: \nsRepository: ")

  (let* (;; Keep these vars around for output insertion
         (point (point))
         (buffer (current-buffer))
         (name (concat "github-fetcher/" owner "/" repo))
         (outbuf (format "*%s*" name))
         (errbuf (get-buffer-create "*github-fetcher/errors*"))
         (cleanup (lambda ()
                    (kill-buffer outbuf)
                    (kill-buffer errbuf)
                    (with-current-buffer buffer
                      (read-only-mode -1))))
         (prefetch-handler
          (lambda (_process event)
            (unwind-protect
                (pcase event
                  ("finished\n"
                   (let* ((json-string (with-current-buffer outbuf
                                         (buffer-string)))
                          (result (json-parse-string json-string)))
                     (with-current-buffer buffer
                       (goto-char point)
                       (map-let (("rev" rev) ("sha256" sha256)) result
                         (read-only-mode -1)
                         (insert (format "fetchFromGitHub {
  owner = \"%s\";
  repo = \"%s\";
  rev = \"%s\";
  sha256 = \"%s\";
};" owner repo rev sha256))
                         (indent-region point (point))))))
                  (_ (with-current-buffer errbuf
                       (error "Failed to prefetch %s/%s: %s"
                              owner repo (buffer-string)))))
              (funcall cleanup)))))

    ;; Fetching happens asynchronously, but we'd like to make sure the
    ;; point stays in place while that happens.
    (read-only-mode)
    (make-process :name name
                  :buffer outbuf
                  :command `("nix-prefetch-github" ,owner ,repo)
                  :stderr errbuf
                  :sentinel prefetch-handler)))

(provide 'nix-util)
