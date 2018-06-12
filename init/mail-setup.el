(require 'notmuch)

(global-set-key (kbd "C-c m") 'notmuch-hello)
(global-set-key (kbd "C-c C-e n") 'notmuch-mua-new-mail)

(setq notmuch-cache-dir (format "%s/.cache/notmuch" (getenv "HOME")))
(make-directory notmuch-cache-dir t)

;; Cache addresses for completion:
(setq notmuch-address-save-filename (concat notmuch-cache-dir "/addresses"))

;; Don't spam my home folder with drafts:
(setq notmuch-draft-folder "drafts") ;; relative to notmuch database

;; Mark things as read when archiving them:
(setq notmuch-archive-tags '("-inbox" "-unread" "+archive"))

;; Show me saved searches that I care about:
(setq notmuch-saved-searches
      '((:name "inbox" :query "tag:inbox" :count-query "tag:inbox AND tag:unread" :key "i")
        (:name "aprila-dev" :query "tag:aprila-dev" :count-query "tag:aprila-dev AND tag:unread" :key "d")
        (:name "gitlab" :query "tag:gitlab" :key "g")
        (:name "sent" :query "tag:sent" :key "t")
        (:name "drafts" :query "tag:draft")))
(setq notmuch-show-empty-saved-searches t)

;; Mail sending configuration
(setq send-mail-function 'sendmail-send-it) ;; sendmail provided by MSMTP
(setq notmuch-always-prompt-for-sender t)
(setq notmuch-mua-user-agent-function
      (lambda () (format "Emacs %s; notmuch.el %s" emacs-version notmuch-emacs-version)))

;; Ensure sender is correctly passed to msmtp
(setq mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
(setq mail-host-address (system-name))

;; Store sent mail in the correct folder per account
(setq notmuch-maildir-use-notmuch-insert nil)
(setq notmuch-fcc-dirs '(("mail@tazj.in" . "tazjin/Sent")
                         ;; Not a mistake, Office365 apparently
                         ;; renames IMAP folders (!) to your local
                         ;; language instead of providing translations
                         ;; in the UI m(
                         ("vincent@aprila.no" . "aprila/Sende element")))

;; I don't use drafts but I instinctively hit C-x C-s constantly, lets
;; handle that gracefully.
(define-key notmuch-message-mode-map (kbd "C-x C-s") #'ignore)

;; MSMTP decrypts passwords using pass, but pinentry doesn't work
;; correctly in that setup. This forces a warmup of the GPG agent
;; before sending the message.
;;
;; Note that the sending function is advised because the provided hook
;; for this seems to run at the wrong time.
(advice-add 'notmuch-mua-send-common :before 'warmup-gpg-agent)

(provide 'mail-setup)
