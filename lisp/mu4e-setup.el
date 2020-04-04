;;; mu4e-setup.el --- Mu4e Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Mu4e Setup

;;; Code:


(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(use-package smtpmail
  :ensure nil
  :init
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials
        '(("smtp.gmail.com" 587 nil nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t))

(use-package mu4e
  :ensure nil
  :config
  (progn
    (require 'org-mu4e)
    (setq mu4e-maildir (expand-file-name "~/.mail")
          mu4e-attachment-dir "~/Downloads"
          mu4e-compose-signature-auto-include nil

          mu4e-trash-folder "/home/[Gmail]/Trash"
          mu4e-refile-folder "/home/[Gmail]/Archive"
          ;; mu4e-sent-folder "/SentMail"

          mu4e-get-mail-command "mbsync -c ~/.mbsyncrc home-inbox"
          mu4e-html2text-command "w3m -T text/html"

          mu4e-maildir-shortcuts
          '(("/inbox" . ?i)
            ("/Drafts" . ?D)
            ("/SentMail" . ?s))

          mu4e-update-interval 300
          mu4e-headers-visible-lines 30
          mu4e-headers-auto-update t
          mu4e-view-show-addresses t
          mu4e-view-show-images t
          mu4e-headers-skip-duplicates t
          mu4e-compose-signature-auto-include nil
          mu4e-sent-messages-behavior 'delete)

    ;; Use 'helm' to select mailboxes
    (setq mu4e-completing-read-function 'completing-read)

    ;; Don't ask to quit
    (setq mu4e-confirm-quit nil)

    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))
    (setq mu4e-headers-fields
          '((:date .  25)
            (:flags .  6)
            (:from-or-to . 22)
            (:mailing-list . 10)
            (:size . 7)
            (:thread-subject . nil))
          mu4e-headers-from-or-to-prefix nil)
    ;; view in Firefox with 'aV'
    (add-to-list 'mu4e-view-actions
                 '("ViewInBrowser" . mu4e-action-view-in-browser) t)
    (add-hook 'mu4e-compose-mode-hook
              (lambda ()
                (set-fill-column 76)
                (flyspell-mode)))))

(provide 'mu4e-setup)

;;; mu4e-setup.el ends here
