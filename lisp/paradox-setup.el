;;; paradox-setup.el --- Paradox Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Paradox Setup

;;; Code:

(use-package paradox
  :defer t
  :config
  (progn
    (setq paradox-spinner-type 'progress-bar
          paradox-execute-asynchronously t
          paradox-column-width-package 27
          paradox-column-width-version 13)
    (setq paradox-github-token "56f78f2809f8c42ece61e27b7061a5f72e9efd20")
;;    (load (locate-user-emacs-file "paradox-token") :noerror :nomessage)
    (setq paradox-lines-per-entry 1)
    (setq paradox-automatically-star t)
    (paradox-enable)))

(provide 'paradox-setup)

;;; paradox-setup.el ends here
