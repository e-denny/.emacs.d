;;; all-the-icons-setup.el --- All-The-Icons Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; All-The-Icons Setup

;;; Code:

(use-package all-the-icons
  :if window-system
  :ensure t
  :config
  (when (not (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(provide 'all-the-icons-setup)

;;; all-the-icons-setup.el ends here
