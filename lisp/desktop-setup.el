;;; desktop-setup.el --- Desktop Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Desktop Setup

;;; Code:

;; save current buffer configuration
(use-package desktop
  :defer nil
  :custom
  (desktop-restore-eager   10)
  (desktop-lazy-idle-delay 1)
  (desktop-lazy-verbose nil)
  :bind
  ("C-M-s-k" . desktop-clear)
  :config
  (desktop-save-mode 1))

(provide 'desktop-setup)

;;; desktop-setup.el ends here
