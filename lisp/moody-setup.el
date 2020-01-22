;;; moody-setup.el --- Moody Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Spaceline Setup

;;; Code:

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 32)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config
  (setq minions-direct '(projectile-mode flycheck-mode))
  (minions-mode 1))

(provide 'moody-setup)

;;; moody-setup.el ends here
