;;; avy-setup.el --- Avy Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Avy Setup

;;; Code:

(use-package avy
  :ensure t
  :commands (avy-goto-word-or-subword-1 avy-goto-char avy-goto-char-in-line)
  :config
  (setq avy-background t))

(provide 'avy-setup)
;;; avy-setup.el ends here
