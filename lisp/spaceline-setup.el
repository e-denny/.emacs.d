;;; spaceline-setup.el --- Spaceline Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Spaceline Setup

;;; Code:

(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (progn (require 'spaceline-config)
         (spaceline-spacemacs-theme)
         (spaceline-helm-mode)))

(provide 'spaceline-setup)

;;; spaceline-setup.el ends here
