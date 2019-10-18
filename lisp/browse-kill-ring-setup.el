;;; browse-kill-ring-setup.el --- Browse-Kill-Ring Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Browse-Kill-Ring Setup

;;; Code:

(use-package browse-kill-ring
  :bind ("C-x C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(setq save-interprogram-paste-before-kill t)


(provide 'browse-kill-ring-setup)

;;; browse-kill-ring-setup.el ends here
