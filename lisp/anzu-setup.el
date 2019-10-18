;;; anzu-setup.el --- Anzu Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Anzu Setup

;;; Code:

(use-package anzu
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode))

(provide 'anzu-setup)

;;; anzu-setup.el ends here
