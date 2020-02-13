;;; fringe-setup.el --- Fringe Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Fringe Setup

;;; Code:

(use-package fringe
  :ensure t
  :commands fringe-mode
  :config
  (set-fringe-mode '(8 . 0)))

(provide 'fringe-setup)

;;; fringe-setup.el ends here
