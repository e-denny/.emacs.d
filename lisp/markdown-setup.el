;;; markdown-setup.el --- Markdown Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Markdown Setup

;;; Code:

(use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

(provide 'markdown-setup)

;;; markdown-setup.el ends here
