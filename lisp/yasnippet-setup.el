;;; yasnippet-setup.el --- Yasnippet Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Yasnippet Setup

;;; Code:

(use-package yasnippet
      :ensure t
      :diminish yas-minor-mode
      :config
      (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
      (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
      (yas-global-mode)
      (global-set-key (kbd "M-/") 'company-yasnippet))

(provide 'yasnippet-setup)

;;; yasnippet-setup.el ends here
