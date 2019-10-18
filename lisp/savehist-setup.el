;;; savehist-setup.el --- Savehist Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Savehist Setup

;;; Code:

(use-package savehist
  :init
  (progn
    (setq savehist-file "~/.emacs.d/savehist")
    (setq savehist-save-minibuffer-history 1)
    (setq savehist-additional-variables
          '(kill-ring
            search-ring
            regexp-search-ring))
    (savehist-mode 1)))


(provide 'savehist-setup)

;;; savehist-setup.el ends here
