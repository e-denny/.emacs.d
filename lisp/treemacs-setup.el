;;; treemacs-setup.el --- -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Teemacs Setup

;;; Code:

(use-package treemacs
    :commands (treemacs-follow-mode
               treemacs-filewatch-mode
               treemacs-fringe-indicator-mode
               treemacs-git-mode)
    :bind (([f8]        . treemacs)
           ("M-0"       . treemacs-select-window)
           ("C-x 1"     . treemacs-delete-other-windows)
           ("C-x t 1"   . treemacs-delete-other-windows)
           ("C-x t t"   . treemacs)
           ("C-x t b"   . treemacs-bookmark)
           ("C-x t C-t" . treemacs-find-file)
           ("C-x t M-t" . treemacs-find-tag)
           :map treemacs-mode-map
           ([mouse-1]   . treemacs-single-click-expand-action))
    :config
    (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
          treemacs-sorting                 'alphabetic-case-insensitive-desc
          treemacs-follow-after-init       t
          treemacs-is-never-other-window   t
          treemacs-silent-filewatch        t
          treemacs-silent-refresh          t
          treemacs-width                   30)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(provide 'treemacs-setup)

;;; treemacs-setup.el ends here
