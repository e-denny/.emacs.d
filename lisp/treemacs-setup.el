;;; treemacs-setup.el --- -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Teemacs Setup

;;; Code:

(use-package treemacs
    :commands (treemacs-follow-mode
               treemacs-filewatch-mode
               treemacs-fringe-indicator-mode
               treemacs-git-mode)
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
