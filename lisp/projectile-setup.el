;;; projectile-setup.el --- Projectile Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Projectile Setup

;;; Code:

(use-package projectile
  :ensure t
  :config
  (defun projectile-short-mode-line ()
    "Short version of the default projectile mode line."
    (format " P[%s]" (projectile-project-name)))
  (setq projectile-mode-line-function 'projectile-short-mode-line
        projectile-mode-line-prefix "P")
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :after (projectile counsel)
  :commands
  (counsel-projectile-rg
   counsel-projectile-find-file
   counsel-projectile-switch-project
   counsel-projectile-switch-to-buffer
   counsel-projectile-mode)
  :config
  (counsel-projectile-mode))

(provide 'projectile-setup)

;;; projectile-setup.el ends here
