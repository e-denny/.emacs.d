;;; modeline-setup.el --- Modeline Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Modeline Setup

;;; Code:

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 32)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  )

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   (setq doom-modeline-height 34)
;;   (setq doom-modeline-bar-width 3)
;;   (setq doom-modeline-project-detection 'projectile)
;;   (setq doom-modeline-icon (display-graphic-p))
;;   (setq doom-modeline-major-mode-icon t)
;;   (setq doom-modeline-major-mode-color-icon t)
;;   (setq doom-modeline-buffer-state-icon t)
;;   (setq doom-modeline-buffer-modification-icon t)
;;   (setq doom-modeline-unicode-fallback nil)
;;   (setq doom-modeline-minor-modes t)
;;   (setq doom-modeline-buffer-encoding t)
;;   (setq doom-modeline-vcs-max-length 12)
;;   (setq doom-modeline-lsp t)
;;   (set-face-attribute 'mode-line nil :height 50)
;;   (set-face-attribute 'mode-line-inactive nil :height 50)
;;   )

(setq battery-mode-line-format "[%p] ")
(setq battery-update-interval 15)
(display-battery-mode)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  (setq powerline-default-separator 'butt
        powerline-display-buffer-size nil
        powerline-gui-use-vcs-glyph t
        powerline-display-mule-info nil)
  (setq powerline-height 24))
(add-hook 'after-init-hook 'powerline-reset)

(use-package minions
  :config
  (setq minions-direct '(projectile-mode flycheck-mode))
  (minions-mode 1))

(provide 'modeline-setup)

;;; modeline-setup.el ends here
