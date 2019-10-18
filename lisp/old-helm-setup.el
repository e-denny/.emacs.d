;;; helm-setup.el --- Helm Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Helm Setup

;;; Code:

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (setq helm-candidate-number-limit 100
        helm-ff-file-name-history-use-recentf t
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-ff-skip-boring-files t)
  :bind (("M-x" . helm-M-x)
         ("C-c h". helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x C-b" . helm-buffers-list)
         ("M-y" . helm-show-key-ring)
         ("C-x c o" . helm-occur)
         ("C-h a" . helm-apropos)
         ("C-h d" . helm-info-at-point)
         ("C-x C-r" . helm-recentf))
  :bind (:map helm-map
              ("M-i" . helm-previous-line)
              ("M-k" . helm-next-line)
              ("M-I" . helm-previous-page)
              ("M-K" . helm-next-page)
              ("M-h" . helm-beginning-of-buffer)
              ("M-H" . helm-end-of-buffer))
  :config
  (setq helm-buffers-fuzzy-matching t
        helm-autoresize-max-height 30
        helm-autoresize-min-height 30
        helm-buffer-max-length 80
        helm-echo-input-in-header-line t
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-buffer-not-found))
  (setq-default helm-split-window-inside-p t)
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package helm-bookmark
  :bind (("C-x M-b" . helm-bookmarks)))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))

(use-package helm-flycheck
  :ensure t
  :after flycheck helm
  :bind (:map flycheck-mode-map
              ("C-c ! h" . helm-flycheck)))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop))
  :init
  (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
  :config
  (setq helm-swoop-pre-input-function (lambda () "")
        helm-swoop-split-with-multiple-windows t))

(use-package helm-ag
  :ensure helm-ag
  :bind (("C-c a" . helm-ag)
         ("M-p" . helm-projectile-ag))
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
              helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package helm-projectile
  :ensure t
  :bind (("C-x , p" . helm-projectile-switch-project)
         ("C-x , f" . helm-projectile-find-file)
         ("C-x , b" . projectile-ibuffer)
         ("C-x , i" . projectile-invalidate-cache)
         ("C-x , a" . helm-projectile-ag))
  :config
  (progn
    (projectile-mode)
    (setq-default projectile-enable-caching t)))

(provide 'helm-setup)

;;; helm-setup.el ends here
