;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'package)
(setq package-archives
             '(("gnu" . "https://elpa.gnu.org/packages/")
               ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(customize-set-variable 'use-package-compute-statistics t)

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(require 'defaults-setup)
(require 'diminish-setup)
(require 'fringe-setup)
(require 'treemacs-setup)
(require 'flycheck-setup)
(require 'magit-setup)
(require 'projectile-setup)
(require 'company-setup)
(require 'lsp-setup)
(require 'exwm-setup)
(require 'eshell-setup)
;; (require 'helm-setup)
(require 'all-the-icons-setup)
(require 'hydra-setup)
(require 'dired-setup)
(require 'theme-setup)
(require 'rainbow-setup)
(require 'elisp-coding-setup)
(require 'org-setup)
(require 'mu4e-setup)
(require 'elfeed-setup)
(require 'spell-setup)
(require 'pdf-setup)
(require 'spaceline-setup)
(require 'which-key-setup)
(require 'sx-setup)
(require 'dashboard)
(require 'anzu)
(require 'yasnippet-setup)
(require 'window-management-setup)
(require 'browse-kill-ring-setup)
(require 'markdown-setup)
(require 'savehist-setup)
(require 'recentf-setup)
(require 'hippie-exp-setup)
(require 'paradox-setup)
(require 'multiple-cursors-setup)
(require 'avy-setup)
;; (require 'desktop-setup)
;; (require 'modal-setup)
;; (require 'vterm-setup)
(require 'ivy-setup)
(require 'keybindings-setup)
;; (require 'evil-setup)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(provide 'init)

;;; init.el ends here
