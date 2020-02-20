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

(eval-when-compile
  (require 'use-package))

(customize-set-variable 'use-package-compute-statistics t)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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
(require 'exwm-setup)
(require 'diminish-setup)
(require 'fringe-setup)
(require 'treemacs-setup)
(require 'helm-setup)
(require 'flycheck-setup)
(require 'magit-setup)
(require 'projectile-setup)
(require 'company-setup)
(require 'lsp-setup)
(require 'eshell-setup)
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
(require 'modeline-setup)
(require 'moody-setup)
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
(require 'ivy-setup)
(require 'keybindings-setup)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(provide 'init)

;;; init.el ends here
