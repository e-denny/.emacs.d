;;; recentf-setup.el --- Recentf Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Recentf Setup

;;; Code:

(use-package recentf
  :config
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 50
        recentf-exclude '("^/var/folders\\.*"
                          "COMMIT_EDITMSG\\'"
                          "/\\.git/.*\\'"
                          ".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"))
  (recentf-mode 1))

(provide 'recentf-setup)

;;; recentf-setup.el ends here
