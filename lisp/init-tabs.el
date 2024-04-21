;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system



(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "•"
        centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-mode t))

  (provide 'init-tabs)
;;; init-tabs.el ends here
