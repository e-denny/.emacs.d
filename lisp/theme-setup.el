;;; theme-setup.el --- Theme Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Theme Setup

;;; Code:


(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; (use-package gruvbox-theme
;;   :ensure t)
;; (load-theme 'gruvbox-dark-soft t)

(use-package leuven-theme
  :ensure t)
(load-theme 'leuven t)

;; (use-package zenburn-theme
;;   :ensure t)
;; (load-theme 'zenburn t)

(provide 'theme-setup)

;;; theme-setup.el ends here
