;;; theme-setup.el --- Theme Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Theme Setup

;;; Code:


(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

(use-package leuven-theme
  :config
  (load-theme 'leuven t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil  :overline   line)
    (set-face-attribute 'mode-line-inactive nil  :foreground "black" :overline   line)
    (set-face-attribute 'mode-line-inactive nil  :underline  line)
    (set-face-attribute 'mode-line-emphasis nil  :foreground "black")
    (set-face-attribute 'mode-line-highlight nil :foreground "black")
    (set-face-attribute 'mode-line-buffer-id nil :foreground "black")
    (set-face-attribute 'mode-line          nil  :box        nil)
    (set-face-attribute 'mode-line-inactive nil  :box        nil)))

(provide 'theme-setup)

;;; theme-setup.el ends here
