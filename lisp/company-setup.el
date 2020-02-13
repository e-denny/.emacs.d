;;; company-setup.el --- Company Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Company Setup

;;; Code:

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-tooltip-align-annotations t
          company-tooltip-limit 15
          company-show-numbers t
          company-idle-delay 3
          company-minimum-prefix-length 3)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

;; company with icons
(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

(provide 'company-setup)

;;; company-setup.el ends here
