;;; company-setup.el --- Company Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Company Setup

;;; Code:

(use-package company
  :ensure t
  :defer t
  :bind ("C-<tab>" . company-complete)
  :init (global-company-mode)
  :config
  (progn
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-tooltip-align-annotations t
          company-tooltip-limit 15
          company-show-numbers t
          company-idle-delay 3
          company-minimum-prefix-length 3)
    (setq company-dabbrev-downcase nil)
    (let ((map company-active-map))
      (define-key map (kbd "TAB") 'company-complete-selection)
      (define-key map (kbd "C-/") 'company-search-candidates)
      (define-key map (kbd "C-M-/") 'company-filter-candidates)
      (define-key map (kbd "C-d") 'company-show-doc-buffer)))
  :diminish company-mode)


(provide 'company-setup)

;;; company-setup.el ends here
