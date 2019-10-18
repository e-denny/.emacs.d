;;; rainbow-setup.el --- Rainbow Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Rainbow Setup

;;; Code:

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :diminish rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "red"
                      :inherit 'error
                      :box t))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))


(provide 'rainbow-setup)

;;; rainbow-setup.el ends here
