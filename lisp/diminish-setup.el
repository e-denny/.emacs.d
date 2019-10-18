;;; diminish-setup.el --- Diminish Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Diminish Setup

;;; Code:

(use-package diminish
  :ensure t)

(diminish 'visual-line-mode "")
(diminish 'isearch-mode "?")

(provide 'diminish-setup)

;;; diminish-setup.el ends here
