;;; sx-setup.el --- Sx Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Sx Setup

;;; Code:

(use-package sx
    :ensure t
    :config
    (bind-keys :prefix "C-c s"
               :prefix-map my-sx-map
               :prefix-docstring "Global keymap for SX."
               ("q" . sx-tab-all-questions)
               ("i" . sx-inbox)
               ("o" . sx-open-link)
               ("u" . sx-tab-unanswered-my-tags)
               ("a" . sx-ask)
               ("s" . sx-search)))

(provide 'sx-setup)

;;; sx-setup.el ends here
