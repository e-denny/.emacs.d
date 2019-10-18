;;; multiple-cursors-setup.el --- Multiple-Cursors Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Multiple-Cursors Setup

;;; Code:

(use-package multiple-cursors
  :init
  (progn
    ;; these need to be defined here - if they're lazily loaded with
    ;; :bind the key won't work. FIXME: is this true?
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C-'") 'mc-hide-unmatched-lines-mode)))

(provide 'multiple-cursors-setup)

;;; multiple-cursors-setup.el ends here
