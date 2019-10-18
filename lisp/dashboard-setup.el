;;; dashboard-setup.el --- Dashboard Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Dashboard Setup

;;; Code:


(use-package dashboard
  :diminish
  (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-center-content t)
;;  (dashboa)
  (dashboard-items '((recents . 15)
                     (projects . 5)
                     (bookmarks . 5)))
  ;; :custom-face
  ;; (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :hook
  (after-init . dashboard-setup-startup-hook))

(provide 'dashboard-setup)

;;; dashboard-setup.el ends here
