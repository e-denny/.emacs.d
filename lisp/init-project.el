;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; project
;; ----------------------------------------------------------------------

(use-package project
  :general
  (my-leader-key "p"
    '(:keymap project-prefix-map :wk "project"))
  ;; (my-leader-key
  ;;   "ps" 'project-switch-project
  ;;   "pb" 'project-switch-to-buffer
  ;;   "pd" 'project-dired
  ;;   "pc" 'project-compile
  ;;   "pe" 'project-eshell
  ;;   "pk" 'project-kill-buffers
  ;;   "pf" 'project-find-file)
  )

(provide 'init-project)
;;; init-project.el ends here
