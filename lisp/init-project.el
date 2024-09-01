;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; project
;; ----------------------------------------------------------------------

(use-package project
  :ensure nil
  :bind
  (("s-p p" . project-find-file))
  )

(provide 'init-project)
;;; init-project.el ends here
