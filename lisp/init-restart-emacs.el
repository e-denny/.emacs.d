;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; restart emacs
;; ----------------------------------------------------------------------

(use-package restart-emacs
  :config
  (setq restart-emacs-restore-frames t))

(provide 'init-restart-emacs)
;;; init-restart-emacs.el ends here
