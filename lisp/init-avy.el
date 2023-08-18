;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; avy
;; ----------------------------------------------------------------------

(use-package avy
  :init
  (my-leader-key
    "jj" 'avy-goto-char-timer
    "jl" 'avy-goto-line
    "jc" 'avy-goto-char-2)
  :config
  (setq avy-background t))

(provide 'init-avy)
;;; init-avy.el ends here
