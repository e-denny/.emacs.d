;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; avy
;; ----------------------------------------------------------------------

(use-package avy
  :bind
  (("s-j j" . avy-goto-char-timer)
   ("s-j l" . avy-goto-line)
   ("s-j c" . avy-goto-char-2))
  :config
  (setq avy-background t))

(provide 'init-avy)
;;; init-avy.el ends here
