;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; info
;; ----------------------------------------------------------------------

(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;; ----------------------------------------------------------------------
;; helpful
;; ----------------------------------------------------------------------

(use-package helpful
  :bind
  (("s-h c" . helpful-command)
   ("s-h v" . helpful-variable)
   ("s-h f" . helpful-function)
   ("s-h k" . helpful-key)
   ("s-h ." . helpful-at-point)
   ([remap describe-symbol] . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-command] . helpful-command)
   ([remap describe-key] . helpful-key)
   :map helpful-mode-map
   ("q" . delete-window)))

(provide 'init-help)
;;; init-help.el ends here
