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
  ;; FIXME; these bindings do not work
  :general
  (my-leader-key
    "hc" 'helpful-command
    "hv" 'helpful-variable
    "hf" 'helpful-function
    "hk" 'helpful-key
    "hh" 'helpful-at-point)
  :bind
  (([remap describe-symbol] . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-command] . helpful-command)
   ([remap describe-key] . helpful-key)
   :map helpful-mode-map
   ("q" . delete-window)))

(provide 'init-help)
;;; init-help.el ends here
