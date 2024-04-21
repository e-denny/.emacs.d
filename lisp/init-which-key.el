;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; which key
;; ----------------------------------------------------------------------

(use-package which-key
  :diminish which-key-mode
  :config
  (progn
    (setq which-key-idle-secondary-delay 0
          which-key-sort-order 'which-key-key-order-alpha
          which-key-min-display-lines 5
          which-key-show-operator-state-maps t
          which-key-add-column-padding 1)
    (which-key-mode +1)))

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide 'init-which-key)
;;; init.el ends here
