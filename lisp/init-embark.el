;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; embark
;; ----------------------------------------------------------------------

(use-package embark
  :general
  (my-leader-key
    "." 'embark-act)
  :bind (:map minibuffer-local-map
              ("C-." . embark-act))
  :config
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))


(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package wgrep
  :after (embark-consult ripgrep)
  :bind (:map wgrep-mode-map
              ;; Added keybinding to echo Magit behavior
              ("C-c C-c" . save-buffer)
              :map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              :map ripgrep-search-mode-map
              ("e" . wgrep-change-to-wgrep-mode)))


(provide 'init-embark)
;;; init-embark.el ends here
