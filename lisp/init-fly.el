;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; flyspell
;; ----------------------------------------------------------------------

(use-package flyspell
  :commands (flyspell-auto-correct-previous-word flyspell-correct-word-generic)
  :init
  (setq flyspell-use-meta-tab nil)
  :custom
  (flyspell-abbrev-p t)
  (flyspell-use-global-abbrev-table-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :hook
  (text-mode . flyspell-mode)
  :config
  ;; Use mouse
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined))

(add-hook 'flyspell-mode-hook 'flyspell-buffer) ; show mis-spelled

;; ----------------------------------------------------------------------
;; flymake
;; ----------------------------------------------------------------------

(use-package flymake
  :bind (("s-e c" . display-local-help)
     ("s-e e" . flymake-show-buffer-diagnostics)
     ("s-e m" . flymake-menu)
     ("s-e n" . flymake-goto-next-error)
     ("s-e p" . flymake-goto-previous-error)))


(provide 'init-fly)
;;; init-fly.el ends here
