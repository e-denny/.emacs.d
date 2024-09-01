;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; snippets
;; ----------------------------------------------------------------------

(use-package yasnippet
  ;; :diminish yas-minor-mode
  :ensure t
  :hook (LaTeX-mode . yas-minor-mode)
  ;; :bind (:map yas-minor-mode-map
  ;;             ("SPC" . yas-expand))
  :config

  (defun my/yas-try-2-suffix (pt)
    (skip-syntax-backward "w" (- (point) 2)))

  (defun my/yas-try-3-suffix (pt)
    (skip-syntax-backward "w" (- (point) 3)))

  (add-to-list 'yas-key-syntaxes #'my/yas-try-2-suffix)
  (add-to-list 'yas-key-syntaxes #'my/yas-try-3-suffix)

  (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
  (add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package warnings
  :ensure nil
  :config
  (cl-pushnew '(yasnippet backquote-change) warning-suppress-types
              :test 'equal))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
