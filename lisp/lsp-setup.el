;;; lsp-setup.el --- Lsp Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Lsp Setup

;;; Code:

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (require 'lsp-clients)
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil)
  :hook ((c-mode c++-mode java-mode python-mode) . lsp))

(use-package lsp-ui
  :ensure t
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-after-open . (lambda ()
                             (lsp-ui-flycheck-enable 1))))
  :config
  (progn (require 'lsp-ui-flycheck)
         (setq lsp-ui-sideline-show-hover nil)
         (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  :bind (:map lsp-ui-mode-map
              ("C-c r ." . lsp-ui-peek-find-definitions)
              ("C-c r ?" . lsp-ui-peek-find-references)
              ("C-c r d" . lsp-ui-peek-find-definitions)
              ("C-c r r" . lsp-ui-peek-find-references)
              ("C-c r F" . lsp-ui-sideline-apply-code-actions)
              ("C-c r R" . lsp-rename)))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :after company lsp-mode
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t
        company-lsp-cache-candidates 'auto
        company-lsp-enable-recompletion t))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(provide 'lsp-setup)

;;; lsp-setup.el ends here
