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
;;  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
  :hook ((c-mode c++-mode java-mode python-mode) . lsp))

(use-package ccls
  :config
  (setq ccls-sem-highlight-method 'font-lock)
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-after-open . (lambda ()
                             (lsp-ui-flycheck-enable 1))))
  :config
  (require 'lsp-ui-flycheck)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-delay 5)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (lsp-ui-peek-find-custom "$ccls/call")
  (lsp-ui-peek-find-custom "$ccls/call" '(:callee t))

  (defun ccls/callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
  (defun ccls/caller () (interactive) (lsp-ui-peek-find-custom "$ccls/call"))
  (defun ccls/vars (kind) (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
  (defun ccls/base (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
  (defun ccls/derived (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
  (defun ccls/member (kind) (interactive) (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

  ;; References w/ Role::Role
  (defun ccls/references-read () (interactive)
         (lsp-ui-peek-find-custom "textDocument/references"
                                  (plist-put (lsp--text-document-position-params) :role 8)))

  ;; References w/ Role::Write
  (defun ccls/references-write ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 16)))

  ;; References w/ Role::Dynamic bit (macro expansions)
  (defun ccls/references-macro () (interactive)
         (lsp-ui-peek-find-custom "textDocument/references"
                                  (plist-put (lsp--text-document-position-params) :role 64)))

  ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
  (defun ccls/references-not-call () (interactive)
         (lsp-ui-peek-find-custom "textDocument/references"
                                  (plist-put (lsp--text-document-position-params) :excludeRole 32)))
  ;; References whose filenames are under this project
  (lsp-ui-peek-find-references nil (list :folders (vector (projectile-project-root))))
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
