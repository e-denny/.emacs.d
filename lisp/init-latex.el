;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; latex
;; ----------------------------------------------------------------------

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\$" . LaTex-mode)
  :hook
  ((Latex-mode . prettify-symbols-mode))
  :custom
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctax)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (LaTeX-indent-level-count 2)
  (TeX-electric-math (cons "$" "$"))
  (LaTeX-electric-left-right-brace t)
  (reftex-ref-macro-prompt nil)
  (reftex-plug-into-AUCTeX t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (TeX-master nil)

  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t))

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; (defun my/yas-try-expanding-auto-snippets ()
;;   (when (bound-and-true-p 'yas-minor-mode)
;;     (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
;;       (yas-expand))))

;; ;; Try after every insertion
;; (add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets)

(use-package latex-preview-pane
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))
  ;; Don't cache preamble, it creates issues with SyncTeX. Let users enable
  ;; caching if they have compilation times that long.
  (setq preview-auto-cache-preamble nil))


(use-package cdlatex
  :hook ((LaTeX-mode . cdlatex-mode)
         (org-mode . org-cdlatex-mode))
  :config
  ;; Use \( ... \) instead of $ ... $.
  ;; (setq cdlatex-use-dollar-to-ensure-math nil)
  ;; Disabling keys that have overlapping functionality with other parts of Doom.
  )

(use-package auctex-latexmk
  :after latex
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active.
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Set LatexMk as the default.
  :hook (LaTeX-mode . (lambda ()
                        (setq TeX-command-default "LatexMk")))
  :config
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup))

(use-package consult-tex)

(use-package reftex
  :ensure nil
  :after latex
  :defer 2
  :commands turn-on-reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :config
  (setq reftex-default-bibliography '("/home/edgar/Documents/Library/library.bib"))
  (setq reftex-insert-label-flags '("sf" "sfte"))
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-ref-style-default-list '("Default" "AMSmath" "Cleveref"))
  (setq reftex-use-multiple-selection-buffers t))

;; Managing Bibliographies
(use-package bibtex
  :ensure nil
  :custom
  (bibtex-dialect 'BibTeX)
  (bibtex-set-dialect 'biblatex)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file." "" )))
  (bibtex-align-at-equal-sign t))


;; Biblio package for adding BibTeX records and download publications
(use-package biblio)

(use-package citar
  :bind (:map org-mode-map
              ("s-o C i" . org-cite-insert)
              ("s-o C r" . citar-insert-reference)
              ("s-o C n" . citar-open-notes)
              :map LaTeX-mode-map
              ("s-l r" . citar-insert-reference)
              ("s-l c" . citar-insert-citation))
  :custom
  (org-cite-global-bibliography '("/home/edgar/Documents/Library/library.bib"))
  (org-cite-insert-pirocessor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-at-point-function 'embark-act)
  (citar-bibliography org-cite-global-bibliography))

(use-package citar-embark
  :after citar embark
  :no-require
  :config
  (citar-embark-mode))

(provide 'init-latex)
;;; init.el ends here
