;;; pdf-setup.el --- Pdf Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Pdf Setup

;;; Code:

(use-package pdf-tools
  :defer 1
  :magic ("%PDF" . pdf-view-mode)
  :init (pdf-tools-install :no-query))

(use-package pdf-view
  :ensure nil
  :after pdf-tools
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("d" . pdf-annot-delete)
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("t" . pdf-annot-add-text-annotation))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  (pdf-view-use-unicode-ligther nil))

(provide 'pdf-setup)

;;; pdf-setup.el ends here
