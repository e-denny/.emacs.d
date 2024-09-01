;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; denote
;; ----------------------------------------------------------------------


(use-package denote
  :init
  (denote-rename-buffer-mode t)
  :custom
  (denote-directory "~/Documents/notes")
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :bind
  (("C-c n n" . denote-create-note)
   ("C-c n d" . denote-date)
   ("C-c n i" . denote-link-or-create)
   ("C-c n l" . denote-find-link)
   ("C-c n b" . denote-find-backlink)
   ("C-c n d" . denote-org-dblock-insert-links)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n k" . denote-keywords-add)
   ("C-c n K" . denote-keywords-remove)))


;; Denote extensions
(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("Denote" ?d "Notes")))
  :bind
  (("C-c n f" . consult-notes)
   ("C-c n s" . consult-notes-search-in-all-notes)))


;; Read ePub files
;; (use-package nov
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))


(use-package denote-explore
  :custom
  ;; Where to store network data and in which format
  (denote-explore-network-directory "<folder>")
  (denote-explore-network-filename "<filename?")
  (denote-explore-network-format 'graphviz)
  :bind
  (;; Statistics
   ("C-c w e c" . denote-explore-count-notes)
   ("C-c w e C" . denote-explore-count-keywords)
   ("C-c w e b" . denote-explore-keywords-barchart)
   ("C-c w e x" . denote-explore-extensions-barchart)
   ;; Random walks
   ("C-c w e r" . denote-explore-random-note)
   ("C-c w e l" . denote-explore-random-link)
   ("C-c w e k" . denote-explore-random-keyword)
   ;; Denote Janitor
   ("C-c w e d" . denote-explore-identify-duplicate-notes)
   ("C-c w e z" . denote-explore-zero-keywords)
   ("C-c w e s" . denote-explore-single-keywords)
   ("C-c w e o" . denote-explore-sort-keywords)
   ("C-c w e r" . denote-explore-rename-keywords)
   ;; Visualise denote
   ("C-c w e n" . denote-explore-network)
   ("C-c w e v" . denote-explore-network-regenerate)
   ("C-c w e D" . denote-explore-degree-barchart)))

(provide 'init-denote)
;;; init-denote.el ends here
