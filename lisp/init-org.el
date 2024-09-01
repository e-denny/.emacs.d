;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; org
;; ----------------------------------------------------------------------

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("s-o s" . org-store-link)
              ("s-o l" . org-insert-link)
              ("s-o m" . org-mu4e-store-and-capture)
              ("s-o c" . org-capture)
              ("s-o a" . org-agenda)
              ("s-o b" . org-iswitchb)
              ("s-o r" . org-refile))
  ;; :custom-face
  ;; (org-document-title ((t (:foreground "#171717" :weight bold :height 1.5))))
  :config
  (progn

    (setq org-cite-export-processors '((latex natbib)
                                       (t basic)))

    (setq org-directory "/home/edgar/org")
    (setq org-agenda-files (directory-files-recursively (concat org-directory "/agenda/") "\.org$"))
    (setq org-log-done 'time)
    (setq org-agenda-show-all-dates nil)
    (setq org-capture-templates
          `(("i" "Todo [inbox]" entry
             (file+headline ,(format "%s/%s" org-directory "Inbox.org") "Inbox")
             "* TODO %i%?")
            ("p" "Permanent note" plain
             (file denote-last-path)
             #'denote-org-capture
             :no-save t
             :immediate-finish nil
             :kill-buffer t
             :jump-to-captured t)
            ("w" "Web site" entry
             (file "")
             "* %a :website:\n\n%U %?\n\n%:initial")))

    (setq org-return-follows-link t)

    ;;    (org-catch-invisible-edits 'show)

    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-src-fontify-natively t
          org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          org-src-preserve-indentation t)

    (add-to-list 'org-modules 'org-habit t)

    ;; log TODO creation also
    (setq org-treat-insert-todo-heading-as-state-change t)
    ;; log into LOGBOOK drawer
    (setq org-log-into-drawer t)

    ;; show agenda from today rather than Monday
    (setq org-agenda-start-on-weekday nil)

    (setq org-startup-indented t
          org-startup-truncated nil
          org-ellipsis " …"
          org-return-follows-link t)
    (setq org-hide-emphasis-markers t)
    (setq org-hide-leading-stars t)
    (setq org-refile-use-outline-path t
          org-reverse-note-order nil)
    (setq org-tags-column 70)
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-deadline-warning-days 7)
    (setq org-agenda-ndays 7)

    ;;don't show tasks that are scheduled or have deadlines in the
    ;;normal todo list
    (setq org-agenda-todo-ignore-deadlines 'all)
    (setq org-agenda-todo-ignore-scheduled 'all)
    ;;sort tasks in order of when they are due and then by priority
    (setq org-agenda-sorting-strategy
          '((agenda deadline-up priority-down)
            (todo priority-down category-keep)
            (tags priority-down category-keep)
            (search category-keep)))

    ;; refile
    (setq org-refile-targets '((nil :maxlevel . 4)
                               (org-agenda-files :maxlevel . 4)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    (setq org-tags-column 0)
    (setq org-agenda-tags-column 0)

    (setq org-agenda-span 14)
    (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "|" "DONE(d)")))
    (add-hook 'org-mode-hook (lambda () (setq fill-column 100)))
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (add-hook 'org-mode-hook 'org-indent-mode)

    (setq org-outline-path-complete-in-steps 't)

    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . t)
                                   (python . t)))))


;; interactively toggle visibility of org elements on entering and leaving
(use-package org-appear
  :after org
  :config
  (setq org-link-descriptive t)
  (setq org-appear-autolinks t)
  (add-hook 'org-mode-hook 'org-appear-mode))


;; Modernise Org mode interface
(use-package org-modern
  :after org
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil))


(use-package org-sidebar
  :after org
  :commands (org-sidebar-tree-toggle org-sidebar-toggle org-sidebar-ql)
  :custom (org-sidebar-tree-side 'left))

(setq package-check-signature nil)

;; download web pages to org
(use-package org-web-tools
  :bind (:map org-mode-map
              ("s-o w" . org-web-tools-insert-link-for-url)
              ("s-o u" . org-web-tools-insert-web-page-as-entry)))

;; drag and drop images to org
(use-package org-download
  :bind ("s-o i" . org-download-image)
  :config
  (setq-default org-download-image-dir (concat org-directory "/marxism/images"))
  :hook (dired-mode . org-download-enable))

;; ----------------------------------------------------------------------
;; citations
;; ----------------------------------------------------------------------

;; provides a completing-read front-end to browse and act on BibTeX, BibLaTeX,
;; and CSL JSON bibliographic data, and LaTeX, markdown, and org-cite editing support.

(use-package oc
  :ensure nil
  :init
  (setq org-cite-global-bibliography '("/home/edgar/org/marxism/biblio/library/library.bib"))
  (setq org-cite-export-processors
        '((md . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
          (latex . biblatex)            ; For humanities
          (odt . (csl "chicago-fullnote-bibliography.csl")) ; Footnote reliant
          (t . (csl "modern-language-association.csl"))     ; Fallback
          )))

;; ----------------------------------------------------------------------
;; pdf
;; ----------------------------------------------------------------------

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf$" . pdf-view-mode)
  :bind
  (:map pdf-view-mode-map
        ("d" . pdf-view-midnight-minor-mode)
        ("i" . consult-imenu)
        ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
        ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page)
        ("C-s" . isearch-forward))
  :config
  (pdf-tools-install-noverify)
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  :custom
  (pdf-view-midnight-colors '("white smoke" . "dark slate gray"))
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(use-package org-pdftools
  :after org
  :init
  (defun +org--pdftools-link-handler (fn &rest args)
    "Produces a link handler for org-pdftools that suppresses missing-epdfinfo errors whenever storing or exporting links."
    (lambda (&rest args)
      (and (ignore-errors (require 'org-pdftools nil t))
           (file-executable-p pdf-info-epdfinfo-program)
           (apply fn args))))
  (org-link-set-parameters (or (bound-and-true-p org-pdftools-link-prefix) "pdf")
                           :follow   (+org--pdftools-link-handler #'org-pdftools-open)
                           :complete (+org--pdftools-link-handler #'org-pdftools-complete-link)
                           :store    (+org--pdftools-link-handler #'org-pdftools-store-link)
                           :export   (+org--pdftools-link-handler #'org-pdftools-export))
  :hook (org-mode . org-pdftools-setup-link)
  :config
  (pdf-tools-install))

;; ----------------------------------------------------------------------
;; latex
;; ----------------------------------------------------------------------


;; LaTeX previews
(use-package org-fragtog
  :after org
  :custom
  (org-startup-with-latex-preview t)
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 1.25)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

(provide 'init-org)
;;; init.el ends here
