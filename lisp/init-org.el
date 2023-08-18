;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; org
;; ----------------------------------------------------------------------

(use-package org
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
            ("w" "Web site" entry
             (file "")
             "* %a :website:\n\n%U %?\n\n%:initial")
            ("M" "mu4e with kill" entry
             (file+headline ,(format "%s/%s" org-directory "Inbox.org") "Inbox")
             "* TODO %a %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\")) \n %c \n")
            ("m" "mu4e" entry
             (file+headline ,(format "%s/%s" org-directory "Inbox.org") "Inbox")
             "* TODO %a %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
            ("j" "Task Diary" entry
             (file+datetree ,(format "%s/%s" org-directory "/Agenda/Journal.org"))
             "* TODO %^{Description}  %^g \n %? \n Added: %U")
            ("T" "Tickler" entry
             (file+headline ,(format "%s/%s" org-directory "Tickler.org") "Tickler")
             "* %i%? \n %^t")))

    (setq org-return-follows-link t)

    ;;    (org-catch-invisible-edits 'show)

    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-src-fontify-natively t
          org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          org-src-preserve-indentation t)
    (setq org-use-speed-commands t)

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
    (setq org-src-fontify-natively t)
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
    (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "NOTE(n)" "WAITING(w)" "|" "DONE(d)")))
    (add-hook 'org-mode-hook (lambda () (setq fill-column 100)))
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (add-hook 'org-mode-hook 'org-indent-mode)

    (setq org-clock-persist t)
    (org-clock-persistence-insinuate)
    (setq org-outline-path-complete-in-steps 't)
    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . t)
                                   (python . t)))

    ;; scale text in latex preview
    (plist-put org-format-latex-options :scale 0.6)

    ;; ---- toggle latex fragments ----

    (defvar org-latex-fragment-last nil
      "Holds last fragment/environment you were on.")

    (defun my/org-latex-fragment--get-current-latex-fragment ()
      "Return the overlay associated with the image under point."
      (car (--select (eq (overlay-get it 'org-overlay-type) 'org-latex-overlay) (overlays-at (point)))))

    (defun my/org-in-latex-fragment-p ()
      "Return the point where the latex fragment begins, if inside
  a latex fragment. Else return false"
      (let* ((el (org-element-context))
             (el-type (car el)))
        (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
             (org-element-property :begin el))))

    (defun org-latex-fragment-toggle-auto ()
      ;; Wait for the s
      (interactive)
      (while-no-input
        (run-with-idle-timer 0.05 nil 'org-latex-fragment-toggle-helper)))

    (defun org-latex-fragment-toggle-helper ()
      "Toggle a latex fragment image "
      (condition-case nil
          (and (eq 'org-mode major-mode)
               (let* ((begin (my/org-in-latex-fragment-p)))
                 (cond
                  ;; were on a fragment and now on a new fragment
                  ((and
                    ;; fragment we were on
                    org-latex-fragment-last
                    ;; and are on a fragment now
                    begin
                    ;; but not on the last one this is a little tricky. as you edit the
                    ;; fragment, it is not equal to the last one. We use the begin
                    ;; property which is less likely to change for the comparison.
                    (not (= begin
                            org-latex-fragment-last)))
                   ;; go back to last one and put image back
                   (save-excursion
                     (goto-char org-latex-fragment-last)
                     (when (my/org-in-latex-fragment-p) (org-toggle-latex-fragment))
                     ;; now remove current imagea
                     (goto-char begin)
                     (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                       (when ov
                         (delete-overlay ov)))
                     ;; and save new fragment
                     (setq org-latex-fragment-last begin)))

                  ;; were on a fragment and now are not on a fragment
                  ((and
                    ;; not on a fragment now
                    (not begin)
                    ;; but we were on one
                    org-latex-fragment-last)
                   ;; put image back on
                   (save-excursion
                     (goto-char org-latex-fragment-last)
                     (when (my/org-in-latex-fragment-p)(org-toggle-latex-fragment)))

                   ;; unset last fragment
                   (setq org-latex-fragment-last nil))

                  ;; were not on a fragment, and now are
                  ((and
                    ;; we were not one one
                    (not org-latex-fragment-last)
                    ;; but now we are
                    begin)
                   (save-excursion
                     (goto-char begin)
                     ;; remove image
                     (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                       (when ov
                         (delete-overlay ov)))
                     (setq org-latex-fragment-last begin)))
                  ;; else not on a fragment
                  ((not begin)
                   (setq org-latex-fragment-last nil)))))
        (error nil)))

    (add-hook 'post-command-hook 'org-latex-fragment-toggle-auto)
    (setq org-latex-fragment-toggle-helper (byte-compile 'org-latex-fragment-toggle-helper))
    (setq org-latex-fragment-toggle-auto (byte-compile 'org-latex-fragment-toggle-auto))

    ;; ---- end: toggle latex fragments ----

    ;; (org-link-set-parameters
    ;;  "search-view" :follow 'my-search-view-link)

    (defun my-search-view-link (txt)
      "Display a list of TODOs that match txt"
      (setq current-prefix-arg 1)
      (org-search-view (null current-prefix-arg) txt))

    (defun my/hash-word-to-search-view ()
      "Create search-view link if word starts with a hash."
      (when (eq major-mode 'org-mode)
        (with-current-buffer (current-buffer)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "\\\( \\\)\\\(#\\\)\\\(\\\w+\\\|\\\w+\\\s_\\\w+\\\)\\\( \\\|$\\\)" nil t)
              (let ((txt (match-string 3)))
                (replace-match (concat " [[search-view:" txt "][#" txt "]] "))))))))

    (add-hook 'before-save-hook 'my/hash-word-to-search-view)

    (setq org-priority-faces '((?A . (:background "DimGrey" :weight bold))
                               (?B . (:background "DimGrey" :weight bold))
                               (?C . (:background "DimGrey" :weight bold))))
    ))

;; (use-package org-bullets
;;   ;;  :commands org-bullets-mode
;;   :config
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (org-bullets-mode 1)))
;;   (setq org-bullets-bullet-list '("●" "○" "■" "□")))

;; interactively toggle visibility of org elements on entering and leaving
(use-package org-appear
  :after org
  :config
  (setq org-link-descriptive t)
  (setq org-appear-autolinks t)
  (add-hook 'org-mode-hook 'org-appear-mode))



(require 'oc)
(setq org-cite-global-bibliography '("/home/edgar/org/marxism/biblio/library/library.bib"))
(setq org-cite-export-processors
      '((md . (csl "chicago-fullnote-bibliography.csl"))   ; Footnote reliant
        (latex . biblatex)                                 ; For humanities
        (odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
        (t . (csl "modern-language-association.csl"))      ; Fallback
        ))


;; provides a completing-read front-end to browse and act on BibTeX, BibLaTeX,
;; and CSL JSON bibliographic data, and LaTeX, markdown, and org-cite editing support.
(use-package citar
  ;;  :after org-cite
  :bind (:map org-mode-map
              ("C-c c i" . org-cite-insert)
              ("C-c c r" . citar-insert-reference)
              ("C-c c n" . citar-open-notes))
  :custom
  (org-cite-global-bibliography '("/home/edgar/org/marxism/biblio/library/library.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography))


(defun my/org-roam-node-from-cite (citekeys &optional entry)
  "Create an org-node note from a CITEKEYS and ENTRY."
  (interactive (list (citar-select-ref :filter nil)))
  (let ((title (with-temp-buffer
                 (insert (citar-format--entry "${author editor} :: ${title}"
                                              (or entry (citar-get-entry citekeys))))
                 (buffer-string))))
    (org-roam-capture- :templates
                       '(("r" "reference" plain "%?" :if-new
                          (file+head "${citekey}.org"
                                     ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                          :immediate-finish t
                          :unnarrowed t))
                       :info (list :citekey citekeys)
                       :node (org-roam-node-create :title title)
                       :props '(:finalize find-file))))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;; An emacs package to provide tighter Citar and Org-Roam integration
(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode))


(use-package pdf-tools
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :config
  (pdf-tools-install-noverify)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(use-package org-noter
  :commands (org-noter)
  :config
  (setq org-noter-notes-search-path (list org-directory))
  (setq org-noter-auto-save-last-location t
        org-noter-separate-notes-from-heading t))

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
  :hook (org-mode . org-pdftools-setup-link))


(use-package org-ql
  :after org
  :commands (org-ql-search org-ql-view org-ql-view-sidebar org-ql-sparse-tree org-ql-block))

(use-package org-super-agenda
  :after org
  :config (org-super-agenda-mode))

(setq org-agenda-custom-commands
      '(("c" "Super Agenda" agenda
         (org-super-agenda-mode)
         ((org-super-agenda-groups
           '(
             (:name "Habits"
                    :habit t)
             (:name "Today"
                    :time-grid t
                    :scheduled today)
             (:name "Due Today"
                    :deadline today)
             (:name "Important"
                    :priority "A")
             (:name "Overdue"
                    :deadline past)
             (:name "Due soon"
                    :deadline future)
             (:name "Waiting"
                    :todo "WAITING")
             )))
         (org-agenda nil "a"))
        ("n" "Agenda"
         ((agenda ""
                  ((org-agenda-overriding-header "Scheduled")))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled")
                 (org-agenda-skip-function
                  (quote
                   (org-agenda-skip-entry-if
                    (quote scheduled)))))))
         nil nil)))

(use-package org-sidebar
  :after org
  :commands (org-sidebar-tree-toggle org-sidebar-toggle org-sidebar-ql)
  :custom (org-sidebar-tree-side 'left))

(setq package-check-signature nil)

;; (use-package org-ref
;;   :after org
;;   :commands (org-ref-insert-link org-ref-insert-cite-function org-ref-insert-ref-function org-ref-insert-label-function)
;;   :init
;;   (setq reftex-default-bibliography '("~/Documents/Economics/Economics.bib"))
;;   (setq org-ref-default-bibliography '("~/Documents/Economics/Economics.bib"))
;;   (setq org-ref-pdf-directory '("~/Documents/Economics/files/"))

;;   (setq bibtex-completion-bibliography "~/Documents/Economics/Economics.bib"
;;         bibtex-completion-library-(point)ath "~/Documents/Economics/files/"
;;         ;; bibtex-completion-notes-path "~/Dropbox/bibliography/bibtex-notes"
;;         ))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (concat org-directory "/marxism"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam)
         ("C-c n f" . org-roam-find-file)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-insert))
  :config
  (org-roam-db-autosync-mode))

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

(use-package org-journal
  :after org
  :config
  (setq org-journal-dir "~/org/agenda"))

;; ----------------------------------------------------------------------
;; pdf-tools
;; ----------------------------------------------------------------------



;; ----------------------------------------------------------------------
;; latex
;; ----------------------------------------------------------------------

(defvar previewable-environments
  "List of environments that should be previewed."
  '("tabular" "tabular*" "tikzpicture" "..."))

(defadvice preview-region (around preview-at-point-no-long-pauses activate)
  "Preview all latex snippets when no snippet at point."
  (message "preview-region")
  (if (or (not (eq this-command 'preview-at-point))
          (TeX-active-mark)
          (texmathp)
          (member (LaTeX-current-environment) previewable-environments))
      ad-do-it
    (preview-section)))


(provide 'init-org)
;;; init.el ends here
