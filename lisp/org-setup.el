 ;;; org-setup.el --- Org Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Org Setup

;;; Code:

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile))
  :custom-face
  (org-document-title ((t (:foreground "#171717" :weight bold :height 1.5))))
  :config
  (progn
    (setq org-directory
          (cond ((equal system-type 'windows-nt) "c:/Users/edenny/Org")
                ((equal system-type 'gnu/linux) "/home/edgar/Notes")))
    (setq org-agenda-files (directory-files-recursively (concat org-directory "/Agenda/") "\.org$"))
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
            ("T" "Tickler" entry
             (file+headline ,(format "%s/%s" org-directory "Tickler.org") "Tickler")
             "* %i%? \n %^t")))

    (setq org-return-follows-link t)

;;    (org-catch-invisible-edits 'show)

;;    (variable-pitch ((t (:family "Libre Baskerville"))))

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
          org-ellipsis " ▼"
;;          org-pretty-entities t
          org-hide-emphasis-markers t
          org-return-follows-link t)
    (setq org-hide-emphasis-markers t)
    (setq org-src-fontify-natively t)
    (setq org-refile-use-outline-path t
          org-reverse-note-order nil)
    (setq org-tags-column -90)
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

    (setq org-priority-faces '((?A . (:background "#F0DFAF" :weight bold))
                               (?B . (:background "LightSteelBlue"))
                               (?C . (:background "OliveDrab"))))

    ;; refile
    (setq org-refile-targets '((nil :maxlevel . 4)
                               (org-agenda-files :maxlevel . 4)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    (setq org-tags-column -90)

    (setq org-agenda-span 14)
    (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)")))
    (add-hook 'org-mode-hook (lambda () (setq fill-column 100)))
    (add-hook 'org-mode-hook 'turn-on-auto-fill)

    (setq org-todo-keyword-faces
          '(("TODO" . org-warning)
            ("IN-PROGRESS" . "green4")
            ("WAITING" . "red")
            ("COMPLETE" . "blue")))

    (setq org-clock-persist t)
    (org-clock-persistence-insinuate)
    (setq org-outline-path-complete-in-steps 't)
    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


    ;; (defun jethro/style-org ()
    ;;   (setq line-spacing 0.2)
    ;;   (variable-pitch-mode +1)
    ;;   (mapc
    ;;    (lambda (face) ;; Other fonts with fixed-pitch.
    ;;      (set-face-attribute face nil :inherit 'fixed-pitch))
    ;;    (list 'org-code
    ;;          'org-link
    ;;          'org-block
    ;;          'org-table
    ;;          'org-verbatim
    ;;          'org-block-begin-line
    ;;          'org-block-end-line
    ;;          'org-meta-line
    ;;          'org-document-info-keyword)))

    ;; (add-hook 'org-mode-hook #'jethro/style-org)

    (org-link-set-parameters
     "search-view" :follow 'my-search-view-link)

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
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . t)
                                   (python . t)))))


(use-package org-bullets
  :ensure t
;;  :commands org-bullets-mode
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("●" "○" "♦" "♢")))

(use-package helm-org
  :after (hem org)
  :commands (helm-org-agenda-files-headings helm-org-in-buffer-headings helm-org-parent-headings
                                            helm-org-completing-read-tags)
  :config
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
  )

(use-package helm-org-rifle
  :after (helm org)
  :commands (helm-org-rifle helm-org-rifle-current-buffer helm-org-rifle-org-directory)
  :config
  (setq helm-org-rifle-show-path t))

(use-package org-ql
  :after org
  :commands (org-ql-search org-ql-view org-ql-view-sidebar org-ql-sparse-tree helm-org-ql org-ql-block)
  )

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

;; (use-package org-cliplink
;;   :bind
;;   ("C-c C" . 'jethro/org-capture-link)
;;   :config
;;   (defun jethro/org-capture-link ()
;;     "Captures a link, and stores it in inbox."
;;     (interactive)
;;     (org-capture nil "l")))

;; (use-package org-download
;;   :after org
;;   :bind
;;   (:map org-mode-map
;;         (("s-Y" . org-download-screenshot)
;;          ("s-y" . org-download-yank)))
;;   :config
;;   (if (memq window-system '(mac ns))
;;       (setq org-download-screenshot-method "screencapture -i %s")
;;     (setq org-download-screenshot-method "maim -s %s"))
;;   (defun my-org-download-method (link)
;;     "This is a helper function for org-download.
;; It creates a folder in the root directory (~/.org/img/) named after the
;; org filename (sans extension) and puts all images from that file in there.
;; Inspired by https://github.com/daviderestivo/emacs-config/blob/6086a7013020e19c0bc532770e9533b4fc549438/init.el#L701"
;;     (let ((filename
;;            (file-name-nondirectory
;;             (car (url-path-and-query
;;                   (url-generic-parse-url link)))))
;;           ;; Create folder name with current buffer name, and place in root dir
;;           (dirname (concat "./images/"
;;                            (replace-regexp-in-string " " "_" (downcase (file-name-base buffer-file-name))))))

;;       ;; Add timestamp to filename
;;       (setq filename-with-timestamp (format "%s%s.%s"
;;                                             (file-name-sans-extension filename)
;;                                             (format-time-string org-download-timestamp)
;;                                             (file-name-extension filename)))
;;       ;; Create folder if necessary
;;       (unless (file-exists-p dirname)
;;         (make-directory dirname))
;;       (expand-file-name filename-with-timestamp dirname)))
;;   (setq org-download-method 'my-org-download-method))

(use-package org-sidebar
  :after org
  :commands (org-sidebar-tree-toggle org-sidebar-toggle org-sidebar-ql)
  :custom (org-sidebar-tree-side 'left))

(setq package-check-signature nil)

;; token: 4/wAFO2rD1bj_Dga_2HeH4qfV6N0RzJjHd2tXysrjaQbpTmGOUwWQpYA0
(use-package org-gcal
  :ensure t
  :after org
  :commands (org-gcal-sync org-gcal-fetch)
  :config
  (setq org-gcal-client-id "577056606825-kfucdrv8lcvmc6ld6641h6c6ddf3iod7.apps.googleusercontent.com"
        org-gcal-client-secret "DtVI0DuOYvCZozCY5JkZG6O_"
        org-gcal-file-alist `(("edgar1denny@gmail.com" .  ,(concat org-directory "/gcal.org"))
                              ("59nii32e0b96o08lut9kc4mejrp5dinh@import.calendar.google.com". ,(concat org-directory "/cal.org"))))
  ;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
  )

(use-package org-roam
  :after org
  :hook
  ((org-mode . org-roam-mode)
   (after-init . org-roam--build-cache-async) ;; optional!
   )
  :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
  :custom
  (org-roam-directory (concat org-directory "/Roam"))
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph)
  :config
  (setq org-roam-buffer-width 0.4)
  (setq org-roam-link-title-format "{{%s}}")
  (setq org-roam-templates
        '(("default" (:file org-roam--file-name-timestamp-title
                            :content "#+TITLE: ${title} \n")))))

(provide 'org-setup)

;;; org-setup.el ends here
