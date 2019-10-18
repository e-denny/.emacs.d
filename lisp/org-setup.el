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
    (setq org-directory "~/Notes")
    (setq org-agenda-files (append
                            (file-expand-wildcards (concat org-directory "*/*.org"))
                            (file-expand-wildcards (concat org-directory "*/*/*.org"))))
    (setq org-src-fontify-natively t
          org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          org-src-preserve-indentation t)
    (setq org-use-speed-commands t)
    (setq org-startup-indented t
          org-startup-truncated nil
          org-ellipsis "....."
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-return-follows-link t)
    (setq org-hide-emphasis-markers t)
    (setq org-src-fontify-natively t)
    (setq org-capture-templates
          '(("t" "Todo [inbox]" entry
             (file+headline (concat org-directory "inbox.org") "Tasks")
             "* TODO %i%?")
            ("T" "Tickler" entry
             (file+headline (concat org-directory "tickler.org") "Tickler")
             "* %i%? \n %^t")))
    (setq org-refile-targets '((nil :maxlevel . 3)
                               (org-agenda-files :maxlevel . 3)))
    (setq org-refile-use-outline-path t
          org-reverse-note-order nil)
    (setq org-tags-column -90)
    (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w!)"  "NOTE(n)"  "|" "DONE(d)")))
    (add-hook 'org-mode-hook (lambda ()
                               (setq fill-column 100)))
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (setq org-todo-keyword-faces
          '(("TODO" . org-warning)
            ("IN-PROGRESS" . "green")
            ("WAITING" . "yellow")
            ("NOTE" . "orange")
            ("DONE" . "blue")))
    (setq org-agenda-custom-commands
          '(("@" "Contexts"
             ((tags-todo "@email"
                         ((org-agenda-overriding-header "Emails")))
              (tags-todo "@phone"
                         ((org-agenda-overriding-header "Phone")))))))
   (org-babel-do-load-languages 'org-babel-load-languages
                                '((emacs-lisp . t)
                                  (python . t)))))


(use-package org-bullets
  :ensure t
  :commands org-bullets-mode
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("⚫" "○" "◉" "◎" "✮" "✱" "✵")))


(provide 'org-setup)

;;; org-setup.el ends here
