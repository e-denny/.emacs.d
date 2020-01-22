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
    (setq org-agenda-files (directory-files-recursively (concat org-directory "/") "\.org$"))
    (setq org-log-done 'time)
    (setq org-agenda-show-all-dates nil)
    (setq org-capture-templates
          `(("i" "Todo [inbox]" entry
             (file+headline ,(format "%s/%s" org-directory "Inbox.org") "Inbox")
             "* TODO %i%?")
            ("t" "todo" entry (file+headline ,(format "%s/%s" org-directory "Inbox.org") "Tasks")
             "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
            ("T" "Tickler" entry
             (file+headline ,(format "%s/%s" org-directory "Tickler.org") "Tickler")
             "* %i%? \n %^t")))
    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))
    (setq org-return-follows-link t)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
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

    (setq org-agenda-span 'fortnight)
    (setq org-refile-use-outline-path t)
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

    (setq org-agenda-custom-commands
          '(("n" "Agenda"
             ((agenda ""
                      ((org-agenda-overriding-header "Scheduled")))
              (todo "TODO"
                    ((org-agenda-overriding-header "Unscheduled")
                     (org-agenda-skip-function
                      (quote
                       (org-agenda-skip-entry-if
                        (quote scheduled)))))))
             nil nil)))

    (setq org-clock-persist t)
    (org-clock-persistence-insinuate)
    (setq org-outline-path-complete-in-steps 't)
    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

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
  :commands org-bullets-mode
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("⚫" "○" "◉" "◎" "✮" "✱" "✵")))


(provide 'org-setup)

;;; org-setup.el ends here
