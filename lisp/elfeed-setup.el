;;; elfeed-setup.el --- Elfeed Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Elfeed Setup

;;; Code:


(use-package elfeed
  :defer t
  :ensure t
  :preface
  (progn
    (setq elfeed-db-directory
          (let* ((db-dir-name (format "elfeed_db_%s" "2.6.1"))
                 (dir (file-name-as-directory (expand-file-name db-dir-name user-emacs-directory))))
            (make-directory dir :parents)
            dir)))
  :config
  (progn
    (setq elfeed-feeds
          '(;; emacs
            ("http://stackexchange.com/feeds/tagsets/152198/emacs?sort=active" emacs)
            ("http://nullprogram.com/feed/" emacs)
            ("http://planet.emacsen.org/atom.xml" emacs)))
    ;; Entries older than 4 weeks are marked as read
    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :before "4 weeks ago"
                                  :remove 'unread))))

(use-package elfeed-goodies
  :ensure t
  :config
  ;; (elfeed-goodies/set)
  )

(provide 'elfeed-setup)

;;; elfeed-setup.el ends here
