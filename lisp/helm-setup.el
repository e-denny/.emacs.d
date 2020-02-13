;;; helm-setup.el --- Setup of helm -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package helm
  :diminish helm-mode
  :commands
  (helm-M-x
   helm-find-files
   helm-mini
   helm-recentf
   helm-occur
   helm-info-at-point
   helm-show-kill-ring
   helm-buffers-list
   helm-semantic-or-imenu
   helm-locate
   helm-locate-library
   helm-apropos
   helm-color
   helm-surfraw
   helm-top
   helm-ucs
   helm-org-in-buffer-headings
   helm-minibuffer-history
   helm-man-woman)
  :config
  (progn
    (setq helm-scroll-amount 4
          helm-quick-update t
          helm-idle-delay 0.01
          helm-input-idle-delay 0.01
          helm-split-window-default-side 'other
          helm-split-window-in-side-p t
          helm-candidate-number-limit 200
          helm-move-to-line-cycle-in-source nil
          helm-M-x-requires-pattern 0
          helm-ff-file-name-history-use-recentf t
          helm-echo-input-in-header-line t
          helm-autoresize-max-height 0
          helm-autoresize-min-height 20
          helm-M-x-fuzzy-match t)
    (setq helm-imenu-fuzzy-match t)
    (setq helm-locate-fuzzy-match t)
    (setq helm-recentf-fuzzy-match t)
    (setq helm-semantic-fuzzy-match t)
    (setq helm-buffers-fuzzy-matching t)
    (helm-autoresize-mode 1)
    (helm-mode 1)))

(use-package helm-descbinds
  :after helm
  :commands helm-descbinds)

(use-package helm-swoop
  :after helm
  :config
  (setq helm-swoop-speed-or-color t)
  :commands (helm-swoop helm-swoop-without-pre-input helm-multi-swoop helm-multi-swoop-all))

(use-package helm-projectile
  :after (projectile helm)
  :commands (helm-projectile-find-file helm-projectile-switch-project helm-projectile-rg helm-projectile-switch-to-buffer)
  :config
  (helm-projectile-on))

(use-package helm-rg
  :after helm
  :commands helm-rg)

(use-package helm-xref
  :after helm
  :commands helm-xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-ls-git
  :after helm
  :commands helm-browse-project)

(setq my-browsers
      '(("Firefox" . browse-url-firefox)
        ("Chromium" . browse-url-chromium)
        ("EWW" . eww-browse-url)))

(defun my-browse-url (&rest args)
  "Select the prefered browser from a helm menu before opening the URL."
  (interactive)
  (let ((browser (or (helm :sources (helm-build-sync-source "WWW browsers"
                                                            :candidates (mapcar 'car my-browsers))
                           :buffer "*my browsers*")
                     (signal 'quit nil))))
    (apply (cdr (assoc browser my-browsers)) args)))

(setq browse-url-browser-function #'my-browse-url)

(provide 'helm-setup)
;;; helm-setup.el ends here
