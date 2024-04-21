;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; dired
;; ----------------------------------------------------------------------

(use-package dired
  :ensure nil
  :bind
  (("s-d d" . dired)
   ("s-d j" . dired-jump))
  :custom
  (dired-listing-switches "-laGh1v --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  ;; we want dired not not make always a new buffer if visiting a directory
  ;; but using only one dired buffer for all directories.
  :config
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  (defadvice dired-advertised-find-file (around dired-subst-directory activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-filename)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))
  ;; auto refresh dired when file changes
  :hook (dired-mode . auto-revert-mode))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))

(use-package dired-narrow
  ;; type 'g' to un-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; colorful dired
(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode 0))

(use-package dired-git-info
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package dired-sidebar
  :bind (("s-d s" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix " |")
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(provide 'init-dired)
;;; init-dired.el ends here
