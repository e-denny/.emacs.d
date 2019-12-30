;;; ivy-setup.el --- Setup of ivy -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst ivy-views-file "~/.emacs.d/ivy-views.el"
  "File in which ivy-views will be saved.")

(use-package ivy
  :diminish ivy-mode
  :ensure ivy-hydra
  :commands (ivy-switch-buffer ivy-push-view ivy-pop-view)
  :hook (emacs-startup . ivy-mode)
  ;;:init
  ;;(add-hook 'emacs-startup-hook #'ivy-mode)
  :config
  (setq ivy-height 12
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers t
        smex-completion-method 'ivy
        ivy-initial-inputs-alist nil
        ivy-format-function #'ivy-format-function-line
        ivy-magic-slash-non-match-action nil
        ivy-re-builders-alist '((t . ivy--regex-plus)))

  (defun my/save-ivy-views ()
    "Save ivy-views to file."
    (interactive)
    (with-temp-file ivy-views-file
      (prin1 ivy-views (current-buffer))
      (message "Saved ivy-views to file %s." ivy-views-file)))

  (defun my/load-ivy-views ()
    "Load ivy-views from file."
    (interactive)
    (if (file-exists-p ivy-views-file)
        (progn (setq ivy-views
                     (with-temp-buffer (insert-file-contents ivy-views-file)
                                       (read (current-buffer))))
               (message "Loaded ivy-views from file %s." ivy-views-file))
      (message "File %s does not exist!" ivy-views-file)))

  (my/load-ivy-views)
  )

(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

(use-package ivy-rich
  :commands (ivy-rich-switch-buffer-transformer)
  :hook (emacs-startup . ivy-rich-mode)
  :init
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev))

(use-package swiper
  :commands (swiper
             swiper-all))

(use-package counsel
  :config
  (defun my/counsel-rg-thing-at-point ()
    (interactive)
    (counsel-rg (format "%s" (let ((sym (thing-at-point 'symbol)))
                               (if sym sym "")))))

  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :commands (counsel-ag counsel-rg counsel-pt counsel-apropos counsel-bookmark
             counsel-describe-function counsel-describe-variable
             counsel-describe-face counsel-M-x counsel-file-jump
             counsel-find-file counsel-find-library counsel-info-lookup-symbol
             counsel-imenu counsel-recentf counsel-yank-pop
             counsel-descbinds counsel-org-capture counsel-grep-or-swiper))

;; `smex': Used by counsel-M-x
(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (smex-initialize))

(provide 'ivy-setup)
;;; ivy-setup.el ends here
