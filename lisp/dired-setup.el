;;; dired-setup.el --- Dired Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Dired Setup

;;; Code:

(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-jump-other-window)
  :config
  (setq dired-listing-switches "-laGh1v --group-directories-first")
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  ;; we want dired not not make always a new buffer if visiting a directory
  ;; but using only one dired buffer for all directories.
  (defadvice dired-advertised-find-file (around dired-subst-directory activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-filename)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig)))))


;; FIXME: this does not seem to work
(defun dired-dotfiles-toggle ()
  "Show/hide dot-files."
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

; narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(provide 'dired-setup)

;;; dired-setup.el ends here
