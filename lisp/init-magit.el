;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; magit
;; ----------------------------------------------------------------------

(use-package magit
  :general
  (my-leader-key
    "gs" 'magit-status
    "gd" 'magit-diff
    "gc" 'magit-commit
    "gu" 'magit-push))

(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (add-hook 'org-mode-hook 'git-gutter-mode)
  (my-leader-key
    "gn" 'git-gutter:next-hunk
    "gp" 'git-gutter:previous-hunk
    "go" 'git-gutter:popup-hunk
    "gr" 'git-gutter:revert-hunk)
  :custom
  (git-gutter:modified-sign ">")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :custom-face
  (git-gutter:modified ((t (:background "#c0b18b" :foreground "#2f2f2f"))))
  (git-gutter:added    ((t (:background "#84edb9" :foreground "#2f2f2f"))))
  (git-gutter:deleted  ((t (:background "#d75f5f" :foreground "#2f2f2f")))))

(provide 'init-magit)
;;; init-magit ends here
