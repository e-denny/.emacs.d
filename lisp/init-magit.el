;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; magit
;; ----------------------------------------------------------------------

(use-package magit
  :bind
  (("s-v s" . magit-status)
   ("s-v d" . magit-diff)
   ("s-v c" . magit-commit)
   ("s-v u" . magit-push)))

(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (add-hook 'org-mode-hook 'git-gutter-mode)
  :bind
  (("s-v n" . git-gutter:next-hunk)
   ("s-v p" . git-gutter:previous-hunk)
   ("s-v o" . git-gutter:popup-hunk)
   ("s-v r" . git-gutter:revert-hunk))
  :custom
  (git-gutter:modified-sign ">")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :custom-face
  (git-gutter:modified ((t (:background "#c0b18b" :foreground "#2f2f2f"))))
  (git-gutter:added    ((t (:background "#84edb9" :foreground "#2f2f2f"))))
  (git-gutter:deleted  ((t (:background "#d75f5f" :foreground "#2f2f2f")))))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(provide 'init-magit)
;;; init-magit ends here
