;;; flycheck-setup.el --- Magit Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Magit Setup

;;; Code:

(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status))

;; | Key Binding | Command                  |
;; |-------------+--------------------------|
;; | C-x p       | git-gutter:previous-hunk |
;; | C-x n       | git-gutter:next-hunk     |
;; | C-x v =     | git-gutter:popup-hunk    |
;; | C-x v r     | git-gutter:revert-hunk   |

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (progn
    (add-hook 'prog-mode-hook 'git-gutter-mode)
    (add-hook 'org-mode-hook 'git-gutter-mode))
  :config
  (progn
    (custom-set-variables
     '(git-gutter:modified-sign ">")
     '(git-gutter:added-sign "+")
     '(git-gutter:deleted-sign "-"))
    (set-face-foreground 'git-gutter:deleted "#990A1B")
    (set-face-foreground 'git-gutter:modified "#00736F")
    (set-face-foreground 'git-gutter:added "#546E00")
    (set-face-background 'git-gutter:modified "none")
    (set-face-background 'git-gutter:added "none")
    (set-face-background 'git-gutter:deleted "none"))
  :bind (("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-x v =" . git-gutter:popup-hunk)
         ("C-x v r" . git-gutter:revert-hunk)))

(provide 'magit-setup)

;;; magit-setup.el ends here
