;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; my commands
;; ----------------------------------------------------------------------

(use-package emacs
  :config

  (defun my/next-begin-sexp ()
    (interactive)
    (forward-char 1)
    (search-forward "(" nil t)
    (backward-char 1))

  (defun my/prev-begin-sexp ()
    (interactive)
    (search-backward "(" nil t))

  (defun my/mark-sexp ()
    (interactive)
    (when (and mark-active (looking-at-p "(.*"))
      (backward-up-list))
    (unless (looking-at-p "(.*")
      (my/prev-begin-sexp))
    (mark-sexp))

  (defun my/kill-sexp ()
    (interactive)
    (unless (looking-at-p "(.*")
      (my/prev-begin-sexp))
    (kill-sexp))

  (defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

  :bind (("s-c l" . my/next-begin-sexp)
         ("s-c h" . my/prev-begin-sexp)
         ("s-c m" . my/mark-sexp)
         ("s-c k" . my/kill-sexp)))

(provide 'init-funcs)
;;; init-funcs.el ends here
