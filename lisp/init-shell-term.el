;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; eshell
;; ----------------------------------------------------------------------

(use-package eshell
  :commands (eshell)
  :bind
  (("s-a s" . eshell))
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "e" "find-file $1")
              (eshell/alias "ll" "ls -ltra --color=always")
              (eshell/alias "d" "dired $1")))
  (add-hook 'eshell-mode-hook
            (lambda()
              ;; programs that don't work well in eshell and should be run in visual mode
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "pacman")
              (add-to-list 'eshell-visual-commands "htop"))))

;; ----------------------------------------------------------------------
;; vterm
;; ----------------------------------------------------------------------

(use-package vterm
  ;; prevent hl-line in vterm
  :commands (vterm)
  :bind
  (("s-a v" . vterm))
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil))))



(use-package eat
  :commands (eat)
  :bind
  (("s-a t" . eat))
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eat--semi-char-mode
            (lambda ()
              (make-local-variable 'scroll-margin)
              (setq scroll-margin 0)))
  ;; (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  )


(provide 'init-shell-term)

;;; init-shell-term.el ends here
