;;; eshell-setup.el --- Eshell Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Eshell Setup

;;; Code:

(use-package eshell
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
              (add-to-list 'eshell-visual-commands "htop"))))


;; TODO: add fish completion to eshell

(use-package eshell-git-prompt
  :ensure
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))


(provide 'eshell-setup)

;;; eshell-setup.el ends here
