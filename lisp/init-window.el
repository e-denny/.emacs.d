;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; window management
;; ----------------------------------------------------------------------

(use-package winum
  :init
  (setq-default winum-keymap nil)
  :config
  (winum-mode))

(use-package ace-window)

(use-package windmove
  :general
  (my-leader-key
    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up)
  :config
  (windmove-default-keybindings))

(use-package winner
  :general
  (my-leader-key
    "wu" 'winner-undo
    "wU" 'winner-redo)
  :config
  (winner-mode 1))

(use-package window
  :ensure nil
  :general
  (my-leader-key
    "wr" 'split-window-right
    "wb" 'split-window-below
    "w=" 'balance-windows
    "wd" 'delete-window
    "wo" 'other-window
    "wz" 'delete-other-windows
    "w+" 'wnlarge-window
    "w." 'enlarge-window-horizontally
    "w," 'shrink-window-horizontally))

(use-package ace-window
  :general
  (my-leader-key
    "wa" 'ace-window))

(provide 'init-window)
;;; init-window.el ends here
