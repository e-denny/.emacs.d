;;; window-management-setup.el --- Window-Management Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Window-Management Setup

;;; Code:

(use-package winum
  :ensure t
  :init
  (setq-default winum-keymap nil)
  :config
  (winum-mode))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (progn
    (setq beacon-blink-when-point-moves-vertically nil)
    (setq beacon-blink-when-point-moves-horizontally nil)
    (setq beacon-blink-when-buffer-changes t)
    (setq beacon-blink-when-window-scrolls t)
    (setq beacon-blink-when-window-changes t)
    (setq beacon-blink-when-focused nil)

    (setq beacon-blink-duration 0.3)
    (setq beacon-blink-delay 0.3)
    (setq beacon-size 20)
    (setq beacon-color "grey")
    (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
    (add-to-list 'beacon-dont-blink-major-modes 'inferior-python-mode)
    (beacon-mode 1)))

(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.15)
  (setq dimmer-adjustment-mode :both)
  (dimmer-mode 1))


;; move a buffer to another window in a specified direction
(use-package buffer-move
  :ensure t
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right))

;; undo a change to a window configuration
(use-package winner
  :ensure t
  :init
  (winner-mode 1))

(provide 'window-management-setup)

;;; window-management-setup.el ends here
