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
    (setq beacon-color "orange")
    (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
    (add-to-list 'beacon-dont-blink-major-modes 'inferior-python-mode)
    (beacon-mode 1)))

(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.25)
  (dimmer-mode 1))


;; move a buffer to another window in a specified direction
(use-package buffer-move
  :ensure t
  :bind (("C-c <s-up>" . buf-move-up)
         ("C-c <s-down>" . buf-move-down)
         ("C-c <s-left>" . buf-move-left)
         ("C-c <s-right>" . buf-move-right)))

;; undo a change to a window configuration
(use-package winner
  :ensure t
  :init
  (winner-mode 1))

;; unset C- and M- digit keys
;; (dotimes (n 10)
;;   (global-unset-key (kbd (format "C-%d" n)))
;;   (global-unset-key (kbd (format "M-%d" n))))

;; (use-package eyebrowse
;;   :ensure t
;;   :diminish eyebrowse-mode
;;   :config (progn
;;             (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
;;             (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
;;             (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
;;             (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
;;             (eyebrowse-mode t)
;;             (setq eyebrowse-new-workspace t)))

(provide 'window-management-setup)

;;; window-management-setup.el ends here
