;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; buffers
;; ----------------------------------------------------------------------

;; move a buffer to another window in a specified direction
(use-package buffer-move
  :bind
  (("s-b I" . buf-move-up)
   ("s-b K" . buf-move-down)
   ("s-b J" . buf-move-left)
   ("s-b L" . buf-move-right)))

(use-package ibuffer
  :ensure nil
  :bind
  (("s-b i" . ibuffer))
  :config
  (setq-default ibuffer-show-empty-filter-groups nil))

(use-package nerd-icons-ibuffer
  :config
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon t)
  (setq nerd-icons-ibuffer-icon-size 1.0)
  (setq nerd-icons-ibuffer-human-readable-size t)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ibuffer-sidebar
  :bind
  (("s-b s" . ibuffer-sidebar-toggle-sidebar)))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; ----------------------------------------------------------------------
;; files
;; ----------------------------------------------------------------------

(use-package files
  :ensure nil
  :bind
  (("s-f s" . save-buffer)
   ("s-f f" . find-file)
   ("s-f w" . write-file)))

(provide 'init-buffer)
;;; init-buffer.el ends here
