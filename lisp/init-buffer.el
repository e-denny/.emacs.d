;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; buffers
;; ----------------------------------------------------------------------

;; move a buffer to another window in a specified direction
(use-package buffer-move
  :config
  (my-leader-key
    "bk" 'buf-move-up
    "bj" 'buf-move-down
    "bh" 'buf-move-left
    "bl" 'buf-move-right))

(use-package ibuffer
  :config
  (my-leader-key
    "bi" 'ibuffer)
  (setq-default ibuffer-show-empty-filter-groups nil))


(use-package ibuffer-sidebar
  :init
  (my-leader-key
    "bs" 'ibuffer-sidebar-toggle-sidebar))

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
  :config
  (my-leader-key
    "fs" 'save-buffer
    "ff" 'find-file
    "fw" 'write-file))

(provide 'init-buffer)
;;; init-buffer.el ends here
