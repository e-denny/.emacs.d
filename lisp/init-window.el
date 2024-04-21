;;; init-window.el  --- User-init-file   -*- lexical-binding: t -*-

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
  :bind
  (("s-w l" . windmove-right)
   ("s-w j" . windmove-left)
   ("s-w k" . windmove-down)
   ("s-w i" . windmove-up))
  :config
  (windmove-default-keybindings))

(use-package winner
  :bind
  (("s-w u" . winner-undo)
   ("s-w U" . winner-redo))
  :config
  (winner-mode 1))

(use-package window
  :ensure nil
  :bind
  (("s-w r" . split-window-right)
   ("s-w b" . split-window-below)
   ("s-w =" . balance-windows)
   ("s-w d" . delete-window)
   ("s-w o" . other-window)
   ("s-w z" . delete-other-windows)
   ("s-w +" . enlarge-window)
   ("s-w ." . enlarge-window-horizontally)
   ("s-w ," . shrink-window-horizontally)))

(use-package ace-window
  :bind
  (("s-w a" . ace-window)))

(use-package selected-window-accent-mode
  :config
  (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  ;; (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'subtle))



(provide 'init-window)
;;; init-window.el ends here
