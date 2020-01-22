;;; which-key-setup.el --- Which-Key Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Which-Key Setup

;;; Code:

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-max-display-columns nil
        which-key-min-display-lines 5
        which-key-sort-order #'which-key-prefix-then-key-order
        which-key-add-column-padding 1)
  (which-key-mode +1))

;; (use-package which-key-posframe
;;   :config
;;   (setq which-key-posframe-parameters
;;         '((parent-frame . nil)
;;           (left-fringe . 5)
;;           (right-fringe . 5)))
;;   (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-left-corner)
;;   (which-key-posframe-mode))

(use-package free-keys
  :commands (free-keys)
  :bind ("C-~" . free-keys))

(provide 'which-key-setup)

;;; which-key-setup.el ends here
