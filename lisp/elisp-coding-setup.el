;;; elisp-coding-setup.el --- Elisp-Coding Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Elisp-Coding Setup

;;; Code:

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
    (setq eldoc-idle-delay 0.1)
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
    (add-hook 'ielm-mode-hook 'eldoc-mode)))

;; M-. navigate to symbol
;; M-, pop back to prevous marks
(use-package elisp-slime-nav
  :diminish
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-slime-nav-mode))

(use-package macrostep
  :ensure t
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-e" . macrostep-expand))
  (:map macrostep-keymap
        ("J" . macrostep-next-macro)
        ("K" . macrostep-prev-macro)
        ("e" . macrostep-expand)
        ("c" . macrostep-collapse)
        ("q" . macrostep-collapse-all)))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (progn
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
    (set-face-attribute 'hl-paren-face nil :background "gray92")
    (global-highlight-parentheses-mode)
    ;;make paren highlight update after stuff like paredit changes
    (add-to-list 'after-change-functions '(lambda (&rest x) (hl-paren-highlight)))))

(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character)
  ;; Indent character samples: | ┆ ┊
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-odd-face-perc 0
        highlight-indent-guides-auto-even-face-perc 0)
  ;; (setq highlight-indent-guides-auto-enabled nil)
  ;; (set-face-background 'highlight-indent-guides-odd-face "gray")
  ;; (set-face-background 'highlight-indent-guides-even-face "gray")
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(provide 'elisp-coding-setup)

;;; elisp-coding-setup.el ends here
