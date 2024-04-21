;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; edgar theme
;; ----------------------------------------------------------------------

;; (use-package edgar-theme
;;   :straight (edgar-theme
;;              :local-repo "~/.emacs.d/lisp/edgar-theme"
;;              :type nil)
;;   :config
;;   (load-theme 'edgar t))

;; ----------------------------------------------------------------------
;; theme
;; ----------------------------------------------------------------------

(use-package rainbow-mode
  :bind
  (("C-M-s-t r" . rainbow-mode))
  :config
  (rainbow-mode 1))

;; (use-package modus-themes
;;   :init
;;   (setq modus-themes-bold-constructs t)
;;   (setq modus-themes-slanted-constructs t)
;;   (setq modus-themes-subtle-line-numbers t)
;;   (setq modus-themes-lang-checkers 'straight-underline)
;;   (setq modus-themes-fringes nil)
;;   (setq modus-themes-completions 'moderate)
;;   (setq modus-themes-org-blocks 'gray-background)
;;   (setq modus-themes-mode-line '(1 accented borderless))
;;   (setq modus-themes-prompts 'subtle)
;;   (setq modus-themes-syntax 'alt-syntax-yellow-comments)
;;   (modus-themes-load-themes)
;;   :config
;;   (setq modus-themes-vivendi-color-overrides
;;         '((bg-main . "gray10")
;;           (bg-hl-line . "gray20")
;;           (bg-hl-alt . "gray20")
;;           ))
;;   (modus-themes-load-vivendi))

(use-package ef-themes
  :config (load-theme 'ef-elea-dark :no-confirm))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))

(provide 'init-theme)
;;; init-theme.el ends here
