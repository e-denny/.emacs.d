;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

(use-package smartparens
  :diminish smartparens-mode
  :hook ((emacs-lisp-mode . smartparens-mode)
         (lisp-interaction-mode . smartparens-mode)
         (lisp-mode . smartparens-mode)
         (ielm-mode . smartparens-mode)
         (eval-expression-minibuffer-setup . smartparens-mode))
  :bind (:map smartparens-mode-map
              ("s-c )" . sp-forward-slurp-sexp)
              ("s-c (" . sp-backward-slurp-sexp)
              ("s-c }" . sp-forward-barf-sexp)
              ("s-c {" . sp-backward-barf-sexp)))


(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  ;; (set-face-attribute 'hl-paren-face nil :background "gray92")
  (global-highlight-parentheses-mode)
  ;;make paren highlight update after stuff like paredit changes
  (add-to-list 'after-change-functions '(lambda (&rest x) (hl-paren-highlight))))


(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((emacs-lisp-mode scheme-mode lisp-mode) . aggressive-indent-mode))


(use-package lispy
  :hook ((lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (ielm-mode . lispy-mode))
  :config
  (setq lispy-close-quotes-at-end-p t)
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode)

  ;; (when (modulep! :lang emacs-lisp)
  ;;   (setq lispy-outline
  ;;         (concat
  ;;          ;; `lispy-mode' requires `lispy-outline' start with ^
  ;;          (unless (string-prefix-p "^" +emacs-lisp-outline-regexp) "^")
  ;;          +emacs-lisp-outline-regexp))
  ;;   (advice-add #'lispy-outline-level :override #'+emacs-lisp-outline-level))
  )

(provide 'init-lisp)
;;; init.el ends here
