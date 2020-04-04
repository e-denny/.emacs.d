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


;; (use-package elisp-mode
;;   :straight nil
;;   :bind (:map emacs-lisp-mode-map
;;               ("C-c C-x" . ielm)
;;               ("C-c C-c" . eval-defun)
;;               ("C-c C-k" . eval-buffer)))

;; (use-package smartparens
;;   :bind (:map smartparens-mode-map
;;               ("C-M-f" . sp-forward-sexp)
;;               ("C-M-b" . sp-backward-sexp)
;;               ("C-M-u" . sp-backward-up-sexp)
;;               ("C-M-d" . sp-down-sexp)
;;               ("C-M-p" . sp-backward-down-sexp)
;;               ("C-M-n" . sp-up-sexp)
;;               ("C-M-s" . sp-splice-sexp)
;;               ("C-M-<up>" . sp-splice-sexp-killing-backward)
;;               ("C-M-<down>" . sp-splice-sexp-killing-forward)
;;               ("C-M-r" . sp-splice-sexp-killing-around)
;;               ("C-)" . sp-forward-slurp-sexp)
;;               ("C-<right>" . sp-forward-slurp-sexp)
;;               ("C-}" . sp-forward-barf-sexp)
;;               ("C-<left>" . sp-forward-barf-sexp)
;;               ("C-(" . sp-backward-slurp-sexp)
;;               ("C-M-<left>" . sp-backward-slurp-sexp)
;;               ("C-{" . sp-backward-barf-sexp)
;;               ("C-M-<right>" . sp-backward-barf-sexp)
;;               ("M-S" . sp-split-sexp))
;;   :init
;;   (smartparens-global-strict-mode +1)
;;   :config
;;   (require 'smartparens-config)
;;   ;; Org-mode config
;;   (sp-with-modes 'org-mode
;;     (sp-local-pair "'" nil :unless '(sp-point-after-word-p))
;;     (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
;;     (sp-local-pair "_" "_" :unless '(sp-point-after-word-p))
;;     (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
;;     (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
;;     (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
;;     (sp-local-pair "«" "»")))

;; (use-package highlight-indent-guides
;;   :diminish highlight-indent-guides-mode
;;   :config
;;   (setq highlight-indent-guides-method 'character)
;;   ;; Indent character samples: | ┆ ┊
;;   (setq highlight-indent-guides-character ?\|)
;;   (setq highlight-indent-guides-responsive 'top
;;         highlight-indent-guides-auto-odd-face-perc 0
;;         highlight-indent-guides-auto-even-face-perc 0)
;;   ;; (setq highlight-indent-guides-auto-enabled nil)
;;   ;; (set-face-background 'highlight-indent-guides-odd-face "gray")
;;   ;; (set-face-background 'highlight-indent-guides-even-face "gray")
;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(provide 'elisp-coding-setup)

;;; elisp-coding-setup.el ends here
