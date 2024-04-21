;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; common-lisp
;; ----------------------------------------------------------------------

(defvar sly-contribs '(sly-fancy))
(defvar inferior-lisp-program "sbcl")

(use-package sly
  :commands (sly)
  :init
  (sly-setup)
  :config
  ;; (add-hook 'sly-mode-hook
  ;;           (lambda ()
  ;;             (unless (sly-connected-p)
  ;;               (save-excursion (sly)))))
  (setq-default sly-symbol-completion-mode nil)
  (setq sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix
        ;; sly-complete-symbol-function 'sly-simple-completions
        )

  ;; FIXME: switch off orderless for sly - need to find better fix
  ;; (defun my-sly-completion ()
  ;;   (setq-local completion-at-point-functions
  ;;               (list
  ;;                (cape-company-to-capf
  ;;                 (apply-partially #'company--multi-backend-adapter
  ;;                                  '(sly-complete-symbol company-dabbrev)))))
  ;;   (setq-local completion-styles '(basic)))

  ;; (defun turn-off-sly-symbol-completion-mode ()
  ;;   (sly-symbol-completion-mode -1))

  (setq switch-to-buffer-obey-display-actions t) ; switch to new windows
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*sly-inspector"
  ;;                display-buffer-in-side-window
  ;;                (side . right)
  ;;                (slot . 2)
  ;;                (window-width . 100)))
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*sly-mrepl"
  ;;                display-buffer-in-side-window
  ;;                (side . right)
  ;;                (slot . 0)
  ;;                (window-width . 100)))
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*sly-db"
  ;;                display-buffer-in-side-window
  ;;                (side . right)
  ;;                (slot . 1)
  ;;                (window-width . 100)))
  :hook
  (
   ;; (sly-mode . turn-off-sly-symbol-completion-mode)
   ;; (sly-mode . my-sly-completion)
   (lisp-mode-hook . sly-editing-mode)))

(use-package sly-repl-ansi-color
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(provide 'init-common-lisp)
;;; init.el ends here
