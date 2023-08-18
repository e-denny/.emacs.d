;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; common-lisp
;; ----------------------------------------------------------------------

(defvar sly-contribs '(sly-fancy))
(defvar inferior-lisp-program "sbcl")

;; TODO: set popup window rules
(use-package sly
  :commands (sly)
  :init
  (sly-setup)
  (evil-set-initial-state 'sly-mrepl-mode 'insert)
  :general
  (my-leader-key
    "ts" 'window-toggle-side-windows
    "ay" 'sly)
  (general-define-key
   :keymaps 'sly-mode-map
   :states 'normal
   "gz" 'sly-repl)
  (general-define-key
   :keymaps 'sly-mrepl-mode-map
   :states 'normal
   "gj" 'sly-mrepl-next-prompt
   "gk" 'sly-mrepl-previous-prompt)
  (general-define-key
   :keymaps 'sly-xref-mode-map
   :states 'normal
   "q" 'quit-window)
  (general-define-key
   :keymaps 'sly-trace-dialog-mode-map
   :state 'normal
   "q" 'quit-window)
  (general-define-key
   :keymaps 'sly-inspector-mode-map
   :states 'normal
   "<tab>" 'forward-button
   "<backtab>" 'backward-button
   "q" 'sly-inspector-quit)
  (general-define-key
   :keymaps 'sly-stickers--replay-mode-map
   :state 'normal
   "q" 'quit-window)
  (general-define-key
   :keymaps 'sly-db-mode-map
   :states 'normal
   "a" 'sly-db-abort
   "q" 'sly-db-quit
   "0" 'sly-db-invoke-restart-0
   "1" 'sly-db-invoke-restart-1
   "2" 'sly-db-invoke-restart-2
   "3" 'sly-db-invoke-restart-3
   "4" 'sly-db-invoke-restart-4
   "5" 'sly-db-invoke-restart-5
   "6" 'sly-db-invoke-restart-6
   "7" 'sly-db-invoke-restart-7
   "8" 'sly-db-invoke-restart-8
   "9" 'sly-db-invoke-restart-9)
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
  (add-to-list 'display-buffer-alist
               '("\\*sly-inspector"
                 display-buffer-in-side-window
                 (side . right)
                 (slot . 2)
                 (window-width . 100)))
  (add-to-list 'display-buffer-alist
               '("\\*sly-mrepl"
                 display-buffer-in-side-window
                 (side . right)
                 (slot . 0)
                 (window-width . 100)))
  (add-to-list 'display-buffer-alist
               '("\\*sly-db"
                 display-buffer-in-side-window
                 (side . right)
                 (slot . 1)
                 (window-width . 100)))
  :hook
  (
   ;; (sly-mode . turn-off-sly-symbol-completion-mode)
   ;; (sly-mode . my-sly-completion)
   (sly-mode . evil-normalize-keymaps)
   (lisp-mode-hook . sly-editing-mode)))


(use-package sly-repl-ansi-color
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(provide 'init-common-lisp)
;;; init.el ends here
