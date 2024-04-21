;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; corfu
;; ----------------------------------------------------------------------

(use-package corfu
  :bind (:map corfu-map
              ("<escape>" . corfu-quit)
              ("<return>" . corfu-insert)
              ("M-l" . corfu-show-location)
              ("M-d" . corfu-show-documentation))
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-count 14)
  (corfu--goto)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  (corfu-scroll-margin 5)
  (corfu-preselect-first t)
  (corfu-min-width 60)
  (corfu-max-width corfu-min-width)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          (t :style "cod" :icon "code" :face font-lock-warning-face)))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package corfu-doc
;;   :load-path "lisp/corfu-doc"
;;   :after corfu
;;   :bind (:map corfu-map
;;               ([remap corfu-show-documentation] . corfu-doc-toggle)
;;               ("M-d" . corfu-doc-toggle)
;;               ("M-n" . corfu-doc-scroll-up)
;;               ("M-p" . corfu-doc-scroll-down))
;;   :custom
;;   (corfu-doc-delay 1.0)
;;   (corfu-doc-max-width 70)
;;   (corfu-doc-max-height 20)
;;   (corfu-echo-documentation nil)
;;   :hook (corfu-mode . corfu-doc-mode))


(use-package dabbrev
  :bind
  (("s-n c" . dabbrev-completion)
   ("s-n e" . dabbrev-expand)))


(use-package cape
  :bind
  (("s-n p" . completion-at-point)
   ("s-n d" . cape-dabbrev)
   ("s-n f" . cape-file)
   ("s-n s" . cape-symbol)
   ("s-n i" . cape-ispell))
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Use Company backends as Capfs.
  ;; (setq-local completion-at-point-functions
  ;;             (mapcar #'cape-company-to-capf
  ;;                     (list #'company-files #'company-ispell #'company-dabbrev)))
  )

;; ----------------------------------------------------------------------
;; hippie expand
;; ----------------------------------------------------------------------

(use-package hippie-exp
  :bind (("s-n h" . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(provide 'init-corfu)
;;; init-corfu.el ends here
