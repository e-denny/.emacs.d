;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; embark
;; ----------------------------------------------------------------------

(use-package embark
  :bind
  (("s-." . embark-act)
   (:map minibuffer-local-map
         ("s-." . embark-act)))
  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))


(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package wgrep
  :after (embark-consult ripgrep)
  :bind (:map wgrep-mode-map
              ;; Added keybinding to echo Magit behavior
              ("C-c C-c" . save-buffer)
              :map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              :map ripgrep-search-mode-map
              ("e" . wgrep-change-to-wgrep-mode)))


(provide 'init-embark)
;;; init-embark.el ends here
