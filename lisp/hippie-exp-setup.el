;;; hippie-exp-setup.el --- Hippie-Exp Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Hippie-Exp Setup

;;; Code:

(use-package hippie-exp
  :bind (("M-/" . hippie-expand))
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

(provide 'hippie-exp-setup)

;;; hippie-exp-setup.el ends here
