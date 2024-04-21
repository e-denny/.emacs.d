;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

(require 'package)
(require 'cl-lib)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

;; make sure packages are in the load path
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)

(use-package git)
(setq use-package-verbose t)

(customize-set-variable 'use-package-compute-statistics t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; ----------------------------------------------------------------------
;; garbage-collection
;; ----------------------------------------------------------------------

(defvar my-gc 100000000) ; 100MB

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold my-gc
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; Run GC every 60 seconds if emacs is idle.
(run-with-idle-timer 60.0 t #'garbage-collect)

(defun my-gc-minibuf ()
  "Prevent garbage collection while in the minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-gc-restore ()
  "Restore garbage collection after leaving minibuffer."
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold my-gc))))

(add-hook 'minibuffer-setup-hook #'my-gc-minibuf)
(add-hook 'minibuffer-exit-hook #'my-gc-restore)

;; ----------------------------------------------------------------------

;; (load "server")
;; (unless (server-running-p)
;;   (server-start))

;; ----------------------------------------------------------------------

(use-package diminish
  :after use-package)

(diminish 'visual-line-mode "")
(diminish 'undo-tree-mode "")
(diminish 'auto-revert-mode "")
(diminish 'isearch-mode "?")
(diminish 'abbrev-mode "")

(require 'init-which-key)
(require 'init-emacs-defaults)
(require 'init-corfu)
(require 'init-avy)
(require 'init-dired)
(require 'init-elisp)
(require 'init-lisp)
(require 'init-common-lisp)
(require 'init-shell-term)
(require 'init-vertico-consult)
(require 'init-embark)
(require 'init-hydra)
(require 'init-theme)
(require 'init-window)
(require 'init-buffer)
(require 'init-yasnippet)
(require 'init-funcs)
(require 'init-magit)
(require 'init-project)
(require 'init-denote)
(require 'init-org)
(require 'init-help)
(require 'init-web)
(require 'init-fly)
(require 'init-mc-iedit)
(require 'init-eglot)
(require 'init-psession)
(require 'init-tabs)
(require 'init-restart-emacs)

(provide 'init)
;;; init.el ends here
