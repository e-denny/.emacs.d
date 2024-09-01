;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; (require 'package)

(require 'cl-lib)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(add-hook 'after-init-hook #'elpaca-process-queues)

(elpaca `(,@elpaca-order))

;; make sure packages are in the load path
;; (package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t)
  (setq use-package-enable-imenu-support t))

(elpaca-wait)

(cl-pushnew 'org elpaca-ignored-dependencies)

(use-package org)

(elpaca-wait)

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

;; (diminish 'visual-line-mode "")
;; (diminish 'undo-tree-mode "")
;; (diminish 'auto-revert-mode "")
;; (diminish 'isearch-mode "?")
;; (diminish 'abbrev-mode "")

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
(require 'init-latex)
(require 'init-org)
(require 'init-help)
(require 'init-web)
(require 'init-fly)
(require 'init-mc-iedit)
(require 'init-eglot)
(require 'init-psession)
;; (require 'init-tabs)
(require 'init-restart-emacs)

(provide 'init)
;;; init.el ends here
