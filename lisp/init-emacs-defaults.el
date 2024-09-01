;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; defaults
;; ----------------------------------------------------------------------

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native complation is *not* available"))


;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq native-comp-async-report-warnings-errors nil)

(setq read-process-output-max (* 1024 1024)) ; 1MB

(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
;; (add-to-list 'default-frame-alist '(internal-border-width . 8))

(set-face-attribute 'default nil
                    :family "Jetbrains Mono" :height 135 :width 'expanded)
(set-face-attribute 'variable-pitch nil
                    :family "Jetbrains Mono" :height 135 :width 'expanded)
;; (set-face-attribute 'default nil
;;                     :family "Jetbrains Mono" :height 135 :weight 'medium)
;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro" :height 110 :weight 'medium)
;; (set-face-attribute 'variable-pitch nil
;;                     :font "Open Sans" :height 105)

;; disable customization
(setq custom-file (make-temp-file "emacs-custom-"))
(load custom-file)

(setq custom-safe-themes t)

(use-package emacs
  :ensure nil
  :config
  (blink-cursor-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (setq use-dialog-box nil)
  (setq use-file-dialog nil)
  ;; (menu-bar-mode -1)
  (tooltip-mode -1)
  (setq initial-scratch-message "")
  (setq inhibit-startup-screen t)
  (setq ring-bell-function 'ignore)
  (setq sentence-end-double-space nil)

  (setq-default cursor-type '(bar . 2))
  (setq-default cursor-in-non-selected-windows 'hollow)

  (put 'downcase-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)

  ;;kill running processes without confirmation on Emacs exit
  (setq confirm-kill-processes nil)

  (set-fringe-mode '(8 . 0))

  (fset 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t)
  (show-paren-mode 1)
  (column-number-mode 1)

  (global-hl-line-mode +1)
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq buffer-file-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)

  ;; focus help window
  (setq help-window-select t)

  (setq history-length t
        history-delete-duplicates t)
  (global-visual-line-mode 1)
  (setq-default indent-tabs-mode nil
                c-basic-offset 4
                tab-width 4
                tab-always-indent 'complete)
  ;; don't load old bype code
  (setq load-prefer-newer t)
  ;; auto revert mode
  (global-auto-revert-mode 1)
  ;; If a popup does happen, don't resize windows to be equal-sized
  (setq even-window-sizes nil)

  ;; (setq window-divider-default-right-width 12)
  ;; (setq window-divider-default-bottom-width 12)
  ;; (setq window-divider-default-places t)
  ;; (window-divider-mode 1)

  ;; (setq default-frame-alist
  ;;       (append (list '(internal-border-width . 12))))

  ;; smooth scrolling
  (setq scroll-margin 0
        scroll-conservatively 100
        scroll-preserve-screen-position t
        scroll-conservatively scroll-margin
        scroll-step 1
        mouse-wheel-scroll-amount '(6 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        scroll-error-top-bottom t
        next-error-recenter (quote (4)))

  (setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 20
        kept-old-versions 5)

  (add-hook 'prog-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

  :hook ((prog-mode . flymake-mode)
         (before-save . whitespace-cleanup)
         (text-mode . turn-on-visual-line-mode))
  :bind (("s-x" . execute-extended-command)
         ("s-u" . undo)
         ("s-b e" . eval-buffer)
         ("s-b d" . delete-buffer)
         ("<copy>" .  kill-ring-save)
         ("<paste>" . yank)
         ("<cut>" . kill-region)))


;; ----------------------------------------------------------------------
;; buffer placement
;; ----------------------------------------------------------------------

(setq display-buffer-alist
      `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.33)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

;; ----------------------------------------------------------------------
;; recentf
;; ----------------------------------------------------------------------

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 50)
  (recentf-max-menu-items 50)
  (recentf-exclude '("^/var/folders\\.*"
                     "COMMIT_EDITMSG\\'"
                     "/\\.git/.*\\'"
                     ".*-autoloads\\.el\\'"
                     "[/\\]\\.elpa/"))
  :config
  (recentf-mode 1))

;; ----------------------------------------------------------------------
;; savehist
;; ----------------------------------------------------------------------

(use-package savehist
  :ensure nil
  :init
  (progn
    (setq savehist-file "~/.emacs.d/savehist")
    (setq savehist-save-minibuffer-history 1)
    (setq savehist-additional-variables
          '(kill-ring
            search-ring
            regexp-search-ring))
    (savehist-mode 1)))

;; ----------------------------------------------------------------------
;; kill ring
;; ----------------------------------------------------------------------

(use-package browse-kill-ring
  :config (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(setq save-interprogram-paste-before-kill t)

;; ----------------------------------------------------------------------
;; proced
;; ----------------------------------------------------------------------

(use-package proced
  :ensure nil
  :commands proced
  :config
  (setq proced-auto-update-interval 1)
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

;; ----------------------------------------------------------------------
;; imenu
;; ----------------------------------------------------------------------

(use-package imenu
  :ensure nil
  :bind ("s-j i" . imenu))

;; ----------------------------------------------------------------------
;; bookmark
;; ----------------------------------------------------------------------

(use-package bookmark
  :ensure nil
  :bind (("s-m m" . consult-bookmark)
         ("s-m s" . bookmark-set)
         ("s-m l" . bookmark-bmenu-list)
         ("s-m d" . bookmark-delete)))

(use-package emacs
  :ensure nil
  :bind (("s-m f" . mark-defun)
         ("s-m s" . mark-sexp)))

(provide 'init-emacs-defaults)
;;; init-emacs-defaults.el ends here
