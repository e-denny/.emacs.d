;;; defaults-setup.el --- Defaults Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Defaults Setup

;;; Code:

(when (equal system-type 'gnu/linux)
  (defun my/start-daemons ()
  "Start some daemons.  Hooks into `after-init-hook'."
  (start-process "" nil "nm-applet")
  (start-process "" nil "syncthing-gtk" "--minimized")
  (start-process "" nil "blueman-applet")
  (start-process "" nil "redshift-gtk"))

  (add-hook 'after-init-hook 'my/start-daemons))

(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(internal-border-width . 7))

(when (equal system-type 'windows-nt)
  (setq inhibit-compacting-font-caches t)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-b]))

(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(display-battery-mode 1)
(setq display-time-default-load-average nil)
(setq display-time-mail-file 'no)
(display-time-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode)

(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)

(setq history-length t
      history-delete-duplicates t)

(setq-default indent-tabs-mode nil
              tab-always-indent 'complete)
(setq tab-width 4)

;; remove whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; move to the place when file last visited
(setq-default save-place t)
(save-place-mode t)

;; smooth scrolling
(setq
  scroll-margin 2
  scroll-conservatively 10000
  scroll-conservatively scroll-margin
  scroll-step 1
  mouse-wheel-scroll-amount '(6 ((shift) . 1))
  mouse-wheel-progressive-speed nil
  scroll-preserve-screen-position t
  scroll-error-top-bottom t
  next-error-recenter (quote (4))
  fast-but-imprecise-scrolling nil
  jit-lock-defer-time 0)

; don't load old bype code
(setq load-prefer-newer t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
    )

;; (setq browse-url-browser-function 'eww-browse-url)

;; (setq gnus-button-url 'browse-url-generic
;;       browse-url-generic-program "firefox"
;;       browse-url-browser-function gnus-button-url)

(add-hook 'prog-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                                        '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

(defun my-beginning-of-line-dwim ()
  "Move point to first non-whitespace character, or beginning of line."
  (interactive "^")
  (let ((origin (point)))
    (beginning-of-line)
    (and (= origin (point))
         (back-to-indentation))))
;; smarter C-a
(global-set-key [remap move-beginning-of-line] #'my-beginning-of-line-dwim)


(provide 'defaults-setup)

;;; defaults-setup.el ends here
