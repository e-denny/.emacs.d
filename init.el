;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'package)
(setq package-archives
             '(("gnu" . "https://elpa.gnu.org/packages/")
               ("melpa" . "https://melpa.org/packages/")
               ("org" . "https://orgmode.org/elpa")))
(package-initialize)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(eval-when-compile
  (defvar use-package-enable-imenu-support)
  (require 'use-package))

(customize-set-variable 'use-package-compute-statistics t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; ignore custom file - different every time
(setq custom-file (make-temp-file ""))
(setq custom-safe-themes t)
(load custom-file)

;; install use-package with straight
(straight-use-package 'use-package)

(use-package diminish
  :straight t)

(diminish 'visual-line-mode "")
(diminish 'undo-tree-mode "")
(diminish 'auto-revert-mode "")
(diminish 'isearch-mode "?")

;; ----------------------------------------------------------------------
;; defaults
;; ----------------------------------------------------------------------

;; (require 'epa)
;;   (epa-file-enable)
;;   (setq epg-gpg-program "gpg")

;;   (load-if-exists "~/.emacs.d/secrets.el.gpg")
;;   (load-if-exists "~/.emacs.d/secrets.el")

(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(internal-border-width . 8))

(set-face-attribute 'default nil
                    :family "Hack"
                    :height 120
                    :weight 'normal
                    :width 'normal)

(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(set-fringe-mode '(8 . 0))

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(column-number-mode 1)

(global-hl-line-mode +1)

;; show line numbers for program buffers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; start flymake mode
(add-hook 'prog-mode-hook 'flymake-mode)

(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)

(setq history-length t
      history-delete-duplicates t)

(setq-default indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4
              tab-always-indent 'complete)

;; remove whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; move to the place when file last visited
;; FIXME: this doesn't work
;; (setq-default save-place t)
;; (save-place-mode t)

;; smooth scrolling
(setq
  scroll-margin 3
  scroll-conservatively 1
  scroll-preserve-screen-position t
  scroll-conservatively scroll-margin
  scroll-step 1
  mouse-wheel-scroll-amount '(6 ((shift) . 1))
  mouse-wheel-progressive-speed nil
  scroll-error-top-bottom t
  next-error-recenter (quote (4))
;;  fast-but-imprecise-scrolling nil
;;  jit-lock-defer-time 0
  )

; don't load old bype code
(setq load-prefer-newer t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
    )

(add-hook 'prog-mode-hook
               (lambda ()
                 (font-lock-add-keywords
                  nil
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

;; auto revert mode
(global-auto-revert-mode 1)

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; ----------------------------------------------------------------------
;; avy
;; ----------------------------------------------------------------------

(use-package avy
  :straight t
  :commands (avy-goto-word-or-subword-1 avy-goto-char avy-goto-char-in-line)
  :config
  (setq avy-background t))

;; ----------------------------------------------------------------------
;; kill ring
;; ----------------------------------------------------------------------

(use-package browse-kill-ring
  :straight t
  :bind ("C-x C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(setq save-interprogram-paste-before-kill t)

;; ----------------------------------------------------------------------
;; company
;; ----------------------------------------------------------------------

(use-package company
  :straight t
  :defer t
  ;; :init (global-company-mode)
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 15
        company-show-numbers nil
        company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)
  ;; (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  :diminish company-mode)

;; company with icons
(use-package company-box
  :straight t
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

;; ----------------------------------------------------------------------
;; dired
;; ----------------------------------------------------------------------

(use-package dired
  :commands (dired dired-jump dired-jump-other-window)
  :config
  (setq dired-listing-switches "-laGh1v --group-directories-first")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  ;; we want dired not not make always a new buffer if visiting a directory
  ;; but using only one dired buffer for all directories.
  (defadvice dired-advertised-find-file (around dired-subst-directory activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-filename)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig)))))

(use-package dired-subtree
  :straight t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

; narrow dired to match filter
(use-package dired-narrow
  :straight t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package vscode-icon
  :straight t
  :commands (vscode-icon-for-file))

(use-package dired-sidebar
  :straight t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
;; (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; colorful dired
(use-package diredfl
  :straight t
    :config
    (diredfl-global-mode 0))

;; show git logs
(use-package dired-git-info
  :straight t
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))

;; ----------------------------------------------------------------------
;; elisp
;; ----------------------------------------------------------------------

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
  :straight t
  :diminish
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-slime-nav-mode))

(use-package macrostep
  :straight t
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
  :straight t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package highlight-parentheses
  :straight t
  :diminish highlight-parentheses-mode
  :config
  (progn
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
    ;; (set-face-attribute 'hl-paren-face nil :background "gray92")
    (global-highlight-parentheses-mode)
    ;;make paren highlight update after stuff like paredit changes
    (add-to-list 'after-change-functions '(lambda (&rest x) (hl-paren-highlight)))))

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

;; ----------------------------------------------------------------------
;; eshell
;; ----------------------------------------------------------------------

(use-package eshell
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "e" "find-file $1")
              (eshell/alias "ll" "ls -ltra --color=always")
              (eshell/alias "d" "dired $1")))
  (add-hook 'eshell-mode-hook
            (lambda()
              ;; programs that don't work well in eshell and should be run in visual mode
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "htop"))))

;; ----------------------------------------------------------------------
;; exwm
;; ----------------------------------------------------------------------

(when (eq window-system 'x)
  (use-package exwm
  :straight t
    :disabled
    :config
    (require 'exwm-config)
    (server-start)
    (setq exwm-workspace-number 1)
    ;; class name the buffer name
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (exwm-workspace-rename-buffer exwm-class-name)))
    (add-hook 'exwm-update-title-hook
              (lambda () (exwm-workspace-rename-buffer exwm-title)))
    (add-hook 'exwm-exit-hook
              (lambda ()
                (shell-command "killall xfce-session")))
    ;; Global keybindings.
    (unless (get 'exwm-input-global-keys 'saved-value)
      (setq exwm-input-global-keys
            `(([?\s-r] . exwm-reset)
              ;; ([?\s-w] . exwm-workspace-switch)
              ;; 's-N': Switch to certain workspace.
              ,@(mapcar (lambda (i)
                          `(,(kbd (format "s-%d" i)) .
                            (lambda ()
                              (interactive)
                              (exwm-workspace-switch-create ,i))))
                        (number-sequence 0 3)))))
    ;; ;; Firefox: open everything in new windows
    ;; 1: about:config -> browser.tabs.opentabfor.middleclick -> false
    ;; 2: place the following in chrome/userChrome.css in your FF profile:
    ;;    #tabbrowser-tabs { visibility: collapse !important; }
    (unless (get 'exwm-input-simulation-keys 'saved-value)
      (setq exwm-input-simulation-keys
            '(
              ;; movement - firefox
              ([?\C-b] . [left])
              ([?\C-f] . [right])
              ([?\C-p] . [up])
              ([?\C-n] . [down])
              ([?\C-a] . [home]) ; top of page
              ([?\C-e] . [end]) ; bottom of page
              ([?\M-v] . [prior])
              ([?\C-v] . [next])
              ([?\C-d] . [delete])
              ([?\C-k] . [S-end delete])
              ([?\C-w] . [?\C-x])
              ([?\M-w] . [?\C-c])
              ([?\C-y] . [?\C-v])
              ([?\C-s] . [?\C-f]) ; search
              ([?\C-g] . [?\C-g]) ; search again
              ([?\C-r] . [?\C-\M-r]) ; reader mode
              ([?\C-j] . [?\C-d]) ; toggle search bar / page
              ([?\C-W] . [?\C-\S-w]) ; close window
              ([?\M-n] . [?\C-n]) ; new window
              ;; Firefox extension - (Saka Key) - link hints
              ;;  - always open in new window 'fn' (not tab)
              ;; Firefox: focus search bar: C-l
              )))
    (setq save-interprogram-paste-before-kill t)

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)

    (exwm-enable)
    (exwm-config-misc)

    (require 'exwm-randr)
    ;;  (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "DP-1"))
    ;;  (add-hook 'exwm-randr-screen-change-hook 'my-change-screen-hook)
    (exwm-randr-enable)))

;; ----------------------------------------------------------------------
;; eyebrowse
;; ----------------------------------------------------------------------

(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t)
)

;; ----------------------------------------------------------------------
;; flycheck
;; ----------------------------------------------------------------------

(use-package flycheck
  :straight t
  :disabled t
  :diminish " ✓"
  :commands global-flycheck-mode
  :init (add-hook 'prog-mode-hook 'global-flycheck-mode)
  :config
  (progn
    (setq-default flycheck-emacs-lisp-initialize-packages t
                  flycheck-highlighting-mode 'lines
                  flycheck-display-errors-delay 0.3
                  flycheck-check-syntax-automatically '(save)
                  flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
    ;; Define fringe indicator / warning levels
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'infoq
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-info)
    (custom-set-variables
     '(flycheck-python-flake8-executable "python3")
     '(flycheck-python-pycompile-executable "python3")
     '(flycheck-python-pylint-executable "python3"))))

;; ----------------------------------------------------------------------
;; evil
;; ----------------------------------------------------------------------

(use-package evil
  :straight t
  :config

  (evil-mode 1)
  (use-package evil-leader
  :straight t
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")))

(use-package evil-surround
  :straight t
  :after evil
  :config (global-evil-surround-mode))

(use-package evil-indent-textobject
  :straight t
  :after evil)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; (defun exit-insert-save ()
;;   (if (buffer-file-name)
;;     (evil-save (buffer-file-name))))

;; (add-hook 'evil-insert-state-exit-hook 'exit-insert-save)

(define-key evil-normal-state-map (kbd "DEL") (lambda ()
                                               (interactive)
                                               (previous-line 10)
                                               (evil-scroll-line-up 10)))

(define-key evil-normal-state-map (kbd "=") (lambda ()
                                              (interactive)
                                              (next-line 10)
                                              (evil-scroll-line-down 10)))

;; ----------------------------------------------------------------------
;; helm
;; ----------------------------------------------------------------------

(use-package helm
  :straight t
  :diminish helm-mode
  :commands
  (helm-M-x
   helm-find-files
   helm-mini
   helm-recentf
   helm-occur
   helm-info-at-point
   helm-show-kill-ring
   helm-buffers-list
   helm-semantic-or-imenu
   helm-locate
   helm-locate-library
   helm-apropos
   helm-color
   helm-surfraw
   helm-top
   helm-ucs
   helm-org-in-buffer-headings
   helm-minibuffer-history
   helm-man-woman)
  :config
  (progn
    (setq helm-scroll-amount 4
          helm-quick-update t
          helm-idle-delay 0.01
          helm-input-idle-delay 0.01
         helm-split-window-default-side 'other
          helm-split-window-in-side-p t
          helm-candidate-number-limit 200
          helm-move-to-line-cycle-in-source nil
          helm-M-x-requires-pattern 0
          helm-ff-file-name-history-use-recentf t
          helm-echo-input-in-header-line t
          helm-autoresize-max-height 40
          helm-autoresize-min-height 40
          helm-M-x-fuzzy-match t)
    (setq helm-imenu-fuzzy-match t)
    (setq helm-locate-fuzzy-match t)
    (setq helm-recentf-fuzzy-match t)
    (setq helm-semantic-fuzzy-match t)
    (setq helm-buffers-fuzzy-matching t)

    (setq helm-grep-ag-command (concat "rg"
                                       ;; " --color=always"
                                       " --smart-case"
                                       " --no-heading"
                                       " --line-number %s %s %s")
      helm-grep-file-path-style 'relative)

    ;; hide helm sources lines
    (set-face-attribute 'helm-source-header nil :height 0.1)
    (helm-autoresize-mode 1)

    ;; should use :bind instead
    ;; use tab for completion
    (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
    (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") #'helm-select-action)

    (helm-mode 1)))

(use-package helm-descbinds
  :straight t
  :after helm
  :commands helm-descbinds)

(use-package helm-swoop
  :straight t
  :after helm
  :config
  (setq helm-swoop-speed-or-color t)
  :commands (helm-swoop helm-swoop-without-pre-input helm-multi-swoop helm-multi-swoop-all))

(use-package helm-projectile
  :straight t
  :after (projectile helm)
  :commands (helm-projectile-find-file helm-projectile-switch-project
                                       helm-projectile-rg helm-projectile-switch-to-buffer)
  :config
  (helm-projectile-on))

(use-package helm-rg
  :straight t
  :after helm
  :commands helm-rg)

(use-package helm-xref
  :straight t
  :after helm
  :commands helm-xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-ls-git
  :straight t
  :after helm
  :commands helm-browse-project)

(setq my-browsers
      '(("Firefox" . browse-url-firefox)
        ("Chromium" . browse-url-chromium)
        ("EWW" . eww-browse-url)))

(defun my-browse-url (&rest args)
  "Select the prefered browser from a helm menu before opening the URL."
  (interactive)
  (let ((browser (or (helm :sources (helm-build-sync-source "WWW browsers"
                                                            :candidates (mapcar 'car my-browsers))
                           :buffer "*my browsers*")
                     (signal 'quit nil))))
    (apply (cdr (assoc browser my-browsers)) args)))

(setq browse-url-browser-function #'my-browse-url)

;; ----------------------------------------------------------------------
;; shrface expand
;; ----------------------------------------------------------------------

(use-package shrface
  :straight t
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t))

(use-package eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

;; ----------------------------------------------------------------------
;; hippie expand
;; ----------------------------------------------------------------------

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

;; ----------------------------------------------------------------------
;; ivy
;; ----------------------------------------------------------------------

(use-package ivy
  :straight t
  :diminish ivy-mode
  :commands (ivy-switch-buffer ivy-push-view ivy-pop-view)
  :hook (emacs-startup . ivy-mode)
  :config
  (setq ivy-height 12
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer nil
        ivy-use-virtual-buffers t
        smex-completion-method 'ivy
        ivy-initial-inputs-alist nil
        ivy-format-function #'ivy-format-function-line
        ivy-magic-slash-non-match-action nil
        ivy-re-builders-alist '((t . ivy--regex-plus))))

(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

(use-package swiper
  :straight t
  :commands (swiper
             swiper-all))

(use-package counsel
  :straight t
  :config
  (defun my/counsel-rg-thing-at-point ()
    (interactive)
    (counsel-rg (format "%s" (let ((sym (thing-at-point 'symbol)))
                               (if sym sym "")))))

  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :commands (counsel-ag counsel-rg counsel-pt counsel-apropos counsel-bookmark
             counsel-describe-function counsel-describe-variable
             counsel-describe-face counsel-M-x counsel-file-jump
             counsel-find-file counsel-find-library counsel-info-lookup-symbol
             counsel-imenu counsel-recentf counsel-yank-pop
             counsel-descbinds counsel-org-capture counsel-grep-or-swiper))

;; `smex': Used by counsel-M-x
(use-package smex
  :straight t
  :commands (smex smex-major-mode-commands)
  :config
  (smex-initialize))

;; ----------------------------------------------------------------------
;; hydra
;; ----------------------------------------------------------------------

(use-package hydra
  :straight t)

;; ----------------------------------------------------------------------
;; eglot
;; ----------------------------------------------------------------------

(use-package ccls
  :straight t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls))))

(use-package eglot
  :straight t
  :bind (:map eglot-mode-map
              ("C-c C-d" . eglot-help-at-point)
              ("M-." . xref-find-definitions)
              ("C-c C-r" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) "ccls"))
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'python-mode #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure))

;; ----------------------------------------------------------------------
;; lsp
;; ----------------------------------------------------------------------

(use-package lsp-mode
  :straight t
  :disabled t
;;  :commands lsp
  :config
  (setq lsp-auto-guess-root t
        lsp-file-watch-threshold 500
        lsp-auto-configure nil
        lsp-prefer-flymake t)
  :hook ((c-mode c++-mode java-mode python-mode) . lsp))

(use-package lsp-ui
  :straight t
  :disabled t
  :after lsp-mode
;;  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-delay -1)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package dap-mode
  :straight t
  :disabled t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; ----------------------------------------------------------------------
;; magit
;; ----------------------------------------------------------------------

(use-package magit
  :straight t
  :bind ("C-c m" . magit-status))

(use-package git-gutter
  :straight t
  :diminish git-gutter-mode
  :init
  (progn
    (add-hook 'prog-mode-hook 'git-gutter-mode)
    (add-hook 'org-mode-hook 'git-gutter-mode))
  :config
  (progn
    (custom-set-variables
     '(git-gutter:modified-sign ">")
     '(git-gutter:added-sign "+")
     '(git-gutter:deleted-sign "-"))
    (set-face-foreground 'git-gutter:deleted "#990A1B")
    (set-face-foreground 'git-gutter:modified "#00736F")
    (set-face-foreground 'git-gutter:added "#546E00")
    (set-face-background 'git-gutter:modified "none")
    (set-face-background 'git-gutter:added "none")
    (set-face-background 'git-gutter:deleted "none"))
)

;; exwm fix to get magit-ediff to work
(use-package ediff
  :config (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

;; ----------------------------------------------------------------------
;; modeline
;; ----------------------------------------------------------------------

(use-package smart-mode-line
  :straight t
  :config
  (setq sml/name-width 40)
  (setq sml/mode-width 'full)
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
  (sml/setup))

;; ----------------------------------------------------------------------
;; mu4e
;; ----------------------------------------------------------------------

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(use-package smtpmail
  :init
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials
        '(("smtp.gmail.com" 587 nil nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t))

(use-package mu4e
  :config
  (progn
    (require 'shr)
    (require 'org-mu4e)
    (setq mu4e-maildir (expand-file-name "~/Maildir")
          mu4e-attachment-dir "~/Downloads"
          mu4e-compose-signature-auto-include nil

          mu4e-trash-folder "/home/[Gmail]/Trash"
          mu4e-refile-folder "/home/[Gmail]/Archive"
          mu4e-sent-folder "/home/[Gmail]/SentMail"

          mu4e-get-mail-command "mbsync -c ~/.mbsyncrc gmail"
          ;; mu4e-html2text-command "w3m -T text/html" ;; use default

          user-mail-address "edgar1denny@gmail.com"

          mu4e-maildir-shortcuts
          '(("/inbox" . ?i)
            ("/Drafts" . ?D)
            ("/SentMail" . ?s))

          mu4e-view-show-images t
          mu4e-update-interval 300
          mu4e-headers-visible-lines 30
          mu4e-headers-auto-update t
          mu4e-view-show-addresses t
          mu4e-view-show-images t
          mu4e-view-prefer-html t
          mu4e-headers-skip-duplicates t
          mu4e-compose-signature-auto-include nil
          mu4e-sent-messages-behavior 'delete)

    ;; Use 'helm' to select mailboxes
    (setq mu4e-completing-read-function 'completing-read)

    ;; Don't ask to quit
    (setq mu4e-confirm-quit nil)

    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    (setq mu4e-headers-fields
          `((:date .  25)
            (:flags .  6)
            (:from-or-to . 22)
            (:subject . 35)
            (:size . 7))
          mu4e-headers-from-or-to-prefix nil)

    ;; view in Firefox with 'aV'
    (add-to-list 'mu4e-view-actions
                 '("ViewInBrowser" . mu4e-action-view-in-browser) t)

    (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
    (add-hook 'mu4e-compose-mode-hook
              (lambda ()
                (set-fill-column 76)
                (flyspell-mode)))))

;; ----------------------------------------------------------------------
;; multiple cursors
;; ----------------------------------------------------------------------

(use-package multiple-cursors
  :straight t
  :init
  (progn
    ;; these need to be defined here - if they're lazily loaded with
    ;; :bind the key won't work. FIXME: is this true?
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C-'") 'mc-hide-unmatched-lines-mode)))

;; ----------------------------------------------------------------------
;; recentf
;; ----------------------------------------------------------------------

(use-package recentf
  :config
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 50
        recentf-exclude '("^/var/folders\\.*"
                          "COMMIT_EDITMSG\\'"
                          "/\\.git/.*\\'"
                          ".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"))
  (recentf-mode 1))

;; ----------------------------------------------------------------------
;; savehist
;; ----------------------------------------------------------------------

(use-package savehist
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
;; flyspell
;; ----------------------------------------------------------------------

(use-package flyspell
  :straight t
  :commands (flyspell-auto-correct-previous-word flyspell-correct-word-generic)
  :init
  (setq flyspell-use-meta-tab nil)
  :custom
  (flyspell-abbrev-p t)
  (flyspell-use-global-abbrev-table-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :config
  ;; Use mouse
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined))

(add-hook 'flyspell-mode-hook 'flyspell-buffer) ; show misspelled

(with-eval-after-load 'hydra
  (defhydra hydra-spelling (:color blue)
  "
  ^
  ^Spelling^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^───────
  _q_ quit            _<_ previous        _c_ correction
  ^^                  _>_ next            _d_ dictionary
  ^^                  _f_ check           _m_ mode
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("<" flyspell-correct-previous :color pink)
  (">" flyspell-correct-next :color pink)
  ("c" ispell)
  ("d" ispell-change-dictionary)
  ("f" flyspell-buffer :color pink)
  ("m" flyspell-mode)))

;; ----------------------------------------------------------------------
;; theme
;; ----------------------------------------------------------------------

(use-package doom-themes
  :straight t
  :disabled t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package modus-operandi-theme
  :straight t
  :config
  (setq modus-operandi-theme-bold-constructs t)
  (setq modus-operandi-theme-fringes nil)
  (setq modus-operandi-theme-syntax 'alt-syntax-yellow-comments)
  (setq modus-operandi-theme-prompts 'subtle)
  (setq modus-operandi-theme-completions 'moderate)
  (setq modus-operandi-theme-org-blocks 'grayscale)
  (load-theme 'modus-operandi t))

;; (use-package modus-vivendi-theme
;;   :straight t
;;   :config
;;   (load-theme 'modus-vivendi t)
;;   (setq modus-vivendi-theme-bold-constructs t)
;;   (setq modus-vivendi-theme-fringes nil)
;;   (setq modus-vivendi-theme-syntax 'yellow-comments-green-strings)
;;   (setq modus-vivendi-theme-prompts 'intense)
;;   (setq modus-vivendi-theme-org-blocks 'grayscale))

;; ----------------------------------------------------------------------
;; treemacs
;; ----------------------------------------------------------------------

(use-package treemacs
  :straight t
    :commands (treemacs-follow-mode
               treemacs-filewatch-mode
               treemacs-fringe-indicator-mode
               treemacs-git-mode)
    :config
    (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
          treemacs-sorting                 'alphabetic-case-insensitive-desc
          treemacs-follow-after-init       t
          treemacs-is-never-other-window   t
          treemacs-silent-filewatch        t
          treemacs-silent-refresh          t
          treemacs-width                   40)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :straight t
  :after treemacs projectile)

(use-package treemacs-magit
  :straight t
  :after treemacs magit)

;; ----------------------------------------------------------------------
;; vterm
;; ----------------------------------------------------------------------

;; (add-to-list 'load-path "/home/edgar/emacs-libterm")
(use-package vterm
  :straight t)

;; ----------------------------------------------------------------------
;; which key
;; ----------------------------------------------------------------------

(use-package which-key
  :straight t
  :diminish which-key-mode
  ;; :custom
  ;; (which-key-setup-side-window-bottom)
  :config
  (setq which-key-max-display-columns nil
        which-key-min-display-lines 5
        which-key-sort-order #'which-key-prefix-then-key-order
        which-key-popup-type 'side-window
        which-key-add-column-padding 1)
  (which-key-mode +1))

;; (use-package which-key-posframe
;;   :config
;;   (setq which-key-posframe-parameters
;;         '((parent-frame . nil)
;;           (left-fringe . 5)
;;           (right-fringe . 5)))
;;   (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-left-corner)
;;   (which-key-posframe-mode))

(use-package free-keys
  :straight t
  :commands (free-keys)
  :bind ("C-~" . free-keys))

;; ----------------------------------------------------------------------
;; window management
;; ----------------------------------------------------------------------

(use-package winum
  :straight t
  :init
  (setq-default winum-keymap nil)
  :config
  (winum-mode))

(use-package windmove
  :straight t
  :config
  (windmove-default-keybindings))

(use-package beacon
  :straight t
  :diminish beacon-mode
  :config
  (progn
    (setq beacon-blink-when-point-moves-vertically nil)
    (setq beacon-blink-when-point-moves-horizontally nil)
    (setq beacon-blink-when-buffer-changes t)
    (setq beacon-blink-when-window-scrolls t)
    (setq beacon-blink-when-window-changes t)
    (setq beacon-blink-when-focused nil)

    (setq beacon-blink-duration 0.3)
    (setq beacon-blink-delay 0.3)
    (setq beacon-size 20)
    (setq beacon-color "grey")
    (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
    (add-to-list 'beacon-dont-blink-major-modes 'inferior-python-mode)
    (beacon-mode 1)))

(use-package dimmer
  :straight t
  :config
  (setq dimmer-fraction 0.15)
  (setq dimmer-adjustment-mode :both)
  (dimmer-mode 1))


;; move a buffer to another window in a specified direction
(use-package buffer-move
  :straight t
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right))

;; undo a change to a window configuration
(use-package winner
  :straight t
  :init
  (winner-mode 1))

;; ----------------------------------------------------------------------
;; snippets
;; ----------------------------------------------------------------------

(use-package yasnippet
  :straight t
      :diminish yas-minor-mode
      :config
      (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
      (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
      (yas-global-mode)
      (global-set-key (kbd "M-/") 'company-yasnippet))

;; ----------------------------------------------------------------------
;; hydras
;; ----------------------------------------------------------------------

;; TODO: look a evil version
(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1) :post (deactivate-mark))
  "
    -----          |   ^_i_^   |    _d_elete    _k_ill
Rectangle Mode     | _j_   _l_ |    _s_tring    _y_ank
    -----          |   ^_k_^   |    _c_opy      _r_eset
"
  ("i" rectangle-previous-line nil)
  ("k" rectangle-next-line nil)
  ("j" rectangle-backward-char nil)
  ("l" rectangle-forward-char nil)
  ("d" delete-rectangle nil)
  ("s" string-rectangle nil :exit t)
  ("k" kill-rectangle nil)
  ("y" yank-rectangle nil :exit t)
  ("c" copy-rectangle-as-kill nil)
  ("r" (progn (if (region-active-p)
                  (deactivate-mark))
              (rectangle-mark-mode 1)) nil))

;; TODO: look at evil version
(use-package multiple-cursors
  :straight t
  :config
  (defhydra hydra-multiple-cursors (:color red :hint nil)
    "
^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
[_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil :color blue)))


(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir            _v_iew             _m_ark                _(_ toggle details   _i_nsert-subdir      wdired
_C_opy               _O_ view other     _U_nmark all          _)_ omit-mode        _;_ hide-subdir      C-x C-q : edit
_D_elete             _o_pen other       _u_nmark             _l_ redisplay         _w_ kill-subdir      C-c C-c : commit
_R_ename             _M_ chmod          _t_oggle             _g_ revert buf        _e_ ediff            C-c ESC : abort
_Y_ rel symlink      _G_ chgrp          _E_xtension mark     _s_ort                _=_ pdiff
_S_ymlink            ^ ^                _F_ind marked        _._ toggle hydra      \\ flyspell
_r_sync              ^ ^                ^ ^                  ^ ^                   _?_ summary
_z_ compress-file    _A_ find regexp    ^ ^                  ^ ^                   _b_ toggle-dotfiles
_Z_ compress         _Q_ repl regexp    ^ ^                  ^ ^                   _/_ narrow

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  (";" dired-subtree-remove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-subtree-insert)
  ("l" dired-do-redisplay)   ;; relist the marked or single directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("b" dired-dotfiles-toggle)
  ("/" dired-narrow)
  ("q" nil)
  ("." nil :color blue))

(defhydra hydra-mu4e-headers (:color blue :hint nil)
  "
 ^General^   | ^Search^           | _!_: read    | _#_: deferred  | ^Switches^
-^^----------+-^^-----------------| _?_: unread  | _%_: pattern   |-^^------------------
_n_: next    | _s_: search        | _r_: refile  | _&_: custom    | _O_: sorting
_p_: prev    | _S_: edit prev qry | _u_: unmk    | _+_: flag      | _P_: threading
_]_: n unred | _/_: narrow search | _U_: unmk *  | _-_: unflag    | _Q_: full-search
_[_: p unred | _b_: search bkmk   | _d_: trash   | _T_: thr       | _V_: skip dups
_y_: sw view | _B_: edit bkmk     | _D_: delete  | _t_: subthr    | _W_: include-related
_R_: reply   | _{_: previous qry  | _m_: move    |-^^-------------+-^^------------------
_C_: compose | _}_: next query    | _a_: action  | _|_: thru shl  | _`_: update, reindex
_F_: forward | _C-+_: show more   | _A_: mk4actn | _H_: help      | _;_: context-switch
_o_: org-cap | _C--_: show less   | _*_: *thing  | _q_: quit hdrs | _j_: jump2maildir "
  ;; general
  ("n" mu4e-headers-next)
  ("p" mu4e-headers-previous)
  ("[" mu4e-select-next-unread)
  ("]" mu4e-select-previous-unread)
  ("y" mu4e-select-other-view)
  ("R" mu4e-compose-reply)
  ("C" mu4e-compose-new)
  ("F" mu4e-compose-forward)
  ("o" my/org-capture-mu4e)                  ; differs from built-in
  ;; search
  ("s" mu4e-headers-search)
  ("S" mu4e-headers-search-edit)
  ("/" mu4e-headers-search-narrow)
  ("b" mu4e-headers-search-bookmark)
  ("B" mu4e-headers-search-bookmark-edit)
  ("{" mu4e-headers-query-prev)              ; differs from built-in
  ("}" mu4e-headers-query-next)              ; differs from built-in
  ("C-+" mu4e-headers-split-view-grow)
  ("C--" mu4e-headers-split-view-shrink)
  ;; mark stuff
  ("!" mu4e-headers-mark-for-read)
  ("?" mu4e-headers-mark-for-unread)
  ("r" mu4e-headers-mark-for-refile)
  ("u" mu4e-headers-mark-for-unmark)
  ("U" mu4e-mark-unmark-all)
  ("d" mu4e-headers-mark-for-trash)
  ("D" mu4e-headers-mark-for-delete)
  ("m" mu4e-headers-mark-for-move)
  ("a" mu4e-headers-action)                  ; not really a mark per-se
  ("A" mu4e-headers-mark-for-action)         ; differs from built-in
  ("*" mu4e-headers-mark-for-something)
  ("#" mu4e-mark-resolve-deferred-marks)
  ("%" mu4e-headers-mark-pattern)
  ("&" mu4e-headers-mark-custom)
  ("+" mu4e-headers-mark-for-flag)
  ("-" mu4e-headers-mark-for-unflag)
  ("t" mu4e-headers-mark-subthread)
  ("T" mu4e-headers-mark-thread)
  ;; miscellany
  ("q" mu4e~headers-quit-buffer)
  ("H" mu4e-display-manual)
  ("|" mu4e-view-pipe)                       ; does not seem built-in any longer
  ;; switches
  ("O" mu4e-headers-change-sorting)
  ("P" mu4e-headers-toggle-threading)
  ("Q" mu4e-headers-toggle-full-search)
  ("V" mu4e-headers-toggle-skip-duplicates)
  ("W" mu4e-headers-toggle-include-related)
  ;; more miscellany
  ("`" mu4e-update-mail-and-index)           ; differs from built-in
  (";" mu4e-context-switch)
  ("j" mu4e~headers-jump-to-maildir)
  ("." nil))

; ----------------------------------------------------------------------
;; functions
;; ----------------------------------------------------------------------

(defun ivy--matcher-desc ()
 "Used in `hydra-ivy'."
 (if (eq ivy--regex-function
         'ivy--regex-fuzzy)
     "fuzzy"
   "ivy"))

(defun my/shell-command (command)
  "Execute shell COMMAND from the minibuffer."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

;; ----------------------------------------------------------------------
;; keybindings
;; ----------------------------------------------------------------------

(use-package general
  :straight t)

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC m")

(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(my-leader-def
  :states 'visual
  "e /" 'paredit-comment-dwim
  )

(my-leader-def
  :states 'normal
  ;; make leader key work everywhere
  :keymaps 'override
  "w"    '(:ignore t :which-key "window")
  "w l"  'windmove-right
  "w h"  'windmove-left
  "w k"  'windmove-up
  "w j"  'windmove-down
  "w v"  'split-window-right
  "w h"  'split-window-below
  "w +"  'enlarge-window
  "w ."  'enlarge-window-horizontally
  "w ,"  'shrink-window-horizontally
  "w ="  'balance-windows
  "w d"  'delete-window
  "w z"  'delete-other-windows
  "w o"  'other-window
  "w t"  'exwm-floating-toggle-floating
  "w u"  'winner-undo
  "w r"  'winner-redo
  "w >" 'eyebrowse-next-window-config
  "w <" 'eyebrowse-prev-window-config
  "w c" 'eyebrowse-create-window-config

  "x" 'helm-M-x
  "d d" 'dired

  "b"   '(:ignore t :which-key "buffer")
  "b b" 'helm-mini
  "b k" 'kill-buffer
  "b p" 'previous-buffer
  "b n" 'next-buffer
  "b i" 'counsel-ibuffer
  "b m" 'bookmark-set
  "b j" 'bookmark-jump
  "b M" 'bookmark-delete

  ;; "C-M-s-I" 'buf-move-up
  ;; "C-M-s-K" 'buf-move-down
  ;; "C-M-s-J" 'buf-move-left
  ;; "C-M-S-L" 'buf-move-right

  "c"   '(:ignore t :which-key "code")
;; "c l" 'helm-locate-library
  "c s" 'helm-info-at-point
  "c u" 'helm-ucs

  ;; TODO: put within LSP mode map
  "c l"     '(:ignore t :which-key "lsp")
  "c l d"   'eglot-find-declaration
  "c l D"   'xref-find-definitions
  "c l R"   'xref-find-references
  "c l i"   'eglot-find-implementation
  "c l t"   'eglot-find-typeDefinition
  "c l s"   'display-local-help
  "c l r"   'eglot-rename
  "c l h"   'eglot-help-at-point
  "c l f"   'eglot-format
  "c l x"   'eglot-code-actions
  "c l M-r" 'eglot-restart
  "c l S"   'eglot-shutdown

  "k d" 'general-describe-keybindings

  "e"   '(:ignore t :which-key "elisp")
  "e l" 'eval-last-sexp
  "e f" 'eval-defun
  "e r" 'eval-region
  "e b" 'eval-buffer
  "e >" 'paredit-forward-slurp-sexp
  "e <" 'paredit-forward-barf-sexp
  "e /" 'paredit-comment-dwim
  "e (" 'paredit-reindent-defun
  "e i" 'ielm

  "f"   '(:ignore t :which-key "file")
  "f f" 'helm-find-files
  "f s" 'save-buffer
  "f w" 'write-file
  "f r" 'helm-recentf
  "f t" 'treemacs
  "f r" 'counsel-recentf
  "f d" 'dired-sidebar-toggle-sidebar

  "l"   '(:ignore t :which-key "launch")
  "l a" 'my/shell-command

  "s"   '(:ignore t :which-key "search")
  "s s" 'helm-swoop-without-pre-input
  "s p" 'helm-swoop
  "s i" 'swiper-isearch
  "s a" 'helm-multi-swoop
  "s a" 'swiper-all
  "s r" 'helm-rg
  "s g" 'counsel-grep-or-swiper
  "s w" 'avy-goto-word-or-subword-1
  "s c" 'avy-goto-char
  "s l" 'avy-goto-char-in-line
  "s m" 'evil-show-marks

  "q"   '(:ignore t :which-key "quit")
  "q q" 'save-buffers-kill-terminal
  "q K" 'save-buffers-kill-emacs

  "g"   '(:ignore t :which-key "git")
  "g s" 'magit-status
  "g d" 'magit-diff
  "g c" 'magit-commit
  "g p" 'magit-push
  "g G" 'helm-browse-project
  "g G" 'counsel-git
  "g n" 'git-gutter:next-hunk
  "g p" 'git-gutter:previouhunk
  "g =" 'git-gutter:popup-hunk
  "g r" 'git-gutter:revert-hunk
  "g l" 'counsel-git-log
  "g g" 'counsel-git-grep

  "j"   '(:ignore t :which-key "jump")
  "j i" 'helm-semantic-or-imenu
  "j o" 'helm-occur
  "j l" 'helm-locate-library

  "p"   '(:ignore t :which-key "project")
  "p r" 'helm-projectile-rg
  "p f" 'helm-projectile-find-file
  "p s" 'helm-projectile-switch-project
  "p b" 'helm-projectile-switch-to-buffer

  "k"   '(:ignore t :which-key "flymake")
  "k c" 'display-local-help
  "k n" 'flymake-goto-next-error
  "k p" 'flymake-goto-previous-error

  "h"   '(:ignore t :which-key "help")
  "h v" 'counsel-describe-variable-functionriable
  "h w" 'helm-descbinds
  "h s" 'describe-syntax
  "h P" 'describe-package
  "h o" 'describe-symbol
  "h m" 'describe-mode
  "h k" 'describe-key
  "h f" 'counsel-describe-function
  "h d" 'apropos-documentation
  "h a" 'apropos-command
  "h r" 'info-emacs-manual
  "h p" 'finder-by-keyword
  "h l" 'view-lossage
  "h i" 'info
  "h e" 'view-echo-area-messages
  "h b" 'describe-bindings
  "h S" 'info-lookup-symbol

 ;; "h m" '(hydra-multiple-cursors/body :which-key "multiple-cursors")
 ;; "h r" '(hydra-rectangle/body :which-key "rectangle")

  "v"   '(:ignore t :which-key "view")
  "v p" 'ivy-push-view
  "v o" 'ivy-pop-view
  "v s" 'my/save-ivy-views
  "v l" 'my/load-ivy-views
  "v v" 'ivy-switch-view

  ;; FIXME: this is totally wrong
  "m"   '(:ignore t :which-key "minibuffer")
  "m h" 'helm-minibuffer-history

  "o"   '(:ignore t :which-key "org")
  "o m" 'org-mu4e-store-and-capture)

(global-set-key [remap apropos] #'helm-apropos)
(global-set-key [remap find-library] #'helm-locate-library)
(global-set-key [remap bookmark-jump]  #'helm-bookmarks)
(global-set-key [remap execute-extended-command]  #'helm-M-x)
(global-set-key [remap find-file] #'helm-find-files)
(global-set-key [remap locate] #'helm-locate)
(global-set-key [remap imenu] #'helm-semantic-or-imenu)
(global-set-key [remap noop-show-kill-ring] #'helm-show-kill-ring)
(global-set-key [remap switch-to-buffer] #'helm-buffers-list)
(global-set-key [remap projectile-find-file] #'helm-projectile-find-file)
(global-set-key [remap projectile-recentf] #'helm-projectile-recentf)
(global-set-key [remap projectile-switch-project] #'helm-projectile-switch-project)
(global-set-key [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer)
(global-set-key [remap recentf-open-files] #'helm-recentf)
(global-set-key [remap yank-pop] #'helm-show-kill-ring)

;; Get prefixes to work in X applications
;; (when (equal window-system 'x)
;;   (push '?\s-b exwm-input-prefix-keys)
;;   (push '?\s-f exwm-input-prefix-keys)
;;   (push '?\s-w exwm-input-prefix-keys)
;;   (push '?\C-\M-\s-j exwm-input-prefix-keys)
;;   (push '?\C-\M-\s-i exwm-input-prefix-keys)
;;   (push '?\C-\M-\s-k exwm-input-prefix-keys)
;;   (push '?\C-\M-\s-l exwm-input-prefix-keys)
;;   )

;;; Mode Keybindings

(general-define-key
 :keymaps 'projectile-mode-map
 "s-p" '(:ignore t :which "projectile mode map")
 "s-p p" 'projectile-command-map)

(general-define-key
 :keymaps 'company-active-map
 "TAB"   'company-complete-selection
 "C-/"   'company-search-candidates
 "C-M-/" 'company-filter-candidates
 "C-d"   'company-show-doc-buffer)

(general-define-key
 :states 'insert
 "C-<tab>" 'company-complete)

(general-define-key
 :keymaps 'mu4e-headers-mode-map
 "."   'hydra-mu4e-headers/body
 "o"   'my/org-capture-mu4e) ;; TODO: write this function

(use-package dumb-jump
  :straight t
  :general
  (my/all-states-keys
    :states '(insert emacs normal)
    "M-g o" 'dumb-jump-go-other-window
    "M-g g" 'dumb-jump-go
    "M-g l" 'dumb-jump-quick-look
    "M-g x" 'dumb-jump-go-prefer-external
    "M-g z" 'dumb-jump-go-prefer-external-other-window)
  :config
  (progn
    (setq dumb-jump-selector 'ivy)))

(provide 'init)
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
