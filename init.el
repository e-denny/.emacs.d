;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

(require 'package)
(require 'cl-lib)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
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

(use-package git)
(setq use-package-verbose t)


(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native complation is *not* available"))

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq native-comp-async-report-warnings-errors nil)

(customize-set-variable 'use-package-compute-statistics t)

;; ----------------------------------------------------------------------
;; garbage-collection
;; ----------------------------------------------------------------------

;; (load "server")
;; (unless (server-running-p)
;;   (server-start))

(setq read-process-output-max (* 1024 1024)) ; 1MB
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
  "Prevent garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-gc-restore ()
  "Restore garbage collection."
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold my-gc))))

(add-hook 'minibuffer-setup-hook #'my-gc-minibuf)
(add-hook 'minibuffer-exit-hook #'my-gc-restore)

;; ----------------------------------------------------------------------

;; disable customization
(setq custom-file (make-temp-file "emacs-custom-"))
(load custom-file)

(setq custom-safe-themes t)

(use-package diminish)

(diminish 'visual-line-mode "")
(diminish 'undo-tree-mode "")
(diminish 'auto-revert-mode "")
(diminish 'isearch-mode "?")
(diminish 'abbrev-mode "")

;; ----------------------------------------------------------------------
;; defaults
;; ----------------------------------------------------------------------

;; (require 'epa)
;;   (epa-file-enable)
;;   (setq epg-gpg-program "gpg")

;;   (load-if-exists "~/.emas.d/secrets.el.gpg")
;;   (load-if-exists "~/.emacs.d/secrets.el")

(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
;; (add-to-list 'default-frame-alist '(internal-border-width . 8))

;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro" :height 110 :width 'expanded)
(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 105 :weight 'medium)
;; (set-face-attribute 'variable-pitch nil
;;                     :font "Open Sans" :height 105)

(use-package emacs
  :config
  (blink-cursor-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
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
  (setq scroll-margin 3
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
  :bind (("<copy>" .  kill-ring-save)
         ("<paste>" . yank)
         ("<cut>" . kill-region)
         ("C-S-b e" . eval-buffer)
         ("C-S-b k" . kill-buffer)))

;; Search (and search/replace) using regex by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; ----------------------------------------------------------------------
;; restart emacs
;; ----------------------------------------------------------------------

(use-package restart-emacs
  :config
  (setq restart-emacs-restore-frames t))

;; ----------------------------------------------------------------------
;; edgar theme
;; ----------------------------------------------------------------------

;; (use-package edgar-theme
;;   :straight (edgar-theme
;;              :local-repo "~/.emacs.d/lisp/edgar-theme"
;;              :type nil)
;;   :config
;;   (load-theme 'edgar t))

;; ----------------------------------------------------------------------
;; avy
;; ----------------------------------------------------------------------

(use-package avy
  :bind (("C-S-g j" . avy-goto-char-timer)
         ("C-S-g l" . avy-goto-line)
         ("C-S-g c" . avy-goto-char-2))
  :config
  (setq avy-background t))

;; ----------------------------------------------------------------------
;; kill ring
;; ----------------------------------------------------------------------

(use-package browse-kill-ring
  :config (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(setq save-interprogram-paste-before-kill t)

;; ----------------------------------------------------------------------
;; psession
;; ----------------------------------------------------------------------

(use-package psession
  :config
  (psession-mode 1)
  (psession-savehist-mode 1)
  (psession-autosave-mode 1))

;; ----------------------------------------------------------------------
;; symex
;; ----------------------------------------------------------------------

;; (use-package symex
;;   :commands symex-mode
;;   :config
;;   (symex-initialize)
;;   (global-set-key (kbd "s-;") 'symex-mode-interface))


;; ----------------------------------------------------------------------
;; corfu
;; ----------------------------------------------------------------------

(use-package corfu
  :bind (:map corfu-map
              ("<escape>" . corfu-quit)
              ("<return>" . corfu-insert)
              ("M-l" . corfu-show-location))
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show all candidates in popup menu
  (corfu-cycle t)
  (corfu-auto nil)
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

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))


(use-package cape
  :bind (("C-S-c p" . completion-at-point)
         ("C-S-c d" . cape-dabbrev)
         ("C-S-c f" . cape-file)
         ("C-S-c s" . cape-symbol)
         ("C-S-c i" . cape-ispell))
  :config
  ;; Use Company backends as Capfs.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (setq-local completion-at-point-functions
  ;;             (mapcar #'cape-company-to-capf
  ;;                     (list #'company-files #'company-ispell #'company-dabbrev)))
  )

;; ----------------------------------------------------------------------
;; dired
;; ----------------------------------------------------------------------

(use-package dired
  :ensure nil
  :bind (("C-S-d d" . dired)
         ("C-S-d j" . dired-jump))
  :custom
  (dired-listing-switches "-laGh1v --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  ;; we want dired not not make always a new buffer if visiting a directory
  ;; but using only one dired buffer for all directories.
  :config
  (defadvice dired-advertised-find-file (around dired-subst-directory activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-filename)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))
  ;; auto refresh dired when file changes
  :hook (dired-mode . auto-revert-mode))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))

(use-package dired-narrow
  ;; type 'g' to un-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-sidebar
  :bind ("C-S-d s" . dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; colorful dired
(use-package diredfl
  :config
  (diredfl-global-mode 0))

(use-package dired-git-info
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))


;; ----------------------------------------------------------------------
;; info
;; ----------------------------------------------------------------------

(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;; ----------------------------------------------------------------------
;; elisp
;; ----------------------------------------------------------------------

(defvar +emacs-lisp--face nil)

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and variables.")

;; from doom emacs
(defun +emacs-lisp-highlight-vars-and-faces (end)
  "Match defined variables and functions to END.
Functions are differentiated into special forms, built-in functions and
  library/userland functions"
  (catch 'matcher
    (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
      (let ((ppss (save-excursion (syntax-ppss))))
        (cond ((nth 3 ppss)  ; strings
               (search-forward "\"" end t))
              ((nth 4 ppss)  ; comments
               (forward-line +1))
              ((let ((symbol (intern-soft (match-string-no-properties 0))))
                 (and (cond ((null symbol) nil)
                            ((eq symbol t) nil)
                            ((keywordp symbol) nil)
                            ((special-variable-p symbol)
                             (setq +emacs-lisp--face 'font-lock-variable-name-face))
                            ((and (fboundp symbol)
                                  (eq (char-before (match-beginning 0)) ?\()
                                  (not (memq (char-before (1- (match-beginning 0)))
                                             (list ?\' ?\`))))
                             (let ((unaliased (indirect-function symbol)))
                               (unless (or (macrop unaliased)
                                           (special-form-p unaliased))
                                 (let (unadvised)
                                   (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                   (setq unaliased (indirect-function unadvised)))))
                                   unaliased)
                                 (setq +emacs-lisp--face
                                       (if (subrp unaliased)
                                           'font-lock-constant-face
                                         'font-lock-function-name-face))))))
                      (throw 'matcher t)))))))
    nil))

;; We byte-compile to ensure they run as fast as possible:
(dolist (fn '(+emacs-lisp-highlight-vars-and-faces))
  (unless (byte-code-function-p (symbol-function fn))
    (with-no-warnings (byte-compile fn))))

(use-package highlight-quoted)

(use-package elisp-mode
  :ensure nil
  :custom
  (font-lock-maximum-decoration t)
  (debugger-stack-frame-as-list t)
  ;; Enhance elisp syntax highlighting, by highlighting defined symbols.
  :config
  (defun my-enhanced-elisp-fontification ()
    (font-lock-add-keywords
     'emacs-lisp-mode
     (append
      ;; highlight defined, special variables & functions
      (when +emacs-lisp-enable-extra-fontification
        `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face))))))

  (defun my/debugger-pp-frame ()
    (interactive)
    (let ((inhibit-read-only t)
          (frame (backtrace-frame (debugger-frame-number))))
      (set-buffer (pop-to-buffer "*BT: Frame*"))
      (destructuring-bind (special fn &rest args) frame
                          (erase-buffer)
                          (progn
                            (insert "(" (pp-to-string fn))
                            (dolist (arg args)
                              (insert "\n" (pp-to-string arg)))
                            (insert ")"))
                          (goto-char (point-min))
                          (indent-pp-sexp))))
  :hook ((emacs-lisp-mode . highlight-quoted-mode)
         (emacs-lisp-mode . my-enhanced-elisp-fontification)))


(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init
  (setq eldoc-idle-delay 0.1)
  :hook ((emacs-lisp-mode . eldoc-mode)
         (lisp-interaction-mode . eldoc-mode)
         (lisp-mode . eldoc-mode)
         (ielm-mode . eldoc-mode)))

;; M-. navigate to symbol
;; M-, pop back to prevous marks
(use-package elisp-slime-nav
  :diminish
  ;; FIXME: this does not work
  :bind ("C-S-h v" . elisp-slime-nav-describe-elisp-thing-at-point)
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-slime-nav-mode))

(use-package macrostep
  :bind (("C-S-x e" . macrostep-expand)
         ("C-S-x n" . macrostep-next-macro)
         ("C-S-x p" . macrostep-prev-macro)
         ("C-S-x c" . macrostep-collapse)
         ("C-S-x q" . macrostep-collapse-all)))

(use-package smartparens
  :diminish smartparens-mode
  :hook ((emacs-lisp-mode . smartparens-mode)
         (lisp-interaction-mode . smartparens-mode)
         (lisp-mode . smartparens-mode)
         (ielm-mode . smartparens-mode)
         (eval-expression-minibuffer-setup . smartparens-mode))
  :bind (:map smartparens-mode-map
              ("C-S-c )" . sp-forward-slurp-sexp)
              ("C-S-c (" . sp-backward-slurp-sexp)
              ("C-S-c }" . sp-forward-barf-sexp)
              ("C-S-c {" . sp-backward-barf-sexp)))

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  ;; (set-face-attribute 'hl-paren-face nil :background "gray92")
  (global-highlight-parentheses-mode)
  ;;make paren highlight update after stuff like paredit changes
  (add-to-list 'after-change-functions '(lambda (&rest x) (hl-paren-highlight))))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((emacs-lisp-mode scheme-mode lisp-mode) . aggressive-indent-mode))

(use-package eros
  :init (eros-mode t))

(use-package lispy
  :hook ((lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (ielm-mode . lispy-mode))
  :config
  (setq lispy-close-quotes-at-end-p t)
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode)

  ;; (when (modulep! :lang emacs-lisp)
  ;;   (setq lispy-outline
  ;;         (concat
  ;;          ;; `lispy-mode' requires `lispy-outline' start with ^
  ;;          (unless (string-prefix-p "^" +emacs-lisp-outline-regexp) "^")
  ;;          +emacs-lisp-outline-regexp))
  ;;   (advice-add #'lispy-outline-level :override #'+emacs-lisp-outline-level))
  )


(with-eval-after-load "lispy"
  (define-key lispy-mode-map (kbd "<f14>") 'my/lispy-cheat-sheet/body))

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
  :hook
  (
   ;; (sly-mode . turn-off-sly-symbol-completion-mode)
   ;; (sly-mode . my-sly-completion)
   (lisp-mode-hook . sly-editing-mode)))


(use-package sly-repl-ansi-color
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

;; ----------------------------------------------------------------------
;; eshell
;; ----------------------------------------------------------------------

(use-package eshell
  :commands (eshell)
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
              (add-to-list 'eshell-visual-commands "pacman")
              (add-to-list 'eshell-visual-commands "htop"))))

;; ----------------------------------------------------------------------
;; proced
;; ----------------------------------------------------------------------

(use-package proced
  :commands proced
  :config
  (setq proced-auto-update-interval 1)
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

;; ----------------------------------------------------------------------
;; browser
;; ----------------------------------------------------------------------

(use-package eww
  :commands eww
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

(setq my-browsers
      '(("Firefox" . browse-url-firefox)
        ("Chromium" . browse-url-chromium)
        ("EWW" . eww-browse-url)))

(setq browse-url-browser-function #'eww-browse-url)

;; Browse URL

(defun my-webkit-browse-url (url &optional pop-buffer new-session)
  "Browse URL with xwidget-webkit' and switch or pop to the buffer.
POP-BUFFER specifies whether to pop to the buffer.
NEW-SESSION specifies whether to create a new xwidget-webkit session."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))
  (xwidget-webkit-browse-url url new-session)
  (let ((buf (xwidget-buffer (and (fboundp 'xwidget-webkit-current-session)
                                  (xwidget-webkit-current-session)))))
    (when (buffer-live-p buf)
      (and (eq buf (current-buffer)) (quit-window))
      (if pop-buffer
          (pop-to-buffer buf)
        (switch-to-buffer buf)))))

;; ----------------------------------------------------------------------
;; shrface
;; ----------------------------------------------------------------------

(use-package shrface
  :after eww
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t))

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
;; vertico
;; ----------------------------------------------------------------------

(use-package all-the-icons)

;; (use-package all-the-icons-completion
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))


 (use-package vertico
   :bind (:map vertico-map
               ("<tab>" . vertico-insert)                ; insert selected candidate
               ("<escape>" . minibuffer-keyboard-quit)   ; close minibuffer
               ;; cycle through candidate groups
               ("C-M-n" . vertico-next-group)
               ("C-M-p" . vertico-previous-group))
   :init
   (vertico-mode)
   ;; Show more candidates
   (setq vertico-count 20)
   ;; Grow and shrink the Vertico minibuffer
   (setq vertico-resize t)
   (setq vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  :config
  (setq orderless-component-separator "[ &]")
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  (advice-add 'company-capf--candidates :around #'just-one-face))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (setq marginalia-margin-threshold 500)
  ;; (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  )

(use-package consult
  :bind (
         ([remap switch-to-buffer] . consult-buffer)
         ;;        ("C-S-b o" . consult-buffer-other-window)

         ;;        ;; FIXME: these bindings do not work
         ;;        ("M-S-s f" . consult-find)
         ;;        ("M-S-s F" . consult-locate)
         ;;        ("M-S-s g" . consult-grep)
         ;;        ("M-S-s G" . consult-git-grep)
         ;;        ("M-S-s r" . consult-ripgrep)
         ;;        ("M-S-s l" . consult-line)
         ;;        ("M-S-s L" . consult-line-multi)
         ;;        ("M-S-s m" . consult-multi-occur)
         ;;        ("M-S-s k" . consult-keep-lines)
         ;;        ("M-S-s u" . consult-focus-lines)
         ;;        ("M-S-s e" . consult-isearch-history)


         ;;        ("s-b h" . consult-history)
         ;;        ;; ("C-c m" . consult-mode-command)
         ;;        ;; ("s-m b" . consult-bookmark)
         ([remap bookmark-jump] . consult-bookmark)
         ;;        ;; ("C-c k" . consult-kmacro)
         ;;        ;; ("C-x M-:" . consult-complex-command)
         ;;        ;; ("C-x 5 b" . consult-buffer-other-frame)
         ;;        ;; ("M-#" . consult-register-load)
         ;;        ;; ("M-'" . consult-register-store)
         ;;        ;; ("C-M-#" . consult-regi)
         ;;        ;; ("M-y" . consult-yank-pop)
         ;;        ("<help> a" . consult-apropos)

         ("C-S-g e" . consult-compile-error)
         ("C-S-g f" . consult-flymake)
         ("C-S-g g" . consult-goto-line)
         ("C-S-g o" . consult-outline)
         ("C-S-g m" . consult-mark)
         ("C-S-g k" . consult-global-mark)
         ("C-S-g i" . consult-imenu)
         ("C-S-g I" . consult-imenu-multi)

         ;;        :map isearch-mode-map
         ;; ("C-e" . consult-isearch-history)
         ("C-l" . consult-line)
         ([remap isearch-abort] . isearch-cancel)
         ("C-." . isearch-forward-symbol-at-point)
         ("C-o" . isearch-occur)
         ;; FIXME: no such pogram as 'rg'
         ("C-M-g" . consult-ripgrep)
         ("C-M-l" . consult-line-multi))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (Setq consult-preview-key (kbd "M-."))
  ;; (s etq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme
  ;;  :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file
  ;;  consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
  ;;  :preview-key (kbd "M-."))

  ;; ;; Optionally configure the narrowing key.
  ;; ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<")
  ;; (kbd "C-+")

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package consult-dir
  :bind (:map minibuffer-local-filename-completion-map
              ("M-." . consult-dir)
              ("M-j" . consult-dir-jump-file)))

;; ----------------------------------------------------------------------
;; embark
;; ----------------------------------------------------------------------

(use-package embark
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-M-a" . embark-act))
  :config
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

;; Consult users will also want the embark-consult package.
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


;; ----------------------------------------------------------------------
;; hydra
;; ----------------------------------------------------------------------

(use-package hydra)

;; ----------------------------------------------------------------------
;; helpful
;; ----------------------------------------------------------------------

(use-package helpful
  ;; FIXME; these bindings do not work
  :bind  (("C-S-h c" . helpful-command)
          ("C-S-h v" . helpful-variable)
          ("C-S-h f" . helpful-function)
          ("C-S-h k" . helpful-key)
          ("C-S-h h" . helpful-at-point)
          ([remap describe-symbol] . helpful-symbol)
          ([remap describe-variable] . helpful-variable)
          ([remap describe-command] . helpful-command)
          ([remap describe-key] . helpful-key)
          :map helpful-mode-map
          ("q" . delete-window)))

;; ----------------------------------------------------------------------
;; buffer placement
;; ----------------------------------------------------------------------

(setq display-buffer-alist
      `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.33)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

;; ----------------------------------------------------------------------
;; ccls
;; ----------------------------------------------------------------------

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls))))

;; ----------------------------------------------------------------------
;; lsp
;; ----------------------------------------------------------------------

(use-package lsp-mode
  :commands lsp-install-server
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  ;; (defun my/orderless-dispatch-flex-first (_pattern index _total)
  ;;   (and (eq index 0) 'orderless-flex))

  ;; (defun my/lsp-mode-setup-completion ()
  ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
  ;;         '(orderless)))

  ;; ;; Optionally configure the first word as flex filtered.
  ;; (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  ;; ;; Optionally configure the cape-capf-buster.
  ;; (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  :config
  (setq lsp-auto-guess-root t
        lsp-file-watch-threshold 500
        lsp-auto-configure nil
        lsp-eldoc-render-all t
        lsp-enable-xref t
        lsp-enable-symbol-highlighting t
        lsp-semantic-tokens-enable t
        lsp-prefer-flymake t)
  :bind (("C-M-S-p c" . lsp-describe-session)
         ("C-M-S-p d" . lsp-find-declaration)
         ("C-M-S-p D" . xref-find-definitions)
         ("C-M-S-p R" . xref-find-references)
         ("C-M-S-p i" . lsp-find-implementation)
         ("C-M-S-p t" . lsp-find-type-definition)
         ("C-M-S-p ?" . display-local-help)
         ("C-M-S-p s" . lsp-signature-help)
         ("C-M-S-p r" . lsp-rename)
         ("C-M-S-p h" . lsp-describe-thing-at-point)
         ("C-M-S-p f" . lsp-format-buffer)
         ("C-M-S-p x" . lsp-execute-code-action)
         ("C-M-S-p w" . lsp-restart-workspace)
         ("C-M-S-p q" . lsp-shutdown-workspace))
  :hook (((c-mode c++-mode java-mode python-mode) . lsp)
         ;; (lsp-completion-mode . my/lsp-mode-setup-completion)
         ))

(use-package lsp-ui
  :after lsp-mode
  :bind (("C-M-S-p m" . lsp-ui-imenu)
         ("C-M-S-p p d" . lsp-ui-peek-find-definitions)
         ("C-M-S-p p r" . lsp-ui-peek-find-references)
         ("C-M-S-p p i" . lsp-ui-peek-find-implementation))
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-delay 100)
  (setq lsp-ui-sideline-show-hover nil
        lsp-ui-peek-always-show t)
  (setq lsp-ui-doc-include-signature t)
  (lsp-ui-doc-mode -1)
  :hook (lsp-mode . lsp-ui-mode))

(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-debug-edit-template)
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; ----------------------------------------------------------------------
;; magit
;; ----------------------------------------------------------------------

;; (setq auth-sources '("~/.authinfo"))

(use-package magit
  :bind (("C-S-v s" . magit-status)
         ("C-S-v d" . magit-diff)
         ("C-S-v c" . magit-commit)
         ("C-S-v u" . magit-push)))

(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (add-hook 'org-mode-hook 'git-gutter-mode)
  :bind (("C-S-v n" . git-gutter:next-hunk)
         ("C-S-v p" . git-gutter:previous-hunk)
         ("C-S-v o" . git-gutter:popup-hunk)
         ("C-S-v r" . git-gutter:revert-hunk))
  :custom
  (git-gutter:modified-sign ">")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :custom-face
  (git-gutter:modified ((t (:background "#c0b18b" :foreground "#2f2f2f"))))
  (git-gutter:added    ((t (:background "#84edb9" :foreground "#2f2f2f"))))
  (git-gutter:deleted  ((t (:background "#d75f5f" :foreground "#2f2f2f")))))

;; ----------------------------------------------------------------------
;; org
;; ----------------------------------------------------------------------

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("C-S-o s" . org-store-link)
              ("C-S-o l" . org-insert-link)
              ("C-S-o m" . org-mu4e-store-and-capture)
              ("C-S-o c" . org-capture)
              ("C-S-o a" . org-agenda)
              ("C-S-o b" . org-iswitchb)
              ("C-S-o r" . org-refile))
  ;; :custom-face
  ;; (org-document-title ((t (:foreground "#171717" :weight bold :height 1.5))))
  :config
  (progn

    (setq org-cite-export-processors '((latex natbib)
                                       (t basic)))

    (setq org-directory "/home/edgar/org")
    (setq org-agenda-files (directory-files-recursively (concat org-directory "/agenda/") "\.org$"))
    (setq org-log-done 'time)
    (setq org-agenda-show-all-dates nil)
    (setq org-capture-templates
          `(("i" "Todo [inbox]" entry
             (file+headline ,(format "%s/%s" org-directory "Inbox.org") "Inbox")
             "* TODO %i%?")
            ("w" "Web site" entry
             (file "")
             "* %a :website:\n\n%U %?\n\n%:initial")
            ("M" "mu4e with kill" entry
             (file+headline ,(format "%s/%s" org-directory "Inbox.org") "Inbox")
             "* TODO %a %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\")) \n %c \n")
            ("m" "mu4e" entry
             (file+headline ,(format "%s/%s" org-directory "Inbox.org") "Inbox")
             "* TODO %a %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
            ("j" "Task Diary" entry
             (file+datetree ,(format "%s/%s" org-directory "/Agenda/Journal.org"))
             "* TODO %^{Description}  %^g \n %? \n Added: %U")
            ("T" "Tickler" entry
             (file+headline ,(format "%s/%s" org-directory "Tickler.org") "Tickler")
             "* %i%? \n %^t")))

    (setq org-return-follows-link t)

    ;;    (org-catch-invisible-edits 'show)

    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-src-fontify-natively t
          org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          org-src-preserve-indentation t)
    (setq org-use-speed-commands t)

    (add-to-list 'org-modules 'org-habit t)

    ;; log TODO creation also
    (setq org-treat-insert-todo-heading-as-state-change t)
    ;; log into LOGBOOK drawer
    (setq org-log-into-drawer t)

    ;; show agenda from today rather than Monday
    (setq org-agenda-start-on-weekday nil)

    (setq org-startup-indented t
          org-startup-truncated nil
          org-ellipsis " …"
          org-return-follows-link t)
    (setq org-hide-emphasis-markers t)
    (setq org-hide-leading-stars t)
    (setq org-src-fontify-natively t)
    (setq org-refile-use-outline-path t
          org-reverse-note-order nil)
    (setq org-tags-column 70)
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-deadline-warning-days 7)
    (setq org-agenda-ndays 7)

    ;;don't show tasks that are scheduled or have deadlines in the
    ;;normal todo list
    (setq org-agenda-todo-ignore-deadlines 'all)
    (setq org-agenda-todo-ignore-scheduled 'all)
    ;;sort tasks in order of when they are due and then by priority
    (setq org-agenda-sorting-strategy
          '((agenda deadline-up priority-down)
            (todo priority-down category-keep)
            (tags priority-down category-keep)
            (search category-keep)))

    ;; refile
    (setq org-refile-targets '((nil :maxlevel . 4)
                               (org-agenda-files :maxlevel . 4)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    (setq org-tags-column 0)
    (setq org-agenda-tags-column 0)

    (setq org-agenda-span 14)
    (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "NOTE(n)" "WAITING(w)" "|" "DONE(d)")))
    (add-hook 'org-mode-hook (lambda () (setq fill-column 100)))
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (add-hook 'org-mode-hook 'org-indent-mode)

    (setq org-clock-persist t)
    (org-clock-persistence-insinuate)
    (setq org-outline-path-complete-in-steps 't)
    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . t)
                                   (python . t)))

    ;; scale text in latex preview
    (plist-put org-format-latex-options :scale 0.6)

    ;; ---- toggle latex fragments ----

    (defvar org-latex-fragment-last nil
      "Holds last fragment/environment you were on.")

    (defun my/org-latex-fragment--get-current-latex-fragment ()
      "Return the overlay associated with the image under point."
      (car (--select (eq (overlay-get it 'org-overlay-type) 'org-latex-overlay) (overlays-at (point)))))

    (defun my/org-in-latex-fragment-p ()
      "Return the point where the latex fragment begins, if inside
  a latex fragment. Else return false"
      (let* ((el (org-element-context))
             (el-type (car el)))
        (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
             (org-element-property :begin el))))

    (defun org-latex-fragment-toggle-auto ()
      ;; Wait for the s
      (interactive)
      (while-no-input
        (run-with-idle-timer 0.05 nil 'org-latex-fragment-toggle-helper)))

    (defun org-latex-fragment-toggle-helper ()
      "Toggle a latex fragment image "
      (condition-case nil
          (and (eq 'org-mode major-mode)
               (let* ((begin (my/org-in-latex-fragment-p)))
                 (cond
                  ;; were on a fragment and now on a new fragment
                  ((and
                    ;; fragment we were on
                    org-latex-fragment-last
                    ;; and are on a fragment now
                    begin
                    ;; but not on the last one this is a little tricky. as you edit the
                    ;; fragment, it is not equal to the last one. We use the begin
                    ;; property which is less likely to change for the comparison.
                    (not (= begin
                            org-latex-fragment-last)))
                   ;; go back to last one and put image back
                   (save-excursion
                     (goto-char org-latex-fragment-last)
                     (when (my/org-in-latex-fragment-p) (org-toggle-latex-fragment))
                     ;; now remove current imagea
                     (goto-char begin)
                     (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                       (when ov
                         (delete-overlay ov)))
                     ;; and save new fragment
                     (setq org-latex-fragment-last begin)))

                  ;; were on a fragment and now are not on a fragment
                  ((and
                    ;; not on a fragment now
                    (not begin)
                    ;; but we were on one
                    org-latex-fragment-last)
                   ;; put image back on
                   (save-excursion
                     (goto-char org-latex-fragment-last)
                     (when (my/org-in-latex-fragment-p)(org-toggle-latex-fragment)))

                   ;; unset last fragment
                   (setq org-latex-fragment-last nil))

                  ;; were not on a fragment, and now are
                  ((and
                    ;; we were not one one
                    (not org-latex-fragment-last)
                    ;; but now we are
                    begin)
                   (save-excursion
                     (goto-char begin)
                     ;; remove image
                     (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                       (when ov
                         (delete-overlay ov)))
                     (setq org-latex-fragment-last begin)))
                  ;; else not on a fragment
                  ((not begin)
                   (setq org-latex-fragment-last nil)))))
        (error nil)))

    (add-hook 'post-command-hook 'org-latex-fragment-toggle-auto)
    (setq org-latex-fragment-toggle-helper (byte-compile 'org-latex-fragment-toggle-helper))
    (setq org-latex-fragment-toggle-auto (byte-compile 'org-latex-fragment-toggle-auto))

    ;; ---- end: toggle latex fragments ----

    (org-link-set-parameters
     "search-view" :follow 'my-search-view-link)

    (defun my-search-view-link (txt)
      "Display a list of TODOs that match txt"
      (setq current-prefix-arg 1)
      (org-search-view (null current-prefix-arg) txt))

    (defun my/hash-word-to-search-view ()
      "Create search-view link if word starts with a hash."
      (when (eq major-mode 'org-mode)
        (with-current-buffer (current-buffer)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "\\\( \\\)\\\(#\\\)\\\(\\\w+\\\|\\\w+\\\s_\\\w+\\\)\\\( \\\|$\\\)" nil t)
              (let ((txt (match-string 3)))
                (replace-match (concat " [[search-view:" txt "][#" txt "]] "))))))))

    (add-hook 'before-save-hook 'my/hash-word-to-search-view)

    (setq org-priority-faces '((?A . (:background "DimGrey" :weight bold))
                               (?B . (:background "DimGrey" :weight bold))
                               (?C . (:background "DimGrey" :weight bold))))
    ))

;; (use-package org-bullets
;;   ;;  :commands org-bullets-mode
;;   :config
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (org-bullets-mode 1)))
;;   (setq org-bullets-bullet-list '("●" "○" "■" "□")))

;; interactively toggle visibility of org elements on entering and leaving
(use-package org-appear
  :after org
  :config
  (setq org-link-descriptive t)
  (setq org-appear-autolinks t)
  (add-hook 'org-mode-hook 'org-appear-mode))



(require 'oc)
(setq org-cite-global-bibliography '("/home/edgar/org/marxism/biblio/library/library.bib"))
(setq org-cite-export-processors
      '((md . (csl "chicago-fullnote-bibliography.csl"))   ; Footnote reliant
        (latex . biblatex)                                 ; For humanities
        (odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
        (t . (csl "modern-language-association.csl"))      ; Fallback
        ))


;; provides a completing-read front-end to browse and act on BibTeX, BibLaTeX,
;; and CSL JSON bibliographic data, and LaTeX, markdown, and org-cite editing support.
(use-package citar
  ;;  :after org-cite
  :bind (:map org-mode-map
              ("C-c c i" . org-cite-insert)
              ("C-c c r" . citar-insert-reference)
              ("C-c c n" . citar-open-notes))
  :custom
  (org-cite-global-bibliography '("/home/edgar/org/marxism/biblio/library/library.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography))


(defun my/org-roam-node-from-cite (citekeys &optional entry)
  "Create an org-node note from a CITEKEYS and ENTRY."
  (interactive (list (citar-select-ref :filter nil)))
  (let ((title (with-temp-buffer
                 (insert (citar-format--entry "${author editor} :: ${title}"
                                              (or entry (citar-get-entry citekeys))))
                 (buffer-string))))
    (org-roam-capture- :templates
                       '(("r" "reference" plain "%?" :if-new
                          (file+head "${citekey}.org"
                                     ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                          :immediate-finish t
                          :unnarrowed t))
                       :info (list :citekey citekeys)
                       :node (org-roam-node-create :title title)
                       :props '(:finalize find-file))))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;; An emacs package to provide tighter Citar and Org-Roam integration
(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode))


(use-package pdf-tools
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :config
  (pdf-tools-install-noverify)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(use-package org-noter
  :config
  (setq org-noter-notes-search-path (list org-directory))
  (setq org-noter-auto-save-last-location t
        org-noter-separate-notes-from-heading t))

(use-package org-pdftools
  :init
  (defun +org--pdftools-link-handler (fn &rest args)
    "Produces a link handler for org-pdftools that suppresses missing-epdfinfo errors whenever storing or exporting links."
    (lambda (&rest args)
      (and (ignore-errors (require 'org-pdftools nil t))
           (file-executable-p pdf-info-epdfinfo-program)
           (apply fn args))))
  (org-link-set-parameters (or (bound-and-true-p org-pdftools-link-prefix) "pdf")
                           :follow   (+org--pdftools-link-handler #'org-pdftools-open)
                           :complete (+org--pdftools-link-handler #'org-pdftools-complete-link)
                           :store    (+org--pdftools-link-handler #'org-pdftools-store-link)
                           :export   (+org--pdftools-link-handler #'org-pdftools-export))
  :hook (org-mode . org-pdftools-setup-link))

;; (use-package org-noter-pdftools
;;   :after org-noter
;;   :config
;;   ;; Add a function to ensure precise note is inserted
;;   (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((org-noter-insert-note-no-questions (if toggle-no-questions
;;                                                    (not org-noter-insert-note-no-questions)
;;                                                  org-noter-insert-note-no-questions))
;;            (org-pdftools-use-isearch-link t)
;;            (org-pdftools-use-freepointer-annot t))
;;        (org-noter-insert-note (org-noter--get-precise-info)))))

;;   ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
;;   (defun org-noter-set-start-location (&optional arg)
;;     "When opening a session with this document, go to the current location.
;; With a prefix ARG, remove start location."
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((inhibit-read-only t)
;;            (ast (org-noter--parse-root))
;;            (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
;;        (with-current-buffer (org-noter--session-notes-buffer session)
;;          (org-with-wide-buffer
;;           (goto-char (org-element-property :begin ast))
;;           (if arg
;;               (org-entry-delete nil org-noter-property-note-location)
;;             (org-entry-put nil org-noter-property-note-location
;;                            (org-noter--pretty-print-location location))))))))
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


(use-package org-ql
  :after org
  :commands (org-ql-search org-ql-view org-ql-view-sidebar org-ql-sparse-tree org-ql-block))

(use-package org-super-agenda
  :after org
  :config (org-super-agenda-mode))

(setq org-agenda-custom-commands
      '(("c" "Super Agenda" agenda
         (org-super-agenda-mode)
         ((org-super-agenda-groups
           '(
             (:name "Habits"
                    :habit t)
             (:name "Today"
                    :time-grid t
                    :scheduled today)
             (:name "Due Today"
                    :deadline today)
             (:name "Important"
                    :priority "A")
             (:name "Overdue"
                    :deadline past)
             (:name "Due soon"
                    :deadline future)
             (:name "Waiting"
                    :todo "WAITING")
             )))
         (org-agenda nil "a"))
        ("n" "Agenda"
         ((agenda ""
                  ((org-agenda-overriding-header "Scheduled")))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled")
                 (org-agenda-skip-function
                  (quote
                   (org-agenda-skip-entry-if
                    (quote scheduled)))))))
         nil nil)))

(use-package org-sidebar
  :after org
  :commands (org-sidebar-tree-toggle org-sidebar-toggle org-sidebar-ql)
  :custom (org-sidebar-tree-side 'left))

(setq package-check-signature nil)

;; (use-package org-ref
;;   :after org
;;   :commands (org-ref-insert-link org-ref-insert-cite-function org-ref-insert-ref-function org-ref-insert-label-function)
;;   :init
;;   (setq reftex-default-bibliography '("~/Documents/Economics/Economics.bib"))
;;   (setq org-ref-default-bibliography '("~/Documents/Economics/Economics.bib"))
;;   (setq org-ref-pdf-directory '("~/Documents/Economics/files/"))

;;   (setq bibtex-completion-bibliography "~/Documents/Economics/Economics.bib"
;;         bibtex-completion-library-(point)ath "~/Documents/Economics/files/"
;;         ;; bibtex-completion-notes-path "~/Dropbox/bibliography/bibtex-notes"
;;         ))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (concat org-directory "/marxism"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam)
         ("C-c n f" . org-roam-find-file)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-insert))
  :config
  (org-roam-db-autosync-mode))

;; download web pages to org
(use-package org-web-tools
  :bind (:map org-mode-map
              ("C-S-o w" . org-web-tools-insert-link-for-url)
              ("C-S-o u" . org-web-tools-insert-web-page-as-entry)))

;; drag and drop images to org
(use-package org-download
  :bind ("C-S-o i" . org-download-image)
  :config
  (setq-default org-download-image-dir (concat org-directory "/marxism/images"))
  :hook (dired-mode . org-download-enable))

(use-package org-journal
  :after org
  :config
  (setq org-journal-dir "~/org/agenda"))

;; ----------------------------------------------------------------------
;; pdf-tools
;; ----------------------------------------------------------------------



;; ----------------------------------------------------------------------
;; latex
;; ----------------------------------------------------------------------

(defvar previewable-environments
  "List of environments that should be previewed."
  '("tabular" "tabular*" "tikzpicture" "..."))

(defadvice preview-region (around preview-at-point-no-long-pauses activate)
  "Preview all latex snippets when no snippet at point."
  (message "preview-region")
  (if (or (not (eq this-command 'preview-at-point))
          (TeX-active-mark)
          (texmathp)
          (member (LaTeX-current-environment) previewable-environments))
      ad-do-it
    (preview-section)))

;; ----------------------------------------------------------------------
;; mu4e
;; ----------------------------------------------------------------------

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; (use-package smtpmail
;;   :init
;;   (setq message-send-mail-function 'smtpmail-send-it
;;         smtpmail-starttls-credentials
;;         '(("smtp.gmail.com" 587 nil nil))
;;         smtpmail-default-smtp-server "smtp.gmail.com"
;;         smtpmail-smtp-server "smtp.gmail.com"
;;         smtpmail-smtp-service 587
;;         smtpmail-debug-info t))

;; (use-package mu4e
;;   :config
;;   (progn
;;     (require 'shr)
;;     (require 'org-mu4e)
;;     (setq mu4e-maildir (expand-file-name "~/Maildir")
;;           mu4e-attachment-dir "~/Downloads"
;;           mu4e-compose-signature-auto-include nil

;;           mu4e-trash-folder "/home/[Gmail]/Trash"
;;           mu4e-refile-folder "/home/[Gmail]/Archive"
;;           mu4e-sent-folder "/home/[Gmail]/SentMail"

;;           mu4e-get-mail-command "mbsync -c ~/.mbsyncrc gmail"
;;           ;; mu4e-html2text-command "w3m -T text/html" ;; use default

;;           user-mail-address "edgar1denny@gmail.com"

;;           mu4e-maildir-shortcuts
;;           '(("/inbox" . ?i)
;;             ("/Drafts" . ?D)
;;             ("/SentMail" . ?s))

;;           mu4e-view-show-images t
;;           mu4e-update-interval 300
;;           mu4e-headers-visible-lines 30
;;           mu4e-headers-auto-update t
;;           mu4e-view-show-addresses t
;;           mu4e-view-show-images t
;;           mu4e-view-prefer-html t
;;           mu4e-headers-skip-duplicates t
;;           mu4e-compose-signature-auto-include nil
;;           mu4e-sent-messages-behavior 'delete)

;;     (setq mu4e-completing-read-function 'completing-read)

;;     ;; Don't ask to quit
;;     (setq mu4e-confirm-quit nil)

;;     (when (fboundp 'imagemagick-register-types)
;;       (imagemagick-register-types))

;;     (setq mu4e-headers-fields
;;           `((:date .  25)
;;     (when (fboundp 'imagemagick-register-types)
;;       (imagemagick-register-types))

;;     (setq mu4e-headers-fields
;;           `((:date .  25)
;;             (:flags .  6)
;;             (:flags .  6)
;;             (:from-or-to . 22)
;;             (:subject . 35)
;;             (:size . 7))
;;           mu4e-headers-from-or-to-prefix nil)

;;     ;; view in Firefox with 'aV'
;;     (add-to-list 'mu4e-view-actions
;;                  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;     (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
;;     (add-hook 'mu4e-compose-mode-hook
;;               (lambda ()
;;                 (set-fill-column 76)
;;                 (flyspell-mode)))))

;; ----------------------------------------------------------------------
;; multiple cursors / iedit
;; ----------------------------------------------------------------------

(use-package multiple-cursors
  ;; FIXME: captured by the window manager
  :bind (("s-c e" . mc/edit-lines)
         ("s-c a" . mc/mark-all-like-this)
         ("s-c n" . mc/mark-next-like-this)
         ("s-c p" . mc/mark-previous-like-this)
         ("s-c N" . mc/unmark-next-like-this)
         ("s-c P" . mc/unmark-previous-like-this)
         ("s-c s" . mc/skip-to-next-like-this)
         ("s-c S" . mc/skip-to-previous-like-this)
         ("s-c h" . mc-hide-unmatched-lines-mode)))

;; Iedit is interactive edit, where if you are on a word and you enter iedit-mode,
;; you're basically editing every instance of that word/variable in the buffer.
(use-package iedit
  :bind ("C-;" . iedit-mode))

;; ----------------------------------------------------------------------
;; recentf
;; ----------------------------------------------------------------------

(use-package recentf
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

;; ----------------------------------------------------------------------
;; theme
;; ----------------------------------------------------------------------

(use-package rainbow-mode
  :config
  (rainbow-mode 1))

;; (use-package modus-themes
;;   :init
;;   (setq modus-themes-bold-constructs t)
;;   (setq modus-themes-slanted-constructs t)
;;   (setq modus-themes-subtle-line-numbers t)
;;   (setq modus-themes-lang-checkers 'straight-underline)
;;   (setq modus-themes-fringes nil)
;;   (setq modus-themes-completions 'moderate)
;;   (setq modus-themes-org-blocks 'gray-background)
;;   (setq modus-themes-mode-line '(1 accented borderless))
;;   (setq modus-themes-prompts 'subtle)
;;   (setq modus-themes-syntax 'alt-syntax-yellow-comments)
;;   (modus-themes-load-themes)
;;   :config
;;   (setq modus-themes-vivendi-color-overrides
;;         '((bg-main . "gray10")
;;           (bg-hl-line . "gray20")
;;           (bg-hl-alt . "gray20")
;;           ))
;;   (modus-themes-load-vivendi))

(use-package ef-themes
  :config (load-theme 'ef-autumn :no-confirm))

;; ----------------------------------------------------------------------
;; treemacs
;; ----------------------------------------------------------------------

(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind ("C-S-d t" . treemacs)
  :config
  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-sorting 'alphabetic-case-insensitive-desc
        treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-silent-filewatch t
        treemacs-silent-refresh t
        treemacs-width 40)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-magit
  :after treemacs magit)

;; ----------------------------------------------------------------------
;; vterm
;; ----------------------------------------------------------------------

(use-package vterm
  ;; prevent hl-line in vterm
  :commands (vterm)
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil))))

;; ----------------------------------------------------------------------
;; which key
;; ----------------------------------------------------------------------

(use-package which-key
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

(use-package free-keys
  :bind ("C-~" . free-keys))

;; ----------------------------------------------------------------------
;; window management
;; ----------------------------------------------------------------------

(use-package winum
  :init
  (setq-default winum-keymap nil)
  :config
  (winum-mode))

(use-package ace-window)

(use-package windmove
  :bind (("C-S-l" . 'windmove-right)
         ("C-S-k" . 'windmove-up)
         ;;         ("C-S-j" . 'windmove-down)
         ;;         ("C-S-h" . 'windmove-left)
         )
  :config
  (windmove-default-keybindings))

;; (use-package dimmer
;;   :config
;;   (setq dimmer-fraction 0.15)
;;   (setq dimmer-adjustment-mode :both)
;;   (dimmer-mode 1))


;; move a buffer to another window in a specified direction
(use-package buffer-move
  :bind (("M-S-k" . 'buf-move-up)
         ("M-S-j" . 'buf-move-down)
         ("M-S-h" . 'buf-move-left)
         ("M-S-l" . 'buf-move-right)))

(use-package winner
  :bind (("C-S-w u" . winner-undo)
         ("C-S-w r" . winner-redo))
  :init
  (winner-mode 1))
;; undo a change to a window configuration

;; ----------------------------------------------------------------------
;; snippets
;; ----------------------------------------------------------------------

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)


;; ----------------------------------------------------------------------
;; my commands
;; ----------------------------------------------------------------------

(use-package emacs
  :config

  (defun my-open-line ()
    "Insert an empty line after the current line."
    (interactive)
    (move-end-of-line nil)
    (newline-and-indent))

  (defun my/next-begin-sexp ()
    (interactive)
    (forward-char 1)
    (search-forward "(" nil t)
    (backward-char 1))

  (defun my/prev-begin-sexp ()
    (interactive)
    (search-backward "(" nil t))

  (defun my/mark-sexp ()
    (interactive)
    (when (and mark-active (looking-at-p "(.*"))
      (backward-up-list))
    (unless (looking-at-p "(.*")
      (my/prev-begin-sexp))
    (mark-sexp))

  (defun my/kill-sexp ()
    (interactive)
    (unless (looking-at-p "(.*")
      (my/prev-begin-sexp))
    (kill-sexp))

  (defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

  (defun my/html-to-org ()
    (interactive)
    (require 'org-web-tools)
    (let* ((html (gui-get-selection 'CLIPBOARD))
           (txt (org-web-tools--html-to-org-with-pandoc html)))
      (org-paste-subtree (1+ (org-outline-level))
                         (with-temp-buffer
                           (org-mode)
                           (insert txt)
                           ))))
  :bind (("C-S-c l" . my/next-begin-sexp)
         ("C-S-c h" . my/prev-begin-sexp)
         ("C-S-c m" . my/mark-sexp)
         ("C-S-c k" . my/kill-sexp)))

(defun my-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(global-set-key (kbd "C-j") 'my-join-line)
(global-set-key (kbd "C-o") 'my-open-line)
(global-set-key (kbd "C-)") 'my/next-begin-sexp)
(global-set-key (kbd "C-(") 'my/prev-begin-sexp)
(global-set-key (kbd "C-M-S-s") 'isearch-forward-symbol-at-point)

;; ----------------------------------------------------------------------
;; hydras
;; ----------------------------------------------------------------------

(with-eval-after-load 'hydra
  (defhydra hydra-spelling (:color blue)
    "
  ^Spelling^        ^Errors^            ^Checker^
  ^────────^────────^──────^────────────^───────^───────
  [_q_] quit        [_<_] previous      [_c_] correction
  ^^                [_>_] next          [_d_] dictionary
  ^^                [_f_] check         [_m_] mode
  "
    ("q" nil)
    ("<" flyspell-correct-previous :color pink)
    (">" flyspell-correct-next :color pink)
    ("c" ispell)
    ("d" ispell-change-dictionary)
    ("f" flyspell-buffer :color pink)
    ("m" flyspell-mode))

  (defhydra hydra-info (:hint nil)
    "
   Info-mode:
   [_j_] forward   [_l_] last     [_u_] up         [_f_] follow reference  [_T_] TOC
   [_k_] backward  [_r_] return   [_m_] menu       [_i_] index             [_d_] directory
   [_n_] nex       [_H_] history  [_g_] goto       [_,_] next index item   [_c_] copy node name
   [_p_] prev      [_<_] top      [_b_] beginning  [_I_] virtual index     [_C_] clone buffer
   [_s_] search    [_>_] final    [_e_] end        ^^                      [_a_] apropos

   [_1_] .. [_9_] Pick first .. ninth item in the node's menu.
  "

    ("j"   Info-forward-node)
    ("k"   Info-backward-node)
    ("n"   Info-next)
    ("p"   Info-prev)
    ("s"   Info-search)
    ("S"   Info-search-case-sensitively)

    ("l"   Info-history-back)
    ("r"   Info-history-forward)
    ("H"   Info-history)
    ("t"   Info-top-node)
    ("<"   Info-top-node)
    (">"   Info-final-node)

    ("u"   Info-up)
    ("^"   Info-up)
    ("m"   Info-menu)
    ("g"   Info-goto-node)
    ("b"   beginning-of-buffer)
    ("e"   end-of-buffer)

    ("f"   Info-follow-reference)
    ("i"   Info-index)
    (","   Info-index-next)
    ("I"   Info-virtual-index)

    ("T"   Info-toc)
    ("d"   Info-directory)
    ("c"   Info-copy-current-node-name)
    ("C"   clone-buffer)
    ("a"   info-apropos)

    ("1"   Info-nth-menu-item)
    ("2"   Info-nth-menu-item)
    ("3"   Info-nth-menu-item)
    ("4"   Info-nth-menu-item)
    ("5"   Info-nth-menu-item)
    ("6"   Info-nth-menu-item)
    ("7"   Info-nth-menu-item)
    ("8"   Info-nth-menu-item)
    ("9"   Info-nth-menu-item)

    ("?"   Info-summary "Info summary")
    ("h"   Info-help "Info help")
    ("q"   Info-exit "Info exit")
    ("." nil "cancel" :color blue))

  (define-key Info-mode-map "." 'hydra-info/body)

  (defhydra hydra-next-error
    (global-map "C-x")
    "
  Compilation errors:
  _j_: next error        _h_: first error    _q_uit
  _k_: previous error    _l_: last error
  "
    ("`" next-error     nil)
    ("j" next-error     nil :bind nil)
    ("k" previous-error nil :bind nil)
    ("h" first-error    nil :bind nil)
    ("l" (condition-case err
             (while t
               (next-error))
           (user-error nil))
     nil :bind nil)
    ("q" nil            nil :color blue))
  )

;; ----------------------------------------------------------------------
;; bookmark
;; ----------------------------------------------------------------------

(use-package bookmark
  :bind (("C-S-m m" . consult-bookmark)
         ("C-S-m s" . bookmark-set)
         ("C-S-m l" . bookmark-bmenu-list)
         ("C-S-m d" . bookmark-delete)))

;; ----------------------------------------------------------------------
;; window / buffer
;; ----------------------------------------------------------------------

(use-package window
  :ensure nil
  :bind (("C-S-w v" . split-window-right)
         ("C-S-w b" . split-window-below)
         ("C-S-w +" . enlarge-window)
         ("C-S-w ." . enlarge-window-horizontally)
         ("C-S-w ," . shrink-window-horizontally)
         ("C-S-w =" . balance-windows)
         ("C-S-w d" . delete-window)
         ("C-S-w z" . delete-other-windows)
         ("C-S-w o" . other-window)

         ("C-S-b p" . previous-buffer)
         ("C-S-b n" . next-buffer)
         ("C-S-b r" . rename-buffer)))

(use-package ace-window
  :bind ("C-S-w w" . ace-window))

(use-package ibuffer
  :bind ("C-S-b i" . ibuffer)
  :config
  (setq-default ibuffer-show-empty-filter-groups nil))


(use-package ibuffer-sidebar
  :bind ("C-S-b s" . ibuffer-sidebar-toggle-sidebar))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))


;; ----------------------------------------------------------------------
;; files
;; ----------------------------------------------------------------------

(use-package files
  :ensure nil
  :bind (("C-S-f s" . save-buffer)
         ("C-S-f f" . find-file)
         ("C-S-f w" . write-file)))

;; ----------------------------------------------------------------------
;; lisp
;; ----------------------------------------------------------------------

(use-package lisp
  :ensure nil
  :bind (("C-S-e f" . mark-defun)
         ("C-S-e s" . mark-sexp)))

(use-package ielm
  :bind ("C-S-e i" . ielm))

(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-S-e e" . eval-last-sexp)
              ("C-S-e r" . eval-region)
              ("C-S-e b" . eval-buffer)
              ("C-S-e d" . eval-defun)))

(use-package edebug
  :bind (:map emacs-lisp-mode-map
              ("C-S-e b" . edebug-defun)))

;; ----------------------------------------------------------------------
;; project
;; ----------------------------------------------------------------------

(use-package project
  :bind (("C-S-p s" . project-switch-project)
         ("C-S-p b" . project-switch-to-buffer)
         ("C-S-p d" . project-dired)
         ("C-S-p c" . project-compile)
         ("C-S-p e" . project-eshell)
         ("C-S-p k" . project-kill-buffers)
         ("C-S-p f" . project-find-file)))

;; ----------------------------------------------------------------------
;; flymake
;; ----------------------------------------------------------------------

(use-package flymake
  :bind (("C-S-t c" . display-local-help)
         ("C-S-t e" . flymake-show-buffer-diagnostics)
         ("C-S-t m" . flymake-menu)
         ("C-S-t n" . flymake-goto-next-error)
         ("C-S-t p" . flymake-goto-previous-error)))

(provide 'init)
;;; init.el ends here
