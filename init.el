;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
(setq use-package-verbose t))

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))

(setq comp-deferred-compilation t)

;; install use-package with straight
;; (straight-use-package 'use-package)

(customize-set-variable 'use-package-compute-statistics t)

;; (setq straight-check-for-modifications '(check-on-save))

;; ----------------------------------------------------------------------
;; garbage-collection
;; ----------------------------------------------------------------------

(setq read-process-output-max (* 1024 1024)) ; 1MB
(defvar my-gc 100000000) ; 100MB

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

(setq custom-file "~/.emacs.d/custom.el")
(setq custom-safe-themes t)
(load custom-file)

(use-package diminish
  :ensure t)

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

;;   (load-if-exists "~/.emacs.d/secrets.el.gpg")
;;   (load-if-exists "~/.emacs.d/secrets.el")

(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(internal-border-width . 8))

(set-face-attribute 'default nil
                    :family "Hack"
                    :height 115
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
(setq sentence-end-double-space nil)

;;kill running processes without confirmation on Emacs exit
(setq confirm-kill-processes nil)

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

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; smooth scrolling
(setq
 scroll-margin 3
 scroll-conservatively 100
 scroll-preserve-screen-position t
 scroll-conservatively scroll-margin
 scroll-step 1
 mouse-wheel-scroll-amount '(6 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 scroll-error-top-bottom t
 next-error-recenter (quote (4))
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

;; auto revert mode
(global-auto-revert-mode 1)

;; ----------------------------------------------------------------------
;; avy
;; ----------------------------------------------------------------------

(use-package avy
  :ensure t
  :commands (avy-goto-word-or-subword-1 avy-goto-char avy-goto-char-in-line)
  :config
  (setq avy-background t))

;; ----------------------------------------------------------------------
;; kill ring
;; ----------------------------------------------------------------------

(use-package browse-kill-ring
  :ensure t
  :config
  (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(setq save-interprogram-paste-before-kill t)

;; ----------------------------------------------------------------------
;; company
;; ----------------------------------------------------------------------

(use-package company
  :ensure t
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 15
        company-clang-insert-arguments nil
        company-dabbrev-downcase t
        company-dabbrev-ignore-case nil
        company-show-numbers nil
        company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil))

;; company with icons
(use-package company-box
  :ensure t
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

;; ----------------------------------------------------------------------
;; dired
;; ----------------------------------------------------------------------

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

(use-package dired
;;  :straight (:type built-in)
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
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             ("I" . dired-subtree-remove)))

;; narrow dired to match filter
;; type 'g' to un-narrow
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
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
  :ensure t
  :config
  (diredfl-global-mode 0))

;; show git logs - FIXME: does not work.
(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; ----------------------------------------------------------------------
;; info
;; ----------------------------------------------------------------------

;; (use-package info-colors
;;   :straight (info-colors :type git :host github :repo "ubolonton/info-colors")
;;   :config
;;   (add-hook 'Info-selection-hook 'info-colors-fontify-node)
;;   )

;; ----------------------------------------------------------------------
;; elisp
;; ----------------------------------------------------------------------

(defvar +emacs-lisp--face nil)

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and variables.")

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

(use-package highlight-quoted
  :ensure t)

(use-package elisp-mode
  :config
  (setq font-lock-maximum-decoration t)
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)

  ;; Enhance elisp syntax highlighting, by highlighting defined symbols.
  (defun my-enhanced-elisp-fontification ()
    (font-lock-add-keywords
     'emacs-lisp-mode
     (append
      ;; highlight defined, special variables & functions
      (when +emacs-lisp-enable-extra-fontification
        `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face))))))

  (add-hook 'emacs-lisp-mode-hook #'my-enhanced-elisp-fontification)
  (setq debugger-stack-frame-as-list t)

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
  )


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
  :ensure t
  :diminish
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-slime-nav-mode))

(use-package macrostep
  :ensure t)

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (progn
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
    ;; (set-face-attribute 'hl-paren-face nil :background "gray92")
    (global-highlight-parentheses-mode)
    ;;make paren highlight update after stuff like paredit changes
    (add-to-list 'after-change-functions '(lambda (&rest x) (hl-paren-highlight)))))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :hook ((emacs-lisp-mode scheme-mode) . aggressive-indent-mode))

;; (use-package highlight-indent-guides
;;   :straight t
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :init
;;   (general-setq highlight-indent-guides-responsive 'top)
;;   (general-setq highlight-indent-guides-method 'character)
;;   (general-setq highlight-indent-guides-character ?│))

;; ----------------------------------------------------------------------
;; eshell
;; ----------------------------------------------------------------------

(use-package eshell
  ;; :straight (:type built-in)
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

;; ;; ----------------------------------------------------------------------
;; ;; exwm
;; ;; ----------------------------------------------------------------------

;; (when (eq window-system 'x)
;;   (use-package exwm
;;   :straight t
;;     :disabled
;;     :config
;;     (require 'exwm-config)
;;     (server-start)
;;     (setq exwm-workspace-number 1)
;;     ;; class name the buffer name
;;     (add-hook 'exwm-update-class-hook
;;               (lambda ()
;;                 (exwm-workspace-rename-buffer exwm-class-name)))
;;     (add-hook 'exwm-update-title-hook
;;               (lambda () (exwm-workspace-rename-buffer exwm-title)))
;;     (add-hook 'exwm-exit-hook
;;               (lambda ()
;;                 (shell-command "killall xfce-session")))
;;     ;; Global keybindings.
;;     (unless (get 'exwm-input-global-keys 'saved-value)
;;       (setq exwm-input-global-keys
;;             `(([?\s-r] . exwm-reset)
;;               ;; ([?\s-w] . exwm-workspace-switch)
;;               ;; 's-N': Switch to certain workspace.
;;               ,@(mapcar (lambda (i)
;;                           `(,(kbd (format "s-%d" i)) .
;;                             (lambda ()
;;                               (interactive)
;;                               (exwm-workspace-switch-create ,i))))
;;                         (number-sequence 0 3)))))
;;     ;; ;; Firefox: open everything in new windows
;;     ;; 1: about:config -> browser.tabs.opentabfor.middleclick -> false
;;     ;; 2: place the following in chrome/userChrome.css in your FF profile:
;;     ;;    #tabbrowser-tabs { visibility: collapse !important; }
;;     (unless (get 'exwm-input-simulation-keys 'saved-value)
;;       (setq exwm-input-simulation-keys
;;             '(
;;               ;; movement - firefox
;;               ([?\C-b] . [left])
;;               ([?\C-f] . [right])
;;               ([?\C-p] . [up])
;;               ([?\C-n] . [down])
;;               ([?\C-a] . [home]) ; top of page
;;               ([?\C-e] . [end]) ; bottom of page
;;               ([?\M-v] . [prior])
;;               ([?\C-v] . [next])
;;               ([?\C-d] . [delete])
;;               ([?\C-k] . [S-end delete])
;;               ([?\C-w] . [?\C-x])
;;               ([?\M-w] . [?\C-c])
;;               ([?\C-y] . [?\C-v])
;;               ([?\C-s] . [?\C-f]) ; search
;;               ([?\C-g] . [?\C-g]) ; search again
;;               ([?\C-r] . [?\C-\M-r]) ; reader mode
;;               ([?\C-j] . [?\C-d]) ; toggle search bar / page
;;               ([?\C-W] . [?\C-\S-w]) ; close window
;;               ([?\M-n] . [?\C-n]) ; new window
;;               ;; Firefox extension - (Saka Key) - link hints
;;               ;;  - always open in new window 'fn' (not tab)
;;               ;; Firefox: focus search bar: C-l
;;               )))
;;     (setq save-interprogram-paste-before-kill t)

;;     (require 'exwm-systemtray)
;;     (exwm-systemtray-enable)

;;     (exwm-enable)
;;     (exwm-config-misc)

;;     (require 'exwm-randr)
;;     ;;  (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "DP-1"))
;;     ;;  (add-hook 'exwm-randr-screen-change-hook 'my-change-screen-hook)
;;     (exwm-randr-enable)))

;; ;; ----------------------------------------------------------------------
;; ;; eyebrowse
;; ;; ----------------------------------------------------------------------

;; (use-package eyebrowse
;;   :straight t
;;   :config
;;   (eyebrowse-mode t)
;;   (setq eyebrowse-new-workspace t)
;; )

;; ;; ----------------------------------------------------------------------
;; ;; evil
;; ;; ----------------------------------------------------------------------

;; (use-package evil
;;   :straight t
;;   :config
;;   (evil-set-initial-state 'edebug-mode 'normal)
;;   (setq evil-symbol-word-search t
;;         evil-want-visual-char-semi-exclusive t
;;         evil-visual-state-cursor 'hollow
;;         evil-ex-interactive-search-highlight 'selected-window)
;;   (evil-mode 1)

;;   (use-package evil-leader
;;     :straight t
;;     :config
;;     (global-evil-leader-mode t)
;;     (evil-leader/set-leader "<SPC>")))

;; (use-package evil-surround
;;   :straight t
;;   :after evil
;;   :config (global-evil-surround-mode))

;; (use-package evil-indent-textobject
;;   :straight t
;;   :after evil)

;; ;;; esc quits
;; (defun minibuffer-keyboard-quit ()
;;   "Abort recursive edit.
;; In Delete Selection mode, if the mark is active, just deactivate it;
;; then it takes a second \\[keyboard-quit] to abort the minibuffer."
;;   (interactive)
;;   (if (and delete-selection-mode transient-mark-mode mark-active)
;;       (setq deactivate-mark  t)
;;     (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
;;     (abort-recursive-edit)))

;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
;; (define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; (use-package evil-snipe
;;   :straight t
;;   :after evil
;;   :diminish evil-snipe-local-mode
;;   :hook (magit-mode . turn-off-evil-snipe-override-mode)
;;   :custom
;;   (evil-snipe-scope 'visible)
;;   (evil-snipe-repeat-scope 'whole-visible)
;;   (evil-snipe-spillover-scope 'whole-buffer)
;;   :config
;;   (evil-snipe-mode +1)
;;   (evil-snipe-override-mode +1))

;; (use-package evil-easymotion
;;   :straight t
;;   :after evil
;;   :commands evilem-create evilem-default-keybindings
;;   :config
;;   ;; Use evil-search backend, instead of isearch
;;   (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
;;                       :bind ((evil-ex-search-highlight-all nil)))
;;   (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
;;                       :bind ((evil-ex-search-highlight-all nil)))
;;   (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
;;                       :bind ((evil-ex-search-highlight-all nil)))
;;   (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
;;                       :bind ((evil-ex-search-highlight-all nil)))

;;   ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
;;   ;; buffer, rather than just the current line.
;;   (put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
;;   (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'visible)
;;   (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible)
;;   (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible)
;;   (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible)
;;   (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible)
;;   (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible)
;;   (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible)
;;   (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible)
;;   )

;; (use-package edebug-mode
;;   :straight (:type built-in)
;;   :config
;;   (evil-set-initial-state 'edebug-mode 'normal))


;; ----------------------------------------------------------------------
;; scroll-on-jump
;; ----------------------------------------------------------------------

;; (use-package scroll-on-jump
;;   :straight (scroll-on-jump :type git :host gitlab :repo "ideasman42/emacs-scroll-on-jump")
;;   :after evil
;;   :config
;;   (setq scroll-on-jump-duration 0.6
;;         scroll-on-jump-smooth t
;;         scroll-on-jump-use-curve t)
;;   (scroll-on-jump-advice-add evil-undo)
;;   (scroll-on-jump-advice-add evil-redo)
;;   (scroll-on-jump-advice-add evil-jump-item)
;;   (scroll-on-jump-advice-add evil-jump-forward)
;;   (scroll-on-jump-advice-add evil-jump-backward)
;;   (scroll-on-jump-advice-add evil-ex-search-next)
;;   (scroll-on-jump-advice-add evil-ex-search-previous)
;;   (scroll-on-jump-advice-add evil-forward-paragraph)
;;   (scroll-on-jump-advice-add evil-backward-paragraph)

;;   ;; Actions that themselves scroll.
;;   (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
;;   (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
;;   (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
;;   (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
;;   (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom)

;;   (with-eval-after-load 'goto-chg
;;     (scroll-on-jump-advice-add goto-last-change)
;;     (scroll-on-jump-advice-add goto-last-change-reverse))

;;   (global-set-key (kbd "<C-M-next>") (scroll-on-jump-interactive 'diff-hl-next-hunk))
;;   (global-set-key (kbd "<C-M-prior>") (scroll-on-jump-interactive 'diff-hl-previous-hunk)))


;; ----------------------------------------------------------------------
;; shrface
;; ----------------------------------------------------------------------

(use-package shrface
  :ensure t
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t))

;; ----------------------------------------------------------------------
;; browser
;; ----------------------------------------------------------------------

(use-package eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

(setq my-browsers
      '(("Firefox" . browse-url-firefox)
        ("Chromium" . browse-url-chromium)
        ("EWW" . eww-browse-url)))

;; (defun my-browse-url (&rest args)
;;   "Select the prefered browser from a helm menu before opening the URL."
;;   (interactive)
;;   (let ((browser (or (helm :sources (helm-build-sync-source "WWW browsers"
;;                                                             :candidates (mapcar 'car my-browsers))
;;                            :buffer "*my browsers*")
;;                      (signal 'quit nil))))
;;     (apply (cdr (assoc browser my-browsers)) args)))

;; (setq browse-url-browser-function #'my-browse-url)

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
;; selectrum / consult / prescient / marginalia / embark
;; ----------------------------------------------------------------------

(use-package selectrum
  :ensure t
  :config
  (setq selectrum-display-action '(display-buffer-in-side-window
                                   (side . bottom)
                                   (slot . -1)))
  (setq selectrum-extend-current-candidate-highlight t)
  (setq selectrum-num-candidates-displayed 20)
  (setq selectrum-fix-minibuffer-height t)
  (setq selectrum-max-window-height 20)
  (selectrum-mode +1))

(use-package prescient
  :ensure t
  )

(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode t))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  ;; :bind ;; C-c bindings (mode-specific-map)
  ;; ("C-c h" . consult-history)
  ;; ("C-c m" . consult-mode-command)
  ;; ("C-c b" . consult-bookmark)
  ;; ;; C-x bindings (ctl-x-map)
  ;; ("C-x M-:" . consult-complex-command)
  ;; ("C-x b" . consult-buffer)
  ;; ("C-x 4 b" . consult-buffer-other-window)
  ;; ("C-x 5 b" . consult-buffer-other-frame)
  ;; ;; Custom M-# bindings for fast register access
  ;; ("M-#" . consult-register-load)
  ;; ("M-'" . consult-register-store)
  ;; ("C-M-#" . consult-register)
  ;; ;; M-g bindings (goto-map)
  ;; ("M-g g" . consult-goto-line)
  ;; ("M-g M-g" . consult-goto-line)
  ;; ("M-g o" . consult-outline)
  ;; ("M-g m" . consult-mark)
  ;; ("M-g k" . consult-global-mark)
  ;; ("M-g i" . consult-project-imenu) ;; Alternative: consult-imenu
  ;; ("M-g e" . consult-error)
  ;; ;; M-s bindings (search-map)
  ;; ("M-s g" . consult-git-grep)      ;; Alternatives: consult-grep, consult-ripgrep
  ;; ("M-s f" . consult-find)         ;; Alternatives: consult-locate, my-fdfind
  ;; ("M-s l" . consult-line)
  ;; ("M-s m" . consult-multi-occur)
  ;; ("M-s k" . consult-keep-lines)
  ;; ("M-s u" . consult-focus-lines)
  ;; ;; Other bindings
  ;; ("M-y" . consult-yank-pop)
  ;; ("<help> a" . consult-apropos)
  :init
  (defun my-fdfind (&optional dir)
    (interactive "P")
    (let ((consult-find-command '("fdfind" "--color=never" "--full-path")))
      (consult-find dir)))
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)
  (advice-add #'register-preview :around
              (lambda (fun buffer &optional show-empty)
                (let ((register-alist (seq-sort #'car-less-than-car register-alist)))
                  (funcall fun buffer show-empty))
                (when-let (win (get-buffer-window buffer))
                  (with-selected-window win
                    (setq-local mode-line-format nil)
                    (setq-local window-min-height 1)
                    (fit-window-to-buffer)))))
  :config
  (setq consult-project-root-function #'vc-root-dir)
  (setq consult-narrow-key "<")
  (defun my-consult-grep ()
    (interactive)
    (consult-ripgrep (or (vc-root-dir) default-directory) (thing-at-point 'symbol)))
  )

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode)
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package embark
  :ensure t
  :bind
  ;; ("C-a" . embark-act)
  :config
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indator embark-action-indicator))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

;; ----------------------------------------------------------------------
;; hydra
;; ----------------------------------------------------------------------

(use-package hydra
  :ensure t)

;; ----------------------------------------------------------------------
;; helpful
;; ----------------------------------------------------------------------

(use-package helpful
  :ensure t
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  )

;; ----------------------------------------------------------------------
;; buffer placement
;; ----------------------------------------------------------------------

(setq display-buffer-alist
      `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.33)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

;; ----------------------------------------------------------------------
;; eglot
;; ----------------------------------------------------------------------

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls))))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) "ccls"))
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'python-mode #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure))

;; ;; ----------------------------------------------------------------------
;; ;; lsp
;; ;; ----------------------------------------------------------------------

;; (use-package lsp-mode
;;   :straight t
;;   :disabled t
;; ;;  :commands lsp
;;   :config
;;   (setq lsp-auto-guess-root t
;;         lsp-file-watch-threshold 500
;;         lsp-auto-configure nil
;;         lsp-prefer-flymake t)
;;   :hook ((c-mode c++-mode java-mode python-mode) . lsp))

;; (use-package lsp-ui
;;   :straight t
;;   :disabled t
;;   :after lsp-mode
;;   :config
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-ui-doc-delay 100)
;;   (setq lsp-ui-sideline-show-hover nil)
;;   (setq lsp-ui-doc-include-signature t)
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package dap-mode
;;   :straight t
;;   :disabled t
;;   :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t))

;; ----------------------------------------------------------------------
;; magit
;; ----------------------------------------------------------------------

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (progn
    (add-hook 'prog-mode-hook 'git-gutter-mode)
    (add-hook 'org-mode-hook 'git-gutter-mode))
  :custom
  (git-gutter:modified-sign ">")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :custom-face
  (git-gutter:modified ((t (:background "#c0b18b" :foreground "#2f2f2f"))))
  (git-gutter:added    ((t (:background "#84edb9" :foreground "#2f2f2f"))))
  (git-gutter:deleted  ((t (:background "#d75f5f" :foreground "#2f2f2f")))))

;; exwm fix to get magit-ediff to work
(use-package ediff
  :config
  (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

;; ;; ----------------------------------------------------------------------
;; ;; mu4e
;; ;; ----------------------------------------------------------------------

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
;; multiple cursors
;; ----------------------------------------------------------------------

(use-package multiple-cursors
  :ensure t
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
  :ensure t
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

(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-slanted-constructs t)
  (setq modus-themes-subtle-line-numbers t)
  (setq modus-themes-lang-checkers 'straight-underline)
  (setq modus-themes-fringes 'subtle)
  (setq modus-themes-completions 'moderate)
  (setq modus-themes-org-blocks 'grayscale)
  (setq modus-themes-prompts 'subtle)
  (setq modus-themes-syntax 'alt-syntax-yellow-comments)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

;; ----------------------------------------------------------------------
;; treemacs
;; ----------------------------------------------------------------------

(use-package treemacs
  :ensure t
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
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
  :ensure t
  :after treemacs magit)

;; ----------------------------------------------------------------------
;; vterm
;; ----------------------------------------------------------------------

(use-package vterm
  :ensure t
  ;; TODO: is this working?
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil))))

;; ----------------------------------------------------------------------
;; which key
;; ----------------------------------------------------------------------

(use-package which-key
  :ensure t
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
  :ensure t
  :commands (free-keys)
  :bind ("C-~" . free-keys))

;; ----------------------------------------------------------------------
;; window management
;; ----------------------------------------------------------------------

(use-package winum
  :ensure t
  :init
  (setq-default winum-keymap nil)
  :config
  (winum-mode))

(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings))

(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.15)
  (setq dimmer-adjustment-mode :both)
  (dimmer-mode 1))


;; move a buffer to another window in a specified direction
(use-package buffer-move
  :ensure t
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right))

;; undo a change to a window configuration
(use-package winner
  :ensure t
  :init
  (winner-mode 1))

;; ----------------------------------------------------------------------
;; snippets
;; ----------------------------------------------------------------------

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode)
  (global-set-key (kbd "M-/") 'company-yasnippet))

(use-package yasnippet-snippets
  :ensure t
  )

;; ----------------------------------------------------------------------
;; hydras
;; ----------------------------------------------------------------------

;; TODO: look at evil version
(use-package multiple-cursors
  :ensure t
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

;; ----------------------------------------------------------------------
;; functions
;; ----------------------------------------------------------------------

(defun my/shell-command (command)
  "Execute shell COMMAND from the minibuffer."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))


(defun my/insert-new-line ()
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

(defun my/copy-line ()
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (copy-region-as-kill beg end)))

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

(defun my/backward-symbol ()
  (interactive)
  (forward-symbol -1))

(defun my/get-positions-of-line-or-region ()
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

;; ----------------------------------------------------------------------
;; keybindings
;; ----------------------------------------------------------------------

(use-package general
  :ensure t)


(general-define-key
 :keymaps 'override

 "H-M-s-d" 'kill-whole-line
 "H-M-s-y" 'my/copy-line
 "H-M-s-j" 'join-line
 "H-M-s-o" 'my/insert-new-line

 "H-M-s-w" 'forward-symbol
 "H-M-s-b" 'my/backward-symbol

 "H-M-s-a" 'beginning-of-buffer
 "H-M-s-e" 'end-of-buffer

 "H-M-s-c" 'copy-region-as-kill
 "H-M-s-x" 'kill-region
 "H-M-s-v" 'yank
 "H-M-s-z" 'undo

 "H-M-s-k" 'kill-word
 "H-M-s-W" 'backward-kill-word

 "H-M-s-f" 'mark-defun
 "H-M-s-s" 'my/mark-sexp

 "H-M-s-i" 'my/kill-sexp
 "H-M-s-." 'my/next-begin-sexp
 "H-M-s-," 'my/prev-begin-sexp
 )


(general-define-key
 :prefix "<f5>"
 ;; :states 'normal
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

 "x" 'execute-extended-command

 "d"   '(:ignore t :which-key "dired")
 "d d" 'dired
 "d j" 'dired-jump

 "b"   '(:ignore t :which-key "buffer")
 "b b" 'consult-buffer
 "b k" 'kill-buffer
 "b p" 'previous-buffer
 "b n" 'next-buffer
 "b i" 'ibuffer
 "b m" 'bookmark-set
 "b j" 'consult-bookmark
 "b M" 'bookmark-delete

 ;; "C-M-s-I" 'buf-move-up
 ;; "C-M-s-K" 'buf-move-down
 ;; "C-M-s-J" 'buf-move-left
 ;; "C-M-S-L" 'buf-move-right

 "c"   '(:ignore t :which-key "code")
 "c s"     'info-lookup-symbol
 "c u"     'insert-char
 "c y"     'browse-kill-ring
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
 "e e k" 'beginning-of-defun
 "e e j" 'end-of-defun
 "e e u" 'backward-up-list
 "e >" 'paredit-forward-slurp-sexp
 "e <" 'paredit-forward-barf-sexp
 "e /" 'paredit-comment-dwim
 ;; "e (" 'paredit-reindent-defun
 "e (" 'paredit-backward-up
 "e )" 'paredit-forward-down
 "e i" 'ielm
 "e d" 'edebug-defun
 "e m" '(:ignore t :which-key "macrostep")
 "e m e" 'macrostep-expand
 "e m n" 'macrostep-next-macro
 "e m p" 'macrostep-prev-macro
 "e m c" 'macrostep-collapse
 "e m q" 'macrostep-collapse-all

 "f"   '(:ignore t :which-key "file")
 "f f" 'find-file
 "f s" 'save-buffer
 "f w" 'write-file
 "f r" 'consult-recent-file
 "f t" 'treemacs
 "f d" 'dired-sidebar-toggle-sidebar

 "l"   '(:ignore t :which-key "launch")
 "l a" 'my/shell-command

 "s"   '(:ignore t :which-key "search")
 "s l" 'consult-line
 "s i" 'consult-isearch
 "s r" 'my-consult-grep
 "s g" 'consult-git-grep
 "s w" 'avy-goto-word-or-subword-1
 "s c" 'avy-goto-char
 "s m" 'imenu

 "q"   '(:ignore t :which-key "quit")
 "q q" 'save-buffers-kill-terminal
 "q K" 'save-buffers-kill-emacs

 "g"   '(:ignore t :which-key "git")
 "g s" 'magit-status
 "g d" 'magit-diff
 "g c" 'magit-commit
 "g p" 'magit-push
 "g n" 'git-gutter:next-hunk
 "g p" 'git-gutter:previouhunk
 "g =" 'git-gutter:popup-hunk
 "g r" 'git-gutter:revert-hunk
 "g g" 'consult-git-grep

 "j"   '(:ignore t :which-key "jump")
 "j i" 'consult-project-imenu
 "j o" 'consult-multi-occur
 "j g" 'consult-goto-line

 "m"   '(:ignore t :which-key "mark")
 "m d" 'mark-defun
 "m s" 'mark-sexp
 "m w" 'mark-word

 "p"   '(:ignore t :which-key "project")
 "p f" 'project-find-file
 "p s" 'project-switch-project
 "p b" 'project-switch-to-buffer
 "p d" 'project-dired
 "p c" 'project-compile
 "p e" 'project-eshell
 "p k" 'project-kill-buffers
 "p i" 'consult-project-imenu

 "k"   '(:ignore t :which-key "flymake")
 "k c" 'display-local-help
 "k e" 'consult-error
 "k f" 'consult-flymake
 "k n" 'flymake-goto-next-error
 "k p" 'flymake-goto-previous-error

 "h"   '(:ignore t :which-key "help")
 "h v" 'elisp-slime-nav-describe-elisp-thing-at-point
 "h s" 'describe-syntax
 "h P" 'describe-package
 "h o" 'describe-symbol
 "h m" 'describe-mode
 "h k" 'describe-key
 "h K" 'helpful-at-point
 "h f" 'describe-function
 "h d" 'apropos-documentation
 "h a" 'apropos-command
 "h r" 'info-emacs-manual
 "h p" 'finder-by-keyword
 "h l" 'view-lossage
 "h i" 'info
 "h e" 'view-echo-area-messages
 "h b" 'describe-bindings
 "h S" 'info-lookup-symbol

 "o"   '(:ignore t :which-key "org")
 "o m" 'org-mu4e-store-and-capture)

;;; Mode Keybindings

(general-define-key
 :keymaps 'company-active-map
 "TAB"   'company-complete-selection
 "C-/"   'company-search-candidates
 "C-M-/" 'company-filter-candidates
 "C-d"   'company-show-doc-buffer)

(general-define-key
 :keymaps 'helpful-mode-map
 "q" 'delete-window)


(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(provide 'init)
;;; init.el ends here
