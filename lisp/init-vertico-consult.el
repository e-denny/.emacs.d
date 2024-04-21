;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

(use-package vertico
  :bind
  (:map vertico-map
        ("<tab>" . vertico-insert)
        ("<escape>" . minibuffer-keyboard-quit)
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

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


(use-package consult
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
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap isearch-abort] . isearch-cancel)
   ("s-b b" . consult-buffer)
   ("s-b o" . consult-buffer-other-window)
   ("s-f F" . consult-find)
   ("s-s G" . consult-git-grep)
   ("s-s h" . consult-isearch-history)
   ("s-m b" . consult-bookmark)
   ("s-b F" . consult-buffer-other-frame)
   ("s-f r" . consult-recent-file)
   ("s-d c" . consult-dir)
   ("s-e k" . consult-compile-error)
   ("s-e f" . consult-flymake)
   ("s-s s-g" . consult-goto-line)
   ("s-s i" . consult-imenu)
   ("s-s I" . consult-imenu-multi)
   ("s-s l" . consult-line)
   ("M-l" . consult-line)
   ("s-s ." . isearch-forward-symbol-at-point)
   ("s-s o" . isearch-occur)
   ("s-s g" . consult-ripgrep)
   ("s-g s-o" . consult-outline)
   ("s-s L" . consult-line-multi)
   ("s-s m" . consult-mark)
   ("s-s k" . consult-global-mark)
   )


  ;;        ("M-S-s F" . consult-locate)
  ;;        ("M-S-s g" . consult-grep)
  ;; ("s-s o" . consult-multi-occur)
  ;;        ("M-S-s k" . consult-keep-lines)
  ;;        ("M-S-s u" . consult-focus-lines)
  ;; ("s-b h" . consult-history)
  ;;        ;; ("C-c m" . consult-mode-command)
  ;;        ;; ("C-c k" . consult-kmacro)
  ;;        ;; ("C-x M-:" . consult-complex-command)
  ;;        ;; ("M-#" . consult-register-load)
  ;;        ;; ("M-'" . consult-register-store)
  ;;        ;; ("C-M-#" . consult-regi)
  ;;        ;; ("M-y" . consult-yank-pop)
  ;;        ("<help> a" . consult-apropos)
  ;;        :map isearch-mode-map
  ;; ("C-e" . consult-isearch-history)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
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
  :bind
  (:map minibuffer-local-filename-completion-map
        ("M-." . consult-dir)
        ("M-j" . consult-dir-jump-file)))

;; (use-package consult-imenu
;;   :defer t
;;   :config
;;   (setf
;;    (alist-get
;;     ?k (plist-get (alist-get 'emacs-lisp-mode consult-imenu-config) :types))
;;    '("Keymaps" font-lock-variable-name-face)))

(provide 'init-vertico-consult)
;;; init-vertico-consult.el ends here
