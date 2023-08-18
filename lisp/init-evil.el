;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

(use-package general)

(general-create-definer my-normal-keys
  :states 'normal
  :keymaps 'override)

(general-create-definer my-motion-keys
  :states 'motion
  :keymaps 'override)

(general-create-definer my-non-insert-keys
  :states '(nromal visual motion)
  :keymaps 'override)

(general-create-definer my-leader-key
  :states '(normal visual motion emacs insert)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(general-create-definer my-local-leader-key
  :states '(normal visual motion emacs insert)
  :keymaps 'override
  :prefix ","
  :non-normal-prefix "M-,")

(general-create-definer my-leader-key-minor-mode
  :states '(normal visual motion emacs insert)
  :keymaps 'override
  :prefix ";"
  :non-normal-prefix "M-;")

(general-create-definer my-all-states-keys
  :states '(normal visual motion emacs insert)
  :keymaps 'override)

(use-package evil
  ;; :general
  ;; (:keymaps 'override
  ;;           :states 'insert
  ;;           "C-j" 'evil-next-line
  ;;           "C-k" 'evil-previous-line
  ;;           "M-o" 'evil-open-below)
  :init
  (setq evil-want-keybinding nil)
  :config
  (define-key evil-normal-state-map "gh" 'beginning-of-defun)
  (my-normal-keys
    "gD" 'xref-find-definitions-other-window
    "gd" 'xref-find-definitions)
  (progn
    (evil-set-initial-state 'pdf-view-mode 'normal)
    (evil-set-initial-state 'calendar-mode 'normal)
    (evil-set-initial-state 'process-menu-mode 'motion)
    (evil-set-initial-state 'special-mode 'motion)
    (evil-set-initial-state 'helpful-mode 'normal)
    (evil-set-initial-state 'Custom-mode 'normal)
    (evil-set-initial-state 'occur-mode 'normal)
    (evil-set-initial-state 'eshell-mode 'insert)
    (evil-set-initial-state 'vterm-mode 'insert)
    (setq evil-insert-state-cursor '(bar "LimeGreen")
          evil-normal-state-cursor '(box "darkorange")
          evil-visual-state-cursor '(box "LightGoldenrod")
          evil-emacs-state-cursor '(box "MediumPurple2")
          evil-echo-state nil)
    (evil-mode 1)))

(use-package expand-region
  :config
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  :bind ("C-=" . er/expand-region))



(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :init
  (setq evil-snipe-show-prompt nil)
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  :general
  (my-normal-keys
    "s" 'evil-snipe-s
    "S" 'evil-snipe-S))


(use-package evil-anzu
  :ghook ('after-init-hook #'global-anzu-mode)
  :general
  (my-leader-key
    "rs" 'anzu-query-replace
    "rr" 'anzu-query-replace-regexp)
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package evil-surround
  :after evil
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package evil-iedit-state
  :general
  (my-leader-key
    "se" 'evil-iedit-state/iedit-mode)
  :config
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil))

(use-package evil-lion
  :general
  (:states '(normal visual)
           "ga" 'evil-lion-left
           "gA" 'evil-lion-right)
  :config
  (setq evil-lion-left-align-key nil
        evil-lion-right-align-key nil))

(use-package evil-textobj-anyblock
  :defer t
  :config
  (setq evil-textobj-anyblock-blocks
        '(("(" . ")")
          ("{" . "}")
          ("\\[" . "\\]")
          ("<" . ">"))))

;; (use-package evil-visualstar
;;   :commands (evil-visualstar/begin-search
;;              evil-visualstar/begin-search-forward
;;              evil-visualstar/begin-search-backward)
;;   :init
;;   (evil-define-key* 'visual 'global
;;     "*" #'evil-visualstar/begin-search-forward
;;     "#" #'evil-visualstar/begin-search-backward))


(use-package evil-easymotion
  :after evil
  :commands evilem-create evilem-default-keybindings
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil)))


  ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
  ;; buffer, rather than just the current line.
  (put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
  (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible))


(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general
  ([remap comment-line] #'evilnc-comment-or-uncomment-lines))


(use-package evil-goggles
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; ----------------------------------------------------------------------
;; which key
;; ----------------------------------------------------------------------

(use-package which-key
  :diminish which-key-mode
  :config
  (progn
    (setq which-key-idle-secondary-delay 0
          which-key-sort-order 'which-key-key-order-alpha
          which-key-min-display-lines 5
          which-key-allow-evil-operators t
          which-key-show-operator-state-maps t
          which-key-add-column-padding 1)
    (my-leader-key
      "a" '(:ignore t :which-key "applications")
      "b" '(:ignore t :which-key "buffers")
      "c" '(:ignore t :which-key "code")
      "d" '(:ignore t :which-key "dired")
      "e" '(:ignore t :which-key "errors")
      "f" '(:ignore t :which-key "files")
      "F" '(:ignore t :which-key "Frames")
      "g" '(:ignore t :which-key "git")
      "h" '(:ignore t :which-key "help")
      "i" '(:ignore t :which-key "insert")
      "j" '(:ignore t :which-key "jump")
      "m" '(:ignore t :which-key "mark")
      "n" '(:ignore t :which-key "complete")
      ;; "p" '(:ignore t :which-key "projects")
      "q" '(:ignore t :which-key "quit")
      "r" '(:ignore t :which-key "regs/rings/replace")
      "s" '(:ignore t :which-key "search")
      "t" '(:ignore t :which-key "toggles")
      "w" '(:ignore t :which-key "windows")
      "x" '(:ignore t :which-key "text")
      "z" '(:ignore t :which-key "zoom"))
    (which-key-mode +1)))

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide 'init-evil)
;;; init.el ends here
