;;; evil-setup.el --- evil minor mode. -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Modal Command Key Bindings

;;; Code:
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init     ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (require 'evil-leader)

  (setq evil-default-cursor t)

  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" ))

  (define-key evil-normal-state-map "p" 'evil-paste-after)
  (define-key evil-normal-state-map "P" 'evil-paste-before)
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-normal-state-map (kbd "C-]") (kbd "\\ M-."))
  )

(use-package evil-surround
  :after evil
  :defer 1
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :after evil
  :ensure t
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.3)
  :config
  (evil-escape-mode 1))

(use-package general
  :ensure t
  :after evil
  :init (general-evil-setup t))

(defhydra hydra-zoom ()
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-help (:columns 4)
  ("v" counsel-describe-variable "Desc Variable")
  ("w" counsel-descbinds "Desc Bindings")
  ("s" describe-syntax "Desc Syntax")
  ("P" describe-package "Desc Package")
  ("o" describe-symbol "Desc Symbols")
  ("m" describe-mode "Desc Mode")
  ("k" describe-key "Desc Keys")
  ("f" counsel-describe-function "Desc Functions")
  ("d" apropos-documentation "Apropos Docs")
  ("a" apropos-command "Apropos Commands")
  ;; ("j" cce/man-at-point "Man for symbol at point.")
  ("r" info-emacs-manual "Emacs Info Pages")
  ("p" finder-by-keyword "Find Package by Keyword")
  ("l" view-lossage "Recent Commands")
  ("i" info "Info Index")
  ("e" view-echo-area-messages "*Messages*")
  ("b" describe-bindings "List Bindings")
  ("S" info-lookup-symbol "Lookup Symbol in Info")
  ("q" nil))


(defun ivy--matcher-desc ()            ; used in `hydra-ivy'
  (if (eq ivy--regex-function
          'ivy--regex-fuzzy)
      "fuzzy"
    "ivy"))

(general-define-key
 :states 'normal
 :prefix ","
 "w" '(:ignore t :which-key "window")
 "wl"  '(windmove-right :which-key "move right")
 "wh"  '(windmove-left :which-key "move left")
 "wk"  '(windmove-up :which-key "move up")
 "wj"  '(windmove-down :which-key "move bottom")
 "w3"  '(split-window-right :which-key "split right")
 "w2"  '(split-window-below :which-key "split bottom")
 "wx"  '(delete-window :which-key "delete window")
 "w+"  'evil-window-increase-height
 "w-"  'evil-window-decrease-height
 "w>"  'evil-window-increase-width
 "w<"  'evil-window-increase-width
 "w="  'balance-windows
 "wd"  'delete-window
 "d0"  'delete-other-windows

 "SPC" 'counsel-M-x

 "b" '(:ignore t :which-key "buffer")
 "bb" 'ivy-switch-buffer
 "bd" 'kill-buffer
 "bn" 'evil-buffer-new
 "bh" 'previous-buffer
 "bl" 'next-buffer
 "bi" 'counsel-ibuffer

 "f" '(:ignore t :which-key "file")
 "ff" 'counsel-find-file
 "fs" 'save-buffer
 "fr" 'counsel-recentf

 "l" '(:ignore t :which-key "launch")
 "la" 'counsel-linux-app

 "s"  '(:ignore t :which-key "search")
 "ss" 'swiper
 "sp" 'swiper-thing-at-point
 "si" 'swiper-isearch
 "sa" 'swiper-all
 "sc" 'evil-ex-nohighlight

 "g" '(:ignore t :which-key "git")
 "gs" 'magit-status
 "gd" 'magit-diff
 "gc" 'magit-commit
 "gp" 'magit-push

 "j"  '(:ignore t :whick-key "jump")
 "ji" 'counsel-imenu
 "jo" 'ivy-occur

 "h"  '(hydra-help/body :which-key "help")

 "v"  '(:ignore t :which "view")
 "vp" 'ivy-push-view
 "vo" 'ivy-pop-view
 "vs" 'my/save-ivy-views
 "vl" 'my/load-ivy-views
 "vv" 'ivy-switch-view

 "z"  '(hydra-zoom/body :which-key "zoom")
 )

(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-t") #'exwm-floating-toggle-floating)

(defhydra hydra-buffer-menu (:color pink :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only^^
_~_: modified      ^ ^                ^ ^                ^^
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(use-package vimish-fold
  :config

  (defhydra hydra-vimish-fold (:color pink :hint nil :delay 0.5)
    "
Vimish Fold
-----------
[_f_]old    [_d_]elete    [_u_]nfold    [_r_]efold    [_t_]oggle
    Prefix CTRL for `*-all' variants
[_a_]vy  [_q_]uit
"
    ("f" vimish-fold)
    ("C-f" vimish-fold-all)
    ("d" vimish-fold-delete)
    ("C-d" vimish-fold-delete-all)
    ("u" vimish-unfold)
    ("C-u" vimish-fold-unfold-all)
    ("r" vimish-fold-refold)
    ("C-r" vimish-fold-refold-all)
    ("t" vimish-fold-toggle)
    ("C-t" vimish-fold-toggle-all)
    ("a" vimish-fold-avy)
    ("q" nil :color blue)))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("e" exchange-point-and-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil))

(use-package multiple-cursors
  :disabled t
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
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
    ("q" nil)))


(defhydra hydra-flycheck (:color blue)
  "
^
^Flycheck^          ^Errors^            ^Checker^
^────────^──────────^──────^────────────^───────^───────────
[_q_] quit          [_c_] check         [_s_] select
[_v_] verify setup  [_n_] next          [_d_] disable
[_m_] manual        [_p_] previous      [_?_] describe
[_i_] inline
^^                  ^^                  ^^
"
  ("q" nil)
  ("c" flycheck-buffer)
  ("d" flycheck-disable-checker)
  ("m" flycheck-manual)
  ("n" flycheck-next-error :color red)
  ("p" flycheck-previous-error :color red)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup)
  ("i" (lambda ()
         (interactive)
         (if (eq 'flycheck-display-errors-function 'flycheck-display-error-messages)
             (setq-local flycheck-display-error-function 'flycheck-inline)
           (setq-local flycheck-display-error-function 'flycheck-display-error-messages)))
   :color amaranth)
  ("?" flycheck-describe-checker))

;; (ace-link-setup-default)
;; (evil-leader/set-key "a" 'hydra-avy/body)
;; dumb jump and ivy integration
;; elisp-refs package
;; ivy-view
(provide 'evil-setup)

;;; evil-setup ends here
