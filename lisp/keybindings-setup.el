;;; keybindings-setup.el --- evil minor mode. -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Command Key Bindings

;;; Code:

(use-package general
  :ensure t)

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


(use-package multiple-cursors
  ;; :disabled t
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


(defun ivy--matcher-desc ()            ; used in `hydra-ivy'
  (if (eq ivy--regex-function
          'ivy--regex-fuzzy)
      "fuzzy"
    "ivy"))

(defun my/shell-command (command)
  "Execute shell COMMAND from the minibuffer."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(general-define-key
 "s-w"    '(:ignore t :which-key "window")
 "s-w l"  '(windmove-right :which-key "move right")
 "s-w j"  '(windmove-left :which-key "move left")
 "s-w i"  '(windmove-up :which-key "move up")
 "s-w k"  '(windmove-down :which-key "move bottom")
 "s-w 3"  '(split-window-right :which-key "split right")
 "s-w 2"  '(split-window-below :which-key "split bottom")
 "s-w x"  '(delete-window :which-key "delete window")
 "s-w +"  'evil-window-increase-height
 "s-w -"  'evil-window-decrease-height
 "s-w >"  'evil-window-increase-width
 "s-w <"  'evil-window-increase-width
 "s-w ="  'balance-windows
 "s-w d"  'delete-window
 "s-w 0"  'delete-other-windows
 "s-w t"  'exwm-floating-toggle-floating

 "s-SPC" 'counsel-M-x

 "s-b"   '(:ignore t :which-key "buffer")
 "s-b b" 'ivy-switch-buffer
 "s-b d" 'kill-buffer
 "s-b n" 'evil-buffer-new
 "s-b h" 'previous-buffer
 "s-b                "'next-buffer
 "s-b i" 'counsel-ibuffer

 "s-c"   '(:ignore t :which-key "code")
 "s-c l" 'counsel-find-library
 "s-c s" 'counsel-info-lookup-symbol
 "s-c u" 'counsel-unicode-char


 "s-f"   '(:ignore t :which-key "file")
 "s-f f" 'counsel-find-file
 "s-f s" 'save-buffer
 "s-f r" 'counsel-recentf

"s-l"    '(:ignore t :which-key "launch")
"s-l a"  'my/shell-command

 "s-s"   '(:ignore t :which-key "search")
 "s-s s" 'swiper
 "s-s p" 'swiper-thing-at-point
 "s-s i" 'swiper-isearch
 "s-s a" 'swiper-all
 "s-s c" 'evil-ex-nohighlight
 "s-s r" 'counsel-rg
 "s-s g" 'counsel-grep-or-swiper
 "s-s t" 'my/counsel-rg-thing-at-point
 "s-s w" 'avy-goto-word-or-subword-1
 "s-s c" 'avy-goto-char

 "s-g"   '(:ignore t :which-key "git")
 "s-g s" 'magit-status
 "s-g d" 'magit-diff
 "s-g c" 'magit-commit
 "s-g p" 'magit-push
 "s-g G" 'counsel-git

 "s-j"   '(:ignore t :whick-key "jump")
 "s-j i" 'counsel-imenu
 "s-j o" 'ivy-occur
 "s-j l" 'counsel-find-library

 "s-h " '(hydra-help/body :which-key "help")
 "s-c"  '(hydra-flycheck/body :which-key "flycheck")
 "s-m"  '(hydra-multiple-cursors/body :which-key "multiple-cursors")
 "s-r"  '(hydra-rectangle/body)

 "s-v"   '(:ignore t :which "view")
 "s-v p" 'ivy-push-view
 "s-v o" 'ivy-pop-view
 "s-v s" 'my/save-ivy-views
 "s-v l" 'my/load-ivy-views
 "s-v v" 'ivy-switch-view


 "s-x"   '(:ignore t :which "minibuffer")
 "s-x h" 'counsel-minibuffer-history


 "s-z"  '(hydra-zoom/body :which-key "zoom")
 )

(define-key dired-mode-map "." 'hydra-dired/body)
(let ((map company-active-map))
  (define-key map (kbd "TAB")   'company-complete-selection)
  (define-key map (kbd "C-/")   'company-search-candidates)
  (define-key map (kbd "C-M-/") 'company-filter-candidates)
  (define-key map (kbd "C-d")   'company-show-doc-buffer))

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

;; (ace-link-setup-default)
;; dumb jump and ivy integration
;; elisp-refs package
;; ivy-view
(provide 'keybindings-setup)

;;; keybindings-setup ends here
