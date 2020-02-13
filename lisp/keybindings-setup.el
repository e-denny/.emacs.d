;;; keybindings-setup.el --- evil minor mode. -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Command Key Bindings

;;; Code:

(use-package general
  :ensure t)


;;; Hydras

(defhydra hydra-zoom ()
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-help (:columns 4)
  ("v" counsel-describe-variable "Desc Variable")
  ("w" helm-descbinds "Desc Bindings")
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

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1) :post (deactivate-mark))
  "
    -----          |   ^_i_^   |    _d_elete    _k_ill
Rectangle Mode     | _j_   _l_ |    _s_tring    _y_ank
    -----          |   ^_k_^   |    _c_opy      _r_eset"
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

(defhydra hydra-flycheck (:color blue)
  "^Flycheck^          ^Errors^            ^Checker^
^────────^──────────^──────^────────────^───────^───────────
[_q_] quit          [_c_] check         [_s_] select
[_v_] verify setup  [_n_] next          [_d_] disable
[_m_] manual        [_p_] previous      [_?_] describe
[_i_] inline
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
    "^Up^            ^Down^        ^Other^
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

;;; Functions

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

;;; Global Keybindings
<<<<<<< HEAD
(general-define-key
 "s-w"    '(:ignore t :which-key "window")
 "s-w l"  'windmove-right
 "s-w j"  'windmove-left
 "s-w i"  'windmove-up
 "s-w k"  'windmove-down
 "s-w v"  'split-window-right
 "s-w c"  'split-window-below
 "s-w d"  'delete-window
 "s-w +"  'enlarge-window
 "s-w ."  'enlarge-window-horizontally
 "s-w ,"  'shrink-window-horizontally
=======

(general-define-key
 "s-w"    '(:ignore t :which-key "window")
 "s-w l"  '(windmove-right :which-key "move right")
 "s-w j"  '(windmove-left :which-key "move left")
 "s-w i"  '(windmove-up :which-key "move up")
 "s-w k"  '(windmove-down :which-key "move bottom")
 "s-w v"  '(split-window-right :which-key "split right")
 "s-w c"  '(split-window-below :which-key "split bottom")
 "s-w d "  '(delete-window :which-key "delete window")
 ;; "s-w +"  'evil-window-increase-height
 ;; "s-w -"  'evil-window-decrease-height
 ;; "s-w >"  'evil-window-increase-width
 ;; "s-w <"  'evil-window-increase-width
>>>>>>> 9f0abcd... clean ups
 "s-w ="  'balance-windows
 "s-w d"  'delete-window
 "s-w z"  'delete-other-windows
 "s-w t"  'exwm-floating-toggle-floating

 "s-SPC" 'helm-M-x

 "s-b"   '(:ignore t :which-key "buffer")
 "s-b b" 'helm-mini
 "s-b d" 'kill-buffer
 "s-b p" 'previous-buffer
 "s-b n" 'next-buffer
 "s-b i" 'counsel-ibuffer

 "s-c"   '(:ignore t :which-key "code")
 "s-c l" 'helm-locate-library
 "s-c s" 'helm-info-at-point
 "s-c u" 'helm-ucs

 "s-k d" 'general-describe-keybindings

 "s-f"   '(:ignore t :which-key "file")
 "s-f f" 'helm-find-files
 "s-f s" 'save-buffer
 "s-f r" 'helm-recentf
 "s-f t" 'treemacs
 "s-f r" 'counsel-recentf
 "s-f d" 'dired-sidebar-toggle-sidebar

 "s-l"   '(:ignore t :which-key "launch")
 "s-l a" 'my/shell-command

 "s-s"   '(:ignore t :which-key "search")
 "s-s s" 'helm-swoop-without-pre-input
 "s-s p" 'helm-swoop
 "s-s i" 'swiper-isearch
 "s-s a" 'helm-multi-swoop
 "s-s r" 'helm-rg
 "s-s a" 'swiper-all
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
 "s-g G" 'helm-browse-project
 "s-g G" 'counsel-git
 "s-g n" 'git-gutter:next-hunk
 "s-g p" 'git-gutter:previous-hunk
 "s-g =" 'git-gutter:popup-hunk
 "s-g r" 'git-gutter:revert-hunk
 "s-g l" 'counsel-git-log
 "s-g g" 'counsel-git-grep
<<<<<<< HEAD

 "s-j"   '(:ignore t :which-key "jump")
 "s-j i" 'helm-semantic-or-imenu
 "s-j o" 'helm-occur
 "s-j l" 'helm-locate-library

 "s-p"   '(:ignore t :which-key "project")
 "s-p r" 'helm-projectile-rg
 "s-p f" 'helm-projectile-find-file
 "s-p s" 'helm-projectile-switch-project
 "s-p b" 'helm-projectile-switch-to-buffer
=======
>>>>>>> 9f0abcd... clean ups

 "s-j"   '(:ignore t :whick-key "jump")
 "s-j i" 'counsel-imenu
 "s-j o" 'ivy-occur
 "s-j l" 'counsel-find-library

 "s-h"   '(:ignore t :whick-key "hydra")
 "s-h h" '(hydra-help/body :which-key "help")
 "s-h c" '(hydra-flycheck/body :which-key "flycheck")
 "s-h m" '(hydra-multiple-cursors/body :which-key "multiple-cursors")
 "s-h r" '(hydra-rectangle/body)
 "s-h z" '(hydra-zoom/body :which-key "zoom")

 "s-v"   '(:ignore t :which "view")
 "s-v p" 'ivy-push-view
 "s-v o" 'ivy-pop-view
 "s-v s" 'my/save-ivy-views
 "s-v l" 'my/load-ivy-views
 "s-v v" 'ivy-switch-view

 "s-x"   '(:ignore t :which "minibuffer")
 "s-x h" 'helm-minibuffer-history

 "s-<tab>" 'company-complete
<<<<<<< HEAD

 "s-o"   '(:ignore t :which "org")
 "s-o m") 'org-mu4e-store-and-capture

(general-define-key
 :keymaps 'projectile-mode-map
 "s-p" '(:ignore t :which "projectile mode map")
 "s-p p" 'projectile-command-map)

=======
 )

;;; Mode Keybindings

(general-define-key
 :keymaps 'projectile-mode-map
 "s-p" '(:ignore t :which "projectile mode map")
 "s-p p" 'projectile-command-map)

>>>>>>> 9f0abcd... clean ups
;; (general-define-key
;;  :keymaps dired-mode-map
;;  "." 'hydra-dired/body)

(general-define-key
 :keymaps 'company-active-map
 "TAB"   'company-complete-selection
 "C-/"   'company-search-candidates
 "C-M-/" 'company-filter-candidates
 "C-d"   'company-show-doc-buffer)

(general-define-key
 :keymaps 'mu4e-headers-mode-map
 "."   'hydra-mu4e-headers/body
 "o"   'my/org-capture-mu4e) ;; TODO: write this function

<<<<<<< HEAD
(defhydra hydra-navigate (:color red
                          :hint nil)
  "_f_: forward-char       _w_: forward-word       _n_: next-line_b_:
backward-char      _W_: backward-word      _p_: previous-line^ ^
              _o_: subword-right      _,_: beginning-of-line^ ^
             _O_: subword-left       _._: end-of-line
_s_: forward sentence   _a_: forward paragraph  _g_: forward page_S_:
backward sentence  _A_: backward paragraph _G_: backward page
_h_: helm mini _B_: buffer list _i_: window_<left>_: previous buffer
_<right>_: next buffer_<up>_: scroll-up           _<down>_:
scroll-down
_[_: backward-sexp _]_: forward-sexp_<_ beginning of buffer _>_ end of
buffer _m_: set mark _/_: jump to mark"
  ("f" forward-char)
  ("b" backward-char)
  ("w" forward-word)
  ("W" backward-word)
  ("n" next-line)
  ("p" previous-line)
  ("o" subword-right)
  ("O" subword-left)
  ("s" forward-sentence)
  ("S" backward-sentence)
  ("a" forward-paragraph)
  ("A" backward-paragraph)
  ("g" forward-page)
  ("G" backward-page)
  ("<right>" next-buffer)
  ("<left>" previous-buffer)
  ("h" helm-mini :color blue)
  ("i" ace-window :color blue)
  ("m" org-mark-ring-push)
  ("/" org-mark-ring-goto :color blue)
  ("B" helm-buffers-list)
  ("<up>" scroll-up)
  ("<down>" scroll-down)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("." end-of-line)
  ("[" backward-sexp)
  ("]" forward-sexp)
  ("," beginning-of-line)
  ("q" nil "quit" :color blue))

(defhydra hydra-smartparens (:hint nil)
     "
  Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
 ------------------------------------------------------------------------------------------------------------------------
  [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_] rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
  [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_] unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer  [_M-k_] kill inside
  [_f_] forward    [_p_] up        [_l_] slurp      [_U_] bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
  [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[__'__\"_] wrap      [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
     ;; Moving
     ("a" sp-beginning-of-sexp)
     ("e" sp-end-of-sexp)
     ("f" sp-forward-sexp)
     ("b" sp-backward-sexp)
     ("n" sp-down-sexp)
     ("N" sp-backward-down-sexp)
     ("p" sp-up-sexp)
     ("P" sp-backward-up-sexp)

     ;; Slurping & barfing
     ("h" sp-backward-slurp-sexp)
     ("H" sp-backward-barf-sexp)
     ("l" sp-forward-slurp-sexp)
     ("L" sp-forward-barf-sexp)

     ;; Wrapping
     ("R" sp-rewrap-sexp)
     ("u" sp-unwrap-sexp)
     ("U" sp-backward-unwrap-sexp)
     ("(" sp-wrap-round)
     ("{" sp-wrap-curly)
     ("[" sp-wrap-square)
     ("'" my/sp-wrap-single-quote)
     ("\"" my/sp-wrap-double-quote)

     ;; Sexp juggling
     ("S" sp-split-sexp)
     ("s" sp-splice-sexp)
     ("r" sp-raise-sexp)
     ("j" sp-join-sexp)
     ("t" sp-transpose-sexp)
     ("A" sp-absorb-sexp)
     ("E" sp-emit-sexp)
     ("o" sp-convolute-sexp)

     ;; Destructive editing
     ("c" sp-change-inner :exit t)
     ("C" sp-change-enclosing :exit t)
     ("k" sp-kill-sexp)
     ("K" sp-backward-kill-sexp)
     ("M-k" my/sp-kill-inside-sexp)
     ("w" sp-copy-sexp)

     ("q" nil)
     ("g" nil))

(use-package dumb-jump
  :general
  (my/all-states-keys
    :states '(insert emacs normal)
    "M-g o" 'dumb-jump-go-other-window
    "M-g g" 'dumb-jump-go
    "M-g l" 'dumb-jump-quick-look
    "M-g x" 'dumb-jump-go-prefer-external
    "M-g z" 'dumb-jump-go-prefer-external-other-window)
=======
(use-package vimish-fold
>>>>>>> 9f0abcd... clean ups
  :config
  (progn
    (setq dumb-jump-selector 'ivy)))

;; (use-package pretty-hydra)

;; (ace-link-setup-default)
;; dumb jump and ivy integration
;; elisp-refs package
;; ivy-view
(provide 'keybindings-setup)

;;; keybindings-setup ends here
