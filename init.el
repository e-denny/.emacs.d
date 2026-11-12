;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

(require 'package)
(require 'cl-lib)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Org on top of the bundled copy.
;;
;; The LaTeX preview rewrite (`org-latex-preview-mode', the "olp" work by
;; Karthik Chikmagalur and Tecosaur) ships in Org 10.0-pre, not in the 9.8
;; bundled with Emacs 32.  GNU-devel ELPA publishes it as the `org' package;
;; installing it puts its directory at the front of `load-path', ahead of the
;; bundled lisp/org, so `(require 'org)' gets 10.0-pre and every org-*.elc
;; agrees on the version (`org-assert-version') instead of mixing trees.  The
;; devel archive is added only around the install: left in `package-archives'
;; permanently it would offer development builds of every package, and
;; `package-upgrade-all' would take them.
;;
;; `dont-select' keeps `package-install' from rewriting
;; `package-selected-packages' behind Custom's back; `org' is listed in the
;; saved value at the end of this file instead.  The archive *descriptor* is
;; passed rather than the symbol: `(package-installed-p 'org)' is t for the
;; bundled copy, so `package-install' would refuse the symbol as "already
;; installed" -- the descriptor names the 10.0-pre build explicitly.  After
;; that, `M-x my/org-devel-upgrade' installs a newer GNU-devel build.
(defun my/org-devel-upgrade ()
  "Install Org from GNU-devel ELPA, or upgrade it to the latest build."
  (interactive)
  (let ((package-archives
         (cons '("gnu-devel" . "https://elpa.gnu.org/devel/")
               package-archives)))
    (package-refresh-contents)
    (let* ((latest (cadr (assq 'org package-archive-contents)))
           (installed (package-get-descriptor 'org 'installed)))
      (cond
       ((and installed
             (not (version-list-< (package-desc-version installed)
                                 (package-desc-version latest))))
        (message "Org is up to date (%s)"
                 (package-version-join (package-desc-version installed))))
       (installed (package-upgrade 'org))
       (t (package-install latest 'dont-select))))))

(unless (package-installed-p 'org '(10 0 -1))
  (condition-case err
      (my/org-devel-upgrade)
    (error (warn "Org 10 not installed from GNU-devel: %s" err))))

;; Load it now, before anything else can pull a library out of the bundled
;; tree: two Org versions in one session is the mixed install that
;; `org-assert-version' reports.
(require 'org)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)

(use-package git)
(setq use-package-verbose t)

(customize-set-variable 'use-package-compute-statistics t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; ----------------------------------------------------------------------
;; garbage-collection
;; ----------------------------------------------------------------------

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
  "Prevent garbage collection while in the minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-gc-restore ()
  "Restore garbage collection after leaving minibuffer."
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold my-gc))))

(add-hook 'minibuffer-setup-hook #'my-gc-minibuf)
(add-hook 'minibuffer-exit-hook #'my-gc-restore)

;; ----------------------------------------------------------------------
;; Emacs server + pi's Emacs integration
;;
;; pi reaches this Emacs instance through emacsclient, so the server must be
;; up.  The companion packages live in ~/.emacs.d/lisp (already on
;; `load-path' above):
;;
;;   pi-subagents.el  live views of subagent runs, in a term buffer
;;   pi-annotate.el   /annotate: mark up a file beside its review sidecar
;;   pi-sidecar.el    status/model/path sidecar for pi terminals
;;   dired-sidecar.el the workspace's directory listing, above the pi sidecar
;;
;; Both sidecars are kept in every tab; `C-c P' and `C-c T' hide them.

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'pi-subagents)
(require 'pi-annotate)

;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; completion stack: vertico + orderless + consult + corfu + marginalia +
;; embark
;; ----------------------------------------------------------------------

;; Vertico -- vertical completion UI in the minibuffer.
;; Keys are already good by default (remapped): M-n/M-p or C-n/C-p move,
;; TAB/SPC complete, RET submit, C-g cancel.
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

;; Orderless -- match the input out of order and in pieces. The pattern is
;; split on spaces and a candidate must match every word, wherever it appears
;; and in whatever order, so `li my' finds `lisp/my-tabs.el' and `M-x ins pac'
;; finds `package-install'. It is a completion *style*, not a UI, and needs no
;; binding of its own: Vertico, Consult and Corfu all filter through
;; `completion-styles', so this one setting reaches the minibuffer list, the
;; in-buffer popup and consult-line, keep-lines and focus-lines, which filter
;; with the global styles rather than the buffer's. (Consult's asynchronous
;; greps split their input into words themselves, and need nothing here.)
;;
;; `basic' stays behind it as the fallback, because dynamic completion tables
;; (`completion-table-dynamic', `completion-table-in-turn') only work through
;; a style that asks them for candidates.
;;
;; Completion of file names is the exception, and takes the package's own
;; recommendation: the category keeps `partial-completion' alone, which leaves
;; paths typable the old way (`/u/s/l' for `/usr/share/local'), lets TRAMP
;; hosts complete, and keeps wildcards; `completion-pcm-leading-wildcard'
;; then makes that style match inside a word in Emacs 31 instead of only at
;; its start, which is the rest of the behaviour above.
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles partial-completion)))
        completion-pcm-leading-wildcard t))

;; Posframe -- the child frames the two popups below are drawn in.  Nothing is
;; tuned here: it is declared so that it stays installed on its own account.
(use-package posframe
  :ensure t)

;; Vertico-posframe -- show the minibuffer, prompt and candidates together, in
;; a child frame instead of in the echo area: the list rises from the bottom
;; of the frame, centered, like a panel, and leaves the echo area free.
;; `vertico-posframe-width' stays nil, so the frame fits its candidates above
;; the package's floor of 62% of the frame width; the background, the text
;; color and the border come from the `vertico-posframe' faces, which inherit
;; the theme's `default'.  The package is not on MELPA, hence `:vc' (Emacs
;; 30+; `elpa/' is ignored by git): on a machine that lacks it, use-package
;; clones it into `package-user-dir' and byte-compiles it.
(use-package vertico-posframe
  :vc (:url "https://github.com/tumashu/vertico-posframe")
  :after vertico
  :config
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
  (vertico-posframe-mode 1))

;; Marginalia -- annotations (docs, keybindings, file info) on candidates.
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

;; Consult -- search and navigation commands on top of completing-read.
;; Upstream ships no bindings, so these are the ones its README recommends.
;; Note: they replace the built-in commands listed in the comments.
(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)              ; switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ; switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ; switch-to-buffer-other-frame
         ("C-x p b" . consult-project-buffer)      ; project-switch-to-buffer
         ("C-x r b" . consult-bookmark)            ; bookmark-jump
         ("C-x r y" . consult-yank-replace)        ; yank-pop
         ("M-g g"   . consult-goto-line)           ; goto-line
         ("M-g l"   . consult-line)                ; nothing
         ("M-g i"   . consult-imenu)               ; imenu
         ("M-g o"   . consult-outline)             ; nothing
         ("M-g r"   . consult-ripgrep)             ; nothing
         ("M-g k"   . consult-keep-lines)          ; keep-lines
         ("M-g f"   . consult-focus-lines)         ; nothing
         ("M-s r"   . consult-ripgrep)             ; isearch-backward-regexp
         ("M-s l"   . consult-line)                ; nothing
         ("M-s g"   . consult-grep)))              ; nothing

;; Corfu -- in-buffer completion popup.
;; Default corfu-map keys: TAB/up/down/M-n/M-p navigate, RET insert,
;; M-SPC insert separator, M-h documentation, C-g quit.
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1))

;; Embark -- act on whatever is at point or selected in a completion list.
(use-package embark
  :ensure t
  :bind (("C-."   . embark-act)       ; act on target
         ("C-;"   . embark-dwim)      ; default action (DWIM)
         ("C-h B" . embark-bindings)) ; browse all keybindings
  :init
  ;; C-h in a minibuffer shows the keybindings of the active target.
  (setq prefix-help-command #'embark-prefix-help-command))

;; Embark integration for Consult (export/grep/org on search results).
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ----------------------------------------------------------------------
;; magit
;; ----------------------------------------------------------------------

;; Magit -- git porcelain.
;; Upstream ships `C-x g' only via its menu autoload; these are the three
;; bindings its manual recommends. Inside a Magit buffer: TAB/`s' toggle a
;; section, `?' help, `g' refresh, `q' quit, `n'/`p' next/prev section.
(defun my-git-commit-setup ()
  "Use a 72 column commit message body."
  (setq fill-column 72))

;; NOTE on `setq' instead of `:custom' throughout this file: use-package's
;; `:custom' stores values in a pseudo-theme that `custom-enabled-themes'
;; does not list, so loading a real theme (`doom-one' below) disables it and
;; silently drops those values for every package that is not yet loaded.
;; Plain `setq' is not touched by `defcustom' or by theme loading, so it is
;; used here deliberately.
(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)         ; status buffer
         ("C-x M-g" . magit-dispatch)       ; all commands, prefix-style
         ("C-c M-g" . magit-file-dispatch)) ; commands for current file
  :init
  ;; Status/log/diff take over the current window; diffs get their own.
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        ;; Save unsaved buffers a commit/stash/checkout would touch.
        magit-save-repository-buffers 'dontask
        ;; Highlight changed lines inside a hunk, not just the header.
        magit-diff-refine-hunk 'all
        ;; Where `magit-status' looks when not visiting a file in a repo.
        magit-repository-directories '(("~/" . 1)
                                       ("~/Documents" . 1)
                                       ("~/Models" . 1))
        ;; Classic git layout: 50-char summary, 72-char body (Magit: 68).
        git-commit-summary-max-length 50)
  :hook
  ;; `git-commit-setup-auto-fill' is already in the hook; this just sets the
  ;; width it fills to. `fill-column' is buffer-local, so this stays local.
  (git-commit-setup . my-git-commit-setup))

;; with-editor -- make $EDITOR point back at this Emacs, so `git commit'
;; (or `git rebase -i') run from a shell buffer opens its text here.
(use-package with-editor
  :ensure t
  :hook ((shell-mode  . with-editor-export-git-editor)
         (eshell-mode . with-editor-export-git-editor)))

;; ----------------------------------------------------------------------
;; persistence: savehist, saveplace, easysession
;; ----------------------------------------------------------------------

;; savehist -- persist minibuffer histories and the kill ring across restarts.
(use-package savehist
  :ensure nil                          ; built in
  :init
  (savehist-mode 1)
  (setq history-length 1000
        history-delete-duplicates t
        savehist-additional-variables
        '(kill-ring search-ring regexp-search-ring extended-command-history)))

;; saveplace -- reopen each file at the position the cursor was left at.
(use-package saveplace
  :ensure nil                          ; built in
  :init
  (save-place-mode 1))

;; easysession -- named sessions: frames, windows and buffers restored on
;; startup and saved on exit. Replaces desktop-save-mode, which is why this
;; file deliberately does not also enable `desktop-save-mode'.
(use-package easysession
  :ensure t
  :demand t
  :init
  (setq easysession-save-interval (* 10 60)) ; autosave every 10 minutes
  :config
  (easysession-setup)                   ; restore at startup, save on exit
  :bind (("C-c s l" . easysession-switch-to)
         ("C-c s s" . easysession-save)
         ("C-c s L" . easysession-switch-to-and-restore-geometry)
         ("C-c s r" . easysession-rename)
         ("C-c s R" . easysession-reset)
         ("C-c s u" . easysession-unload)
         ("C-c s d" . easysession-delete)))

;; ----------------------------------------------------------------------
;; posframe popups: the frame's corners, and the message buffers
;; ----------------------------------------------------------------------

;; Two position handlers, both measured against the frame rather than the
;; window, so a popup stays put while the windows are split around it.
;; Posframe's own `frame-*' corner handlers cannot be used for this: they
;; return a marker position that only means anything to a refposhandler,
;; which neither caller passes, and so land at the left edge.  Everything
;; needed is in the plist `posframe-show' hands its poshandler.
(defun my-posframe-top-right (info)
  "Position handler: the top right corner of the frame."
  (cons (- (plist-get info :parent-frame-width)
           (plist-get info :posframe-width))
        0))

(defun my-posframe-bottom-right (info)
  "Position handler: the bottom right corner of the frame's text area.
The bottom leaves out the mode line and the minibuffer, the way
`posframe-poshandler-frame-bottom-right-corner' would if it returned
absolute coordinates instead of a marker."
  (cons (- (plist-get info :parent-frame-width)
           (plist-get info :posframe-width))
        (- (plist-get info :parent-frame-height)
           (plist-get info :posframe-height)
           (plist-get info :mode-line-height)
           (plist-get info :minibuffer-height))))

;; Warnings, the message log and byte-compilation errors are things to glance
;; at, and Emacs's answer -- split a window open along the bottom of the
;; frame and leave it there -- costs a split, shifts the text being read and
;; has to be closed by hand afterwards.  They go into a child frame in the
;; top right corner instead, over the least-used part of the frame, and it
;; takes itself away after eight seconds.  `display-buffer' is where a
;; buffer's window is decided and `display-buffer-alist' overrides whatever
;; window the caller asked for, so the entries below replace it.
;;
;; Which displays get the posframe matters as much as where it is: only the
;; ones Emacs puts up by itself.  Looking at one of these buffers on purpose
;; (`C-x b', `C-x 4 b', `C-h e') has to give a window to read it in, so the
;; action below checks how it was called -- see its docstring.
;;
;; The action returns the symbol `fail': no window, and no falling through to
;; the default action.  Returning nil there would let `display-buffer' split
;; a window open anyway, and returning a window would hand it to
;; `warning--display-buffer', which then moves point to the end of the
;; warning buffer in the window it was given -- the one being read.
;;
;; `*compilation*', `*grep*' and the like are deliberately absent: those are
;; worked in (next error, previous error), not glanced at.
(defvar my-posframe-message-timeout 8
  "Seconds a message posframe stays on screen.")

(defun my-posframe-display-message-buffer (buffer alist)
  "Show BUFFER in a posframe for `my-posframe-message-timeout' seconds.
Return `fail', so that `display-buffer' stops here and makes no window.

Only two kinds of display reach the posframe, and both are Emacs acting on
its own.  A warning, which Emacs tags with the `warning' category in its
display action however it was raised -- including from inside a command
that ran into one.  And a log buffer put up while no command is running at
all, which is a timer or a process filter talking.  Anything a command
asks for is left to `display-buffer': `C-x b', `C-x 4 b' and `C-h e' run as
commands, so looking at one of these buffers on purpose still gives a
window to read it in.  `this-command' is the test for that -- it is bound
for the whole of a command, and nil in a timer or a filter."
  (when (and (posframe-workable-p)
             (or (eq (alist-get 'category alist) 'warning)
                 (null this-command)))
    (posframe-show buffer
                   :poshandler #'my-posframe-top-right
                   :timeout my-posframe-message-timeout
                   :max-height 15
                   :max-width 100
                   :border-width 1
                   :border-color (face-attribute 'region :background nil t)
                   ;; Show the newest lines, not the beginning of the log.
                   :window-point (with-current-buffer buffer (point-max))
                   ;; Nothing but the timer above takes it away.
                   :hidehandler #'ignore)
    'fail))

(dolist (name '("\\*Warnings\\*" "\\*Compile-Log\\*" "\\*Messages\\*"))
  (add-to-list 'display-buffer-alist
               `(,name (my-posframe-display-message-buffer))))

;; ----------------------------------------------------------------------
;; which-key (with its posframe) and project (the latter built in)
;; ----------------------------------------------------------------------

;; which-key -- show the completions of a prefix key after a short delay.
;; Already enabled by default in Emacs 30+; these are the usual tunings.
(use-package which-key
  :ensure nil                          ; built in
  :init
  (which-key-mode 1)
  (setq ;; A full second, so that a two-key sequence typed at speed -- C-x C-f --
        ;; is finished before the list would appear between its two halves.
        ;; `which-key-idle-secondary-delay' is the shorter delay for the list
        ;; reappearing after it has been shown once in the same sequence, and
        ;; does not affect that first appearance.
        which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.05
        which-key-add-column-padding 1
        which-key-show-early-on-C-h t))

;; which-key-posframe -- draw that list in a child frame in the bottom right
;; corner of the frame instead of in the side window which-key uses by
;; default, out of the way of the buffer being worked in.  One column makes
;; the list read downwards along that edge: `which-key--create-pages' lays
;; the keys out vertically when `which-key-max-display-columns' is 1.
(use-package which-key-posframe
  :ensure t
  :after which-key
  :config
  (setq which-key-posframe-poshandler #'my-posframe-bottom-right
        which-key-max-display-columns 1)
  (which-key-posframe-mode 1))

;; project -- per-project commands on the built-in `C-x p' prefix.
(use-package project
  :ensure nil                          ; built in
  :init
  (setq project-list-file (expand-file-name "projects" user-emacs-directory)
        project-vc-extra-root-markers '(".project" ".projectile"
                                        "compile_commands.json" "pyproject.toml")
        ;; `C-x p p' reads a project and then asks which command to run in
        ;; it.  Here the answer is always the same, because every project has
        ;; a tab: a single command symbol means no dispatch menu, so the tab
        ;; is selected -- or made -- at once, exactly as a chip in the tab bar
        ;; does, and `C-x p f/b/g/d' still act inside the project afterwards.
        project-switch-commands 'my-tab-bar-visit-project)
  :bind (:map project-prefix-map
              ("m" . magit-project-status)))

;; ----------------------------------------------------------------------
;; completion extras: cape, nerd-icons-corfu
;; ----------------------------------------------------------------------

;; Cape -- extra completion-at-point backends for the stock CAPF machinery
;; (works with corfu above). Dabbrev and file names are the two the README
;; suggests adding globally; mode-specific CAPFs keep priority since these
;; are appended.
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;; Nerd icons in the Corfu popup's left margin.
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ----------------------------------------------------------------------
;; editing visuals: rainbow-delimiters, diff-hl, indent-bars, outline-indent
;; ----------------------------------------------------------------------

;; rainbow-delimiters -- colour brackets by nesting depth.
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; diff-hl -- uncommitted-change indicators in the fringe.
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode 1)
  :config
  ;; Update indicators live while typing.
  (diff-hl-flydiff-mode 1)
  ;; Magit's post-refresh hook keeps indicators in sync after stage/commit.
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; indent-bars -- graphical indentation guides.
(use-package indent-bars
  :ensure t
  :hook (prog-mode . indent-bars-mode)
  :init
  (setq indent-bars-treesit-support t
        indent-bars-no-descend-lists t))

;; outline-indent -- code folding that respects indentation.
(use-package outline-indent
  :ensure t
  :hook (prog-mode . outline-indent-minor-mode)
  :init
  (setq outline-indent-ellipsis " \u25bc"))

;; ----------------------------------------------------------------------
;; window navigation: windmove
;; ----------------------------------------------------------------------

;; windmove -- move between the windows of the frame with ALT+arrows.  Built
;; in, hence `:ensure nil'.  `windmove-wrap-around' carries a move that runs
;; off the edge of the frame on to the window at the opposite edge, so
;; ALT+right from the rightmost window lands in the leftmost one.
;;
;; The keys are ALT+arrows, which Emacs spells M-<left> and friends.  That
;; takes M-<left> and M-<right> away from `left-word'/`right-word'; word
;; motion stays on C-<left>/C-<right> and M-f/M-b.  A terminal keeps the keys
;; for itself by default -- ghostel sends every modified arrow key to the
;; program -- so the ghostel section below lists them as exceptions.
(use-package windmove
  :ensure nil
  :init
  (setq windmove-wrap-around t)
  :bind (("M-<left>" . windmove-left)
         ("M-<right>" . windmove-right)
         ("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)))

;; ----------------------------------------------------------------------
;; theme
;; ----------------------------------------------------------------------

;; doom-themes -- `doom-one' is the Doom One palette kitty runs (see
;; ~/.config/kitty/current-theme.conf): background #282c34, foreground
;; #bbc2cf, and the same 16 ANSI colors. `doom-one-light' is its light
;; counterpart, so `C-c t' keeps flipping dark/light the way ef-themes did.
(defun my-toggle-theme ()
  "Flip between `doom-one' and `doom-one-light'.
Disables the outgoing theme first: `load-theme' otherwise leaves both
enabled, and the older one's faces show through where the newer has none."
  (interactive)
  (let ((next (if (eq (car custom-enabled-themes) 'doom-one)
                  'doom-one-light
                'doom-one)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme next :no-confirm)))

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one :no-confirm)
  :bind (("C-c t" . my-toggle-theme)))

;; ----------------------------------------------------------------------
;; cursor and current line
;; ----------------------------------------------------------------------

;; The line highlight below takes over what the cursor's blinking was doing:
;; showing where point is. `blink-cursor-mode' is a minor mode whose init
;; value is t, so it has to be switched off explicitly.
(blink-cursor-mode -1)

;; HL-Line puts an overlay on the current line. `doom-one' styles the face
;; itself (`hl-line' is `bg-alt', #21242b, a shade under the frame's #282c34),
;; so no face override belongs here. `global-hl-line-buffers' is new in 31.1
;; and keeps the overlay out of the minibuffer and *-prefixed buffers.
(global-hl-line-mode 1)

;; ----------------------------------------------------------------------
;; modeline
;; ----------------------------------------------------------------------

;; doom-modeline -- the mode line. Enabled from `:init' as its README shows;
;; it pulls in nerd-icons itself (installed above) and the Nerd Font is
;; already on this system. Its `...-continuous-word-count-modes' default of
;; (markdown-mode gfm-mode org-mode) is left alone.
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-icon t              ; requires nerd-icons + Nerd Font
        doom-modeline-major-mode-icon t
        ;; Show a word count in the buffers listed in the variable above.
        doom-modeline-enable-word-count t))

;; ----------------------------------------------------------------------
;; selected window
;; ----------------------------------------------------------------------

;; A bright four-pixel bar down the left edge of the selected window, so the
;; active window is obvious at a glance.
;;
;; Window decorations cannot be coloured per window: the `fringe' face is
;; looked up in the buffer on display, and the window parameters documented in
;; `(elisp)Window Parameters' carry formats but no faces.  Recolouring that
;; face was the first attempt and it does not hold up -- the frame does not
;; consider the fringe changed, so the new colour appears only when something
;; forces a full redraw, and measured on this session the accent stayed painted
;; on the window the selection had left.  A fringe *bitmap* is different: it
;; belongs to the line it sits beside, so adding or removing it invalidates
;; that line the way a `diff-hl' mark does, and redisplay repaints it.  The bar
;; is therefore a bitmap -- `my-selected-window-bar-width' pixels wide, one
;; character tall -- put on every line by the buffer-local `line-prefix' and
;; `wrap-prefix', with a whole-buffer overlay carrying the same two properties
;; because a variable change alone marks no window for redisplay.
;;
;; It is on the left because the prefix starts the line; the right fringe keeps
;; its width, so continuation glyphs and the diff-hl marks keep their room.  A
;; buffer shown in two windows wears the bar in both; every other window keeps
;; the theme's fringe.  Bitmaps only exist on lines that exist, so the bar is
;; finished off below the last line with an `after-string' of one display row
;; measured in pixels -- a bitmap of exactly the leftover height, so it meets
;; the mode line with no gap and no seam.
(defcustom my-selected-window-bar 'auto
  "How to colour the bar down the selected window's left edge.
`auto' follows the theme -- the `doom-modeline-bar' background, the accent the
active mode line already wears -- so `C-c t' keeps it readable.  A colour
string fixes it, and nil turns the bar off."
  :type '(choice (const :tag "Follow the theme" auto)
                 (color :tag "Fixed colour")
                 (const :tag "Off" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'my-selected-window-bar-reapply)
           (my-selected-window-bar-reapply))))

(defcustom my-selected-window-bar-width 4
  "Width in pixels of the bar down the selected window's left edge.
Four is the thickness of the mode line's own bar.  Emacs clips fringe bitmaps
to the fringe, so this cannot exceed `(window-fringes)'."
  :type 'integer
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'my-selected-window-bar-reapply)
           (my-selected-window-bar-reapply))))

(defface my-selected-window-bar-face
  '((t :foreground "unspecified"))
  "Face the selected-window bar is drawn with.
Only its foreground matters -- the bitmap's set pixels take it -- and
`my-selected-window-bar-reapply' keeps it equal to `my-selected-window-bar'.")

(defvar my-selected-window-bar--colour nil
  "Colour in force for the bar, recomputed whenever the theme changes.")

(defvar my-selected-window-bar--row-height nil
  "Character height the bar's line bitmap was last defined for.")

(defvar my-selected-window-bar--row-fringe nil
  "Fringe width the bar's line bitmap was last defined for.")

(defvar my-selected-window-bar--row-width nil
  "Bitmap width the bar's line bitmap was last defined for.")

(defvar my-selected-window-bar--row-bitmap nil
  "Name of the bitmap that draws the bar on a line, or nil.")

(defvar my-selected-window-bar--fill-bitmap nil
  "Name of the bitmap that draws the filler row, or nil.")

(defvar my-selected-window-bar--fill-obsolete nil
  "Filler bitmap to destroy once its text is off screen, or nil.")

(defvar my-selected-window-bar--buffer nil
  "Buffer currently carrying the bar, or nil.")

(defvar-local my-selected-window-bar--installed nil
  "Non-nil when the bar is installed in this buffer.")

(defvar-local my-selected-window-bar--saved-line-prefix nil
  "`line-prefix' this buffer had before the bar was installed.")

(defvar-local my-selected-window-bar--saved-wrap-prefix nil
  "`wrap-prefix' this buffer had before the bar was installed.")

(defvar-local my-selected-window-bar--bar nil
  "Overlay carrying the bar's `line-prefix' on this buffer's lines, or nil.")

(defvar-local my-selected-window-bar--filler nil
  "Overlay adding bar-only display rows below this buffer's last line.
A fringe bitmap is drawn only on rows that exist, so in a buffer shorter than
its window the bar would stop at the last line.  The filler is display text,
so it draws the same bitmap without touching the buffer, and its height is in
pixels so the bar ends exactly at the mode line.")

(defun my-selected-window-bar--face-colour (face attribute)
  "Return FACE's ATTRIBUTE as a colour string, or nil if it has none.
`face-attribute' signals for a face that does not exist, which happens before
doom-modeline or the theme is loaded, so the face is checked first."
  (and (facep face)
       (let ((colour (face-attribute face attribute nil t)))
         (and (stringp colour) colour))))

(defun my-selected-window-bar--preferred-colour ()
  "Return the colour `my-selected-window-bar' asks for, or nil for none."
  (cond ((stringp my-selected-window-bar) my-selected-window-bar)
        ((eq my-selected-window-bar 'auto)
         (seq-find #'identity
                   (list (my-selected-window-bar--face-colour
                          'doom-modeline-bar :background)
                         (my-selected-window-bar--face-colour
                          'mode-line-emphasis :foreground)
                         (my-selected-window-bar--face-colour
                          'highlight :background))))
        (t nil)))

(defun my-selected-window-bar--ok-p (window)
  "Return non-nil when WINDOW can show the bar."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (display-graphic-p (window-frame window))
       (> (car (window-fringes window)) 0)))

;; The window's first pixel column in the body is the vertical border drawn
;; between side-by-side windows, and the bar's bitmap is painted under it: the
;; body bar therefore starts one pixel in, at x+1.  The mode line's accent bar
;; and the first chip of the tab line start at x instead, so they look one
;; pixel out of line with it.  Both are drawn one pixel narrower, with that
;; first column painted in the border's own colour, which makes the line read
;; as the border running on through the mode line and the tab line.
;;
;; The mode line's column is left to the border unconditionally: doom-modeline
;; caches the accent image for the whole session (`doom-modeline-refresh-bars'
;; drops it) and one image serves every window, so it cannot vary by window.
;; What that costs is one border-coloured pixel at the frame's own left edge,
;; where the window has no border and its body bar starts at x instead.
(defun my-selected-window-bar--border-colour ()
  "Return the colour the vertical border between windows is drawn in."
  (or (my-selected-window-bar--face-colour 'vertical-border :foreground)
      (my-selected-window-bar--face-colour 'vertical-border :background)
      "black"))

(defun my-selected-window-bar--bar-image (face width height)
  "Build doom-modeline's accent bar image: WIDTH by HEIGHT, coloured by FACE.
Replaces `doom-modeline--create-bar-image' to give the image's first column the
border's colour; the rest of the bar keeps the accent, so the accent is one
pixel narrower than the body's bar but starts at the same place."
  (when (and (image-type-available-p 'pbm)
             (numberp width) (> width 0)
             (numberp height) (> height 0))
    (let* ((edge (min 1 (1- width)))
           (colour (or (face-background face nil t) "None")))
      (propertize " " 'display
                  (ignore-errors
                    (create-image
                     (concat (format "P1\n%i %i\n" width height)
                             (mapconcat (lambda (_row)
                                          (concat (make-string edge ?0)
                                                  (make-string (- width edge) ?1)))
                                        (number-sequence 1 height)
                                        "\n")
                             "\n")
                     'pbm t :scale 1 :foreground colour
                     :background (my-selected-window-bar--border-colour)
                     :ascent 'center))))))

(defun my-selected-window-bar--geometry (window)
  "Return the list (FRINGE HEIGHT WIDTH) for WINDOW's bar bitmap.
HEIGHT is a whole character, because a line bitmap is clipped to its row."
  (let* ((fringe (max 1 (or (car (window-fringes window)) 8)))
         (height (frame-char-height (window-frame window)))
         (width (min (max 1 my-selected-window-bar-width) fringe)))
    (list fringe height width)))

(defun my-selected-window-bar--define (bitmap height fringe width &optional align)
  "Define BITMAP as HEIGHT rows tall and FRINGE pixels wide, bar WIDTH wide.
Emacs centres a bitmap inside the fringe, which left a two-pixel gap between
the bar and the window's edge and put it out of line with the mode line's bar.
The bitmap is therefore defined at the fringe's full width with only its
leftmost WIDTH pixel columns set.  ALIGN places it in its row, `top' by
default."
  (define-fringe-bitmap bitmap
    (make-vector height (ash (1- (ash 1 width)) (- fringe width)))
    height fringe (or align 'top)))

(defun my-selected-window-bar--row-prefix (window)
  "Return the display string that puts the bar on one row of WINDOW."
  (let* ((geometry (my-selected-window-bar--geometry window))
         (fringe (nth 0 geometry))
         (height (nth 1 geometry))
         (width (nth 2 geometry)))
    (unless (and (equal height my-selected-window-bar--row-height)
                 (equal fringe my-selected-window-bar--row-fringe)
                 (equal width my-selected-window-bar--row-width)
                 my-selected-window-bar--row-bitmap)
      ;; Each geometry gets a bitmap of its own name: the fringe keeps the image
      ;; it drew for a bitmap name, so redefining the same name leaves the old
      ;; drawing on screen -- measured, a redefined bitmap kept its old rows
      ;; until a new name was used.
      (setq my-selected-window-bar--row-bitmap
            (intern (format "my-selected-window-bar-%dx%dx%d"
                            (+ height 2) fringe width)))
      ;; Two rows taller than the line, centred, so that each row's bitmap
      ;; covers the pixel above and below it with the same bar.  Measured on
      ;; this display, whose compositor scales the frame by 1.5, a bitmap as
      ;; tall as the line leaves a faint darkening at every line boundary:
      ;; each row's bitmap is a separate image and the scaling leaves its edge
      ;; partly uncovered.  With the overlap the bar measured one colour from
      ;; the top of the window to the mode line; without it, 125 rows of a
      ;; 1893-row window came out 18% darker.
      (my-selected-window-bar--define my-selected-window-bar--row-bitmap
                                      (+ height 2) fringe width 'center)
      (setq my-selected-window-bar--row-height height
            my-selected-window-bar--row-fringe fringe
            my-selected-window-bar--row-width width))
    (propertize " " 'display (list 'left-fringe
                                    my-selected-window-bar--row-bitmap
                                    'my-selected-window-bar-face))))

(defun my-selected-window-bar--filler-string (window height ends)
  "Return display text that paints the bar over HEIGHT pixels below the text.
HEIGHT is exact rather than a whole line, so the bar reaches the mode line
instead of stopping a few pixels short of it.  It is built from whole rows plus
one remainder row because a fringe bitmap is limited to 255 rows tall.  ENDS
says the buffer's last row is already empty, in which case the first row goes on
it rather than on a new row -- otherwise a blank, barless line would separate
the text from the bar."
  (let* ((geometry (my-selected-window-bar--geometry window))
         (fringe (nth 0 geometry))
         (char-h (nth 1 geometry))
         (width (nth 2 geometry))
         (full (/ height char-h))
         (extra (- height (* full char-h)))
         (row (my-selected-window-bar--row-prefix window)))
    (concat (unless ends "\n")
            (mapconcat #'identity (make-vector full row) "\n")
            (when (> extra 0)
              (let ((symbol (intern (format "my-selected-window-bar-fill-%dx%dx%d"
                                            extra fringe width))))
                (unless (equal symbol my-selected-window-bar--fill-bitmap)
                  ;; a new name, because a redefined bitmap is not redrawn
                  (setq my-selected-window-bar--fill-obsolete
                        my-selected-window-bar--fill-bitmap
                        my-selected-window-bar--fill-bitmap symbol)
                  (my-selected-window-bar--define symbol extra fringe width))
                (concat (if (or ends (> full 0)) "\n" "")
                        (propertize " " 'display (list 'left-fringe symbol
                                                        'my-selected-window-bar-face))
                        ;; the spacer is what makes the row exactly EXTRA pixels tall
                        (propertize " " 'display (list 'space :width '(1)
                                                        :height (list extra)))))))))

(defun my-selected-window-bar--window (&optional window)
  "Return the window that should show the bar, preferring WINDOW."
  (let ((buffer my-selected-window-bar--buffer))
    (cond ((and (window-live-p window) (eq (window-buffer window) buffer))
           window)
          ((and (window-live-p (selected-window))
                (eq (window-buffer (selected-window)) buffer))
           (selected-window))
          ((get-buffer-window buffer t))
          ((get-buffer-window buffer)))))

(defun my-selected-window-bar--fill (&optional window &rest _)
  "Size the filler so the bar runs from the last line to the mode line.
Runs from the window hooks -- which pass a window, a frame, or nothing -- and
from the buffer's own change hook, so it never trusts the current buffer."
  (when (and (buffer-live-p my-selected-window-bar--buffer)
             (overlayp my-selected-window-bar--filler))
    (let ((buffer my-selected-window-bar--buffer))
      (with-current-buffer buffer
        (let* ((window (my-selected-window-bar--window window))
               (char-h (and window (frame-char-height (window-frame window))))
               (string nil))
          (when (and char-h
                     (pos-visible-in-window-p (point-max) window)
                     (equal (window-start window) (point-min)))
            (let* ((ends (eq (char-before (point-max)) ?\n))
                   (posn (posn-at-point (point-max) window))
                   (y (and posn (cdr (posn-x-y posn))))
                   (top (and y (+ y (if ends 0 char-h))))
                   (inside (window-inside-pixel-edges window))
                   (body (- (nth 3 inside) (nth 1 inside)))
                   (height (and top (- body top))))
              (when (and height (> height 0) (<= height body))
                (setq string (my-selected-window-bar--filler-string
                              window height ends)))))
          (move-overlay my-selected-window-bar--filler
                        (point-max) (point-max) buffer)
          (unless (equal string
                         (overlay-get my-selected-window-bar--filler 'after-string))
            (overlay-put my-selected-window-bar--filler 'after-string string))
          ;; the previous filler bitmap is off screen now, so its name can go
          (let ((old my-selected-window-bar--fill-obsolete))
            (setq my-selected-window-bar--fill-obsolete nil)
            (when (and old (not (equal old my-selected-window-bar--fill-bitmap))
                       (fringe-bitmap-p old))
              (destroy-fringe-bitmap old))))))))

(defun my-selected-window-bar--install (buffer window)
  "Put the bar on BUFFER, which WINDOW is showing."
  (let ((prefix (my-selected-window-bar--row-prefix window)))
    (with-current-buffer buffer
      ;; Both forms are set: the buffer-local variable covers every line even
      ;; when the overlay's extent has been damaged (a terminal buffer erases
      ;; itself, and erasing shrinks an overlay to nothing), while the overlay
      ;; is what marks the window for redisplay -- a variable changes nothing
      ;; on screen by itself.  A text property takes precedence over the
      ;; variable, so the bar is still drawn once per line.
      (unless my-selected-window-bar--installed
        (setq my-selected-window-bar--saved-line-prefix line-prefix
              my-selected-window-bar--saved-wrap-prefix wrap-prefix
              my-selected-window-bar--installed t))
      (setq line-prefix prefix
            wrap-prefix prefix)
      (unless (overlayp my-selected-window-bar--bar)
        (setq my-selected-window-bar--bar (make-overlay (point-min) (point-max) buffer)))
      (overlay-put my-selected-window-bar--bar 'line-prefix prefix)
      (overlay-put my-selected-window-bar--bar 'wrap-prefix prefix)
      (unless (overlayp my-selected-window-bar--filler)
        (setq my-selected-window-bar--filler
              (make-overlay (point-max) (point-max) buffer)))
      (add-hook 'after-change-functions #'my-selected-window-bar--fill nil t))
    (force-window-update window)
    (my-selected-window-bar--fill window)))

(defun my-selected-window-bar--drop ()
  "Remove the bar from whichever buffer carries it."
  (when (buffer-live-p my-selected-window-bar--buffer)
    (with-current-buffer my-selected-window-bar--buffer
      (remove-hook 'after-change-functions #'my-selected-window-bar--fill t)
      (dolist (overlay (list my-selected-window-bar--filler my-selected-window-bar--bar))
        (when (overlayp overlay)
          (delete-overlay overlay)))
      (setq my-selected-window-bar--filler nil
            my-selected-window-bar--bar nil)
      (when my-selected-window-bar--installed
        (setq line-prefix my-selected-window-bar--saved-line-prefix
              wrap-prefix my-selected-window-bar--saved-wrap-prefix
              my-selected-window-bar--installed nil
              my-selected-window-bar--saved-line-prefix nil
              my-selected-window-bar--saved-wrap-prefix nil)
        (dolist (window (get-buffer-window-list my-selected-window-bar--buffer t t))
          (force-window-update window)))))
  (setq my-selected-window-bar--buffer nil))

(defun my-selected-window-bar-refresh (&rest _)
  "Put the bar on the selected window's buffer, take it off the last one.
Idempotent: it only reinstalls when the selected window's buffer changed,
which is what makes it safe on every window hook."
  (let* ((window (selected-window))
         (wanted (and my-selected-window-bar--colour
                      (my-selected-window-bar--ok-p window)
                      (window-buffer window))))
    (if (eq wanted my-selected-window-bar--buffer)
        ;; same buffer, possibly a different window or window height
        (my-selected-window-bar--fill window)
      (my-selected-window-bar--drop)
      (when wanted
        (setq my-selected-window-bar--buffer wanted)
        (my-selected-window-bar--install wanted window)))))

(defun my-selected-window-bar-reapply ()
  "Recompute the bar's colour and put it back where it belongs.
Call this after changing `my-selected-window-bar' or
`my-selected-window-bar-width' with a plain `setq'; Customize calls it by
itself, and so does a theme change."
  (interactive)
  (setq my-selected-window-bar--colour (my-selected-window-bar--preferred-colour))
  (when (facep 'my-selected-window-bar-face)
    (set-face-attribute 'my-selected-window-bar-face nil :foreground
                        (or my-selected-window-bar--colour
                            (my-selected-window-bar--face-colour
                             'fringe :foreground))))
  ;; force the bitmaps to be redefined for the new colour and size; a new size
  ;; gets a new bitmap name, which is what makes the fringe redraw it
  (setq my-selected-window-bar--row-height nil
        my-selected-window-bar--row-fringe nil
        my-selected-window-bar--row-width nil
        my-selected-window-bar--row-bitmap nil
        my-selected-window-bar--fill-bitmap nil
        my-selected-window-bar--fill-obsolete nil)
  (my-selected-window-bar--drop)
  (my-selected-window-bar-refresh))

;; The window change hooks are the right trigger: Emacs runs their default
;; value for frame events, so a buffer-local value cannot shadow them -- which
;; is why `doom-modeline' uses the same hook.  The bar is text on the lines it
;; belongs to, so it is repainted with those lines rather than needing a full
;; redraw of the frame.
(add-hook 'window-selection-change-functions #'my-selected-window-bar-refresh)
(add-hook 'window-buffer-change-functions #'my-selected-window-bar-refresh)
;; Two more ways the selection changes without the two above noticing: a
;; rebuilt layout (a tab or workspace switch restores a whole window
;; configuration) and a frame taking focus.
(add-hook 'window-configuration-change-hook #'my-selected-window-bar-refresh)
(add-function :after after-focus-change-function #'my-selected-window-bar-refresh)
;; A resize or a scroll changes how many lines the window has left over.
(add-hook 'window-size-change-functions #'my-selected-window-bar--fill)
(add-hook 'window-scroll-functions #'my-selected-window-bar--fill)
;; `C-c t' loads the other theme, which changes what `doom-modeline-bar' says.
(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'my-selected-window-bar-reapply))
;; doom-modeline draws the mode line's accent as a four-pixel image; give that
;; image the border-coloured first column, so the accent begins where the bar
;; down the body begins.
(when (and (fboundp 'doom-modeline--create-bar-image)
           (not (advice-member-p #'my-selected-window-bar--bar-image
                                 'doom-modeline--create-bar-image)))
  (advice-add 'doom-modeline--create-bar-image :override
              #'my-selected-window-bar--bar-image))
(my-selected-window-bar-reapply)

;; ----------------------------------------------------------------------
;; tabs
;; ----------------------------------------------------------------------

;; Both bars live in lisp/my-tabs.el: the tab bar as one pill-shaped chip
;; per project (a project switcher, over the `C-x t' commands), and the tab
;; line above each window as the current project's buffers, spelled
;; alphabetically, in the style of amitp.blogspot.com's prettier tab-line.
(require 'my-tabs)

;; ----------------------------------------------------------------------
;; terminal: ghostel
;; ----------------------------------------------------------------------

;; Ghostel -- terminal emulator on libghostty-vt. The native module was
;; downloaded already; `ghostel-module-auto-install' stays at its default
;; 'ask, so a package upgrade asks before fetching a new one.
(use-package ghostel
  :ensure t
  :bind (("C-c g" . ghostel))
  :init
  ;; Never ask before killing a terminal -- neither on exit ("Ghostel
  ;; buffers with running processes (...); exit anyway?") nor on `C-x k'
  ;; with a command running in it.  Both prompts come from
  ;; `ghostel--query-before-killing-p', which reads this variable.
  (setq ghostel-query-before-killing nil)
  :config
  ;; ALT+arrows are window navigation (`windmove', configured above), so in a
  ;; terminal they pass through to Emacs instead of being sent to the program.
  ;; `ghostel-keymap-exceptions' is the package's own list of such keys, and
  ;; its :set function rebuilds the semi-char keymap in place, so this takes
  ;; effect for terminals already open as well as for new ones.  Char mode
  ;; (C-c M-d) still sends every key, exceptions included.
  (customize-set-variable
   'ghostel-keymap-exceptions
   (append '("M-<left>" "M-<right>" "M-<up>" "M-<down>")
           ghostel-keymap-exceptions)))

;; ghostel-restore -- terminals come back with the session, in the directory
;; they were in, and a pi that was running in one is resumed with the same
;; session file. It registers `ghostel-mode' as an EasySession managed major
;; mode (see lisp/ghostel-restore.el for how the pi session is identified);
;; `M-x ghostel-restore-describe' in a terminal shows what would come back.
(use-package ghostel-restore
  :ensure nil                          ; lives in ~/.emacs.d/lisp
  ;; Loaded eagerly, not `:after (ghostel easysession)': EasySession looks
  ;; the mode up in its restore handler, which runs on `emacs-startup-hook'.
  ;; With `:after', the registration only appeared the first time a terminal
  ;; was opened (ghostel loads on demand), i.e. after the restore, and the
  ;; saved terminals were skipped in silence -- no error, no warning.
  :demand t
  :config (ghostel-restore-mode 1))

;; pi-sidecar -- the status/model/path panel for pi terminals.  Loaded here,
;; after ghostel's own configuration, because it requires ghostel.  Every tab
;; keeps it under the dired listing; `C-c P' hides it from all of them until
;; pressed again, and RET inside it visits the agent's terminal.
(require 'pi-sidecar)
;; Visiting an agent switches the frame to the agent's project first, so the
;; tab bar's chip and the tab line follow the terminal instead of the
;; terminal opening inside whichever project the frame was showing.
(setq pi-sidecar-switch-project-function #'my-tab-bar-select-project)
(global-set-key (kbd "C-c P") #'pi-sidecar-toggle)

;; dired-sidecar -- the workspace's directory listing in the left column,
;; above the pi sidecar.  Every tab shows the listing of its own project root,
;; each entry carries a bar with its state in the repository, and TAB turns a
;; directory into a subtree (dired-subtree, configured under "dired" above).
;; Loaded after pi-sidecar, so that when the listing is the one that creates
;; the left column it asks for the pi sidecar's width.  `C-c T' toggles it,
;; as does `M-x dired-sidecar-toggle' -- autoloaded, so it answers in `M-x'
;; even before this file is loaded, and aliased as `dired-sidebar-toggle'.
(require 'dired-sidecar)
(global-set-key (kbd "C-c T") #'dired-sidecar-toggle)

;; ----------------------------------------------------------------------
;; browser: embr
;; ----------------------------------------------------------------------

;; embr -- Emacs as the display server: a headless Chromium renders the page,
;; streams frames over CDP screencast into an image in the buffer, and passes
;; every key and click back to it, so typing, arrows and TAB behave as in a
;; browser while C-x/M-x stay Emacs'.  Frames go through the native canvas
;; path: Emacs 32 can hand a C module the pixel buffer of a canvas image, and
;; lisp/embr-fork/native/embr-canvas.so -- built once here with
;; `make -C native', it needs libjpeg-turbo's headers, already pulled in by
;; the chromium package -- writes decoded JPEG straight into it.  The older
;; path writes a JPEG per frame and is a third slower on input latency.
;;
;; The package is not on MELPA, and this is a *fork*, not a submodule:
;; lisp/embr-fork holds ordinary files forked from embr.el at commit 2b6725d,
;; so local changes are just edits (see UPSTREAM.md there) with no patch
;; file to reapply and no `git submodule update' to conflict with.  The
;; Python daemon (embr.py) and the C module sit next to embr.el, so it is
;; `:load-path' plus no `:ensure' rather than a package-install.
;;
;; Engine: Arch's own chromium package, named by
;; `embr-chromium-executable'.  Upstream drives it through Playwright and
;; would instead want either the CloakBrowser default (closed-source patched
;; Chromium, shipped to defeat bot detection) or Playwright's own private
;; Chromium build (~170MB into ~/.cache/ms-playwright).  The fork drops
;; Playwright: embr.py speaks CDP to `chromium --headless=new
;; --remote-debugging-pipe' over fds 3/4 with nothing but the Python standard
;; library, so there is no venv, no playwright package and no browser
;; download -- the daemon is `python3 embr.py'.  The ad blocklist (78K
;; domains from StevenBlack/hosts, filtered before they reach the network) is
;; ~/.local/share/embr/blocklist.txt.
;;
;; `embr-hover-rate' is the README's recommendation for the canvas backend
;; (30 is for the JPEG one).  Plain `setq' rather than `:custom' throughout,
;; for the reason given under magit.  Uncomment the `browse-url' line to make
;; embr the browser for every link in Emacs, org and help included.
(use-package embr
  :load-path "lisp/embr-fork"
  :defer t
  :bind (("C-c b" . embr-browse))
  :init
  (setq embr-chromium-executable "/usr/bin/chromium"
        ;; Viewport follows the Emacs window; the screen dimensions reported
        ;; to sites are the real ones, read back once at startup.
        embr-viewport-sizing 'dynamic
        embr-screen-width (or (display-pixel-width) 1920)
        embr-screen-height (or (display-pixel-height) 1080)
        embr-color-scheme 'dark
        embr-hover-rate 60
        embr-scroll-method 'instant
        embr-scroll-step 100
        embr-tab-bar t
        embr-session-restore t
        embr-home-url "about:blank"
        ;; browse-url-browser-function 'embr-browse
        ))

;; A browser buffer has no file of its own, so lisp/my-tabs.el files it under
;; the workspace that shows it -- but only while `project-current' answers nil.
;; Left alone it inherits `default-directory' from the buffer the page was
;; opened from, and a project there claims the buffer: clicking the page then
;; drags the frame to that project's tab.  That is how an HTML mail read from
;; the ~/email workspace kept jumping to ~/.emacs.d, the directory the article
;; buffer happened to be made in; the mu4e side of the same problem is fixed
;; where that buffer is configured.
(defun my-embr-buffer-directory ()
  "Give an embr buffer a directory that belongs to no project."
  (setq-local default-directory (expand-file-name "~/")))

(add-hook 'embr-mode-hook #'my-embr-buffer-directory)

;; ----------------------------------------------------------------------
;; quitting: no questions
;; ----------------------------------------------------------------------

;; Emacs asks, once per process no one else has vouched for, "Active
;; processes exist; kill them and exit anyway?" -- shell and eshell
;; buffers, TRAMP connections, compilation runs.  Nothing here is worth a
;; question: buffers are files on disk and the session (terminals, their
;; directories and pi sessions, see `ghostel-restore') is written by
;; `easysession--auto-save' while Emacs is still shutting down, so the
;; next start puts it all back.  The terminal packages' own versions of
;; this question are switched off where they are configured: ghostel with
;; `ghostel-query-before-killing', pilish with
;; `pilish-quit-without-confirmation'.
;;
;; Prompts about unsaved *files* are gone too, by saving instead of asking:
;; `C-x C-c' runs `my-save-and-exit' below, which is
;; `save-buffers-kill-emacs' with a non-nil ARG -- "silently save all
;; file-visiting buffers without asking".  So the exit is: sessions
;; written, files written, processes killed, nothing to answer.
(setq confirm-kill-processes nil)

(defun my-save-and-exit ()
  "Save every modified file buffer without asking, then exit Emacs.
Unlike plain `save-buffers-kill-emacs', nothing is asked: the ARG that
makes `save-some-buffers' save all is what the prefix would have done.
The session (`easysession--auto-save') and the pi/terminal state go first,
on `kill-emacs-query-functions'."
  (interactive)
  (save-buffers-kill-emacs t))

(global-set-key (kbd "C-x C-c") #'my-save-and-exit)

;; ----------------------------------------------------------------------
;; dired
;; ----------------------------------------------------------------------

;; Dired buffers follow the directory they list. `auto-revert-mode' is a
;; per-buffer minor mode, so it goes on `dired-mode-hook' rather than being
;; switched on globally: every Dired buffer, and nothing else, is watched.
;; Dired sets `buffer-stale-function' to `dired-buffer-stale-p', which is what
;; tells the mode when the listing has drifted from the disk, and
;; `revert-buffer-function' to `dired-revert', which re-reads the directory
;; while keeping point and marks. On a local directory the file notification
;; library (inotify here) gives a near-immediate refresh; where no watch can be
;; placed -- TRAMP, a filesystem without notifications -- it falls back to
;; polling every `auto-revert-interval' seconds. Remote directories are skipped
;; outright unless `auto-revert-remote-files' is set, so an open TRAMP Dired
;; buffer costs nothing. Dired is built in, hence `:ensure nil'. Plain `setq'
;; rather than `:custom' for the reason given under magit above.
(use-package dired
  :ensure nil
  :init
  ;; New, deleted and renamed entries now arrive on their own, so the
  ;; "Reverting buffer" line for every one of them would be noise.
  (setq auto-revert-verbose nil)
  ;; Walking into a directory -- RET, `^' or a mouse click -- replaces the
  ;; listing instead of opening a second Dired buffer for the new directory
  ;; and leaving the old one behind: the buffer being left is killed, as
  ;; `find-alternate-file' would. Emacs does this only while the Dired
  ;; buffer sits in one window, so a second window on the same listing is
  ;; left alone rather than pulled out from under it.
  (setq dired-kill-when-opening-new-dired-buffer t)
  :hook (dired-mode . auto-revert-mode))

;; dired-subtree -- TAB on a directory inserts that directory's contents,
;; indented, right below its line; TAB again takes them away.  This is the
;; tree the dired sidecar draws (see dired-sidecar.el), and it is why no file
;; tree package is needed: a subtree is part of the listing, so Dired's own
;; marks, commands and auto-revert apply to it unchanged.  The package ships
;; no key bindings of its own, hence the two here; `dired-subtree-cycle',
;; which opens one level per press and which the package also leaves unbound,
;; is left for `M-x' or a keymap of its own.
(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)))

;; ----------------------------------------------------------------------
;; icons in dired and ibuffer
;; ----------------------------------------------------------------------

(use-package nerd-icons-dired
  :ensure t
  ;; `dired-subtree' splices a subdirectory's lines into the listing itself and
  ;; only runs its own hooks -- `dired-after-readin-hook', which is where the
  ;; icons come from, never fires for them -- so an opened subtree's icons have
  ;; to be asked for after the insert.  The refresh is a no-op at top level and
  ;; in buffers where the mode is off.
  :hook ((dired-mode . nerd-icons-dired-mode)
         (dired-subtree-after-insert . nerd-icons-dired--refresh)))

;; Note: `nerd-icons-ibuffer-mode' is a buffer-local minor mode, not global.
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; ----------------------------------------------------------------------
;; helpful -- richer *Help* buffers
;; ----------------------------------------------------------------------

;; These replace the built-in describe-* commands, as the README suggests.
(use-package helpful
  :ensure t
  :bind (("C-h f"   . helpful-callable)
         ("C-h v"   . helpful-variable)
         ("C-h k"   . helpful-key)
         ("C-h x"   . helpful-command)
         ("C-h o"   . helpful-symbol)
         ("C-c C-d" . helpful-at-point)))

;; ----------------------------------------------------------------------
;; markdown
;; ----------------------------------------------------------------------

;; markdown-mode -- `markdown-command' already auto-detects (pandoc here),
;; so no :custom is needed.
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; markdown-toc -- upstream ships no binding; `C-c m' is free in
;; markdown-mode-map (C-c C-t is the header-insert prefix, C-c C-o follows
;; the link at point). Refreshes an existing TOC before regenerating it.
(use-package markdown-toc
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c m" . markdown-toc-generate-or-refresh-toc)))

;; ----------------------------------------------------------------------
;; org
;; ----------------------------------------------------------------------

;; Where the files live.
;;
;; Org keeps its notes in the llm-wiki tree. `org-directory' is the base the
;; relative names below resolve against, and `org-agenda-files' takes a
;; *directory*: it scans it one level deep with `directory-files', keeping
;; whatever matches `org-agenda-file-regexp' ("\`[^.].*\.org\'"). So the
;; `.org' files at that root -- `inbox.org', `journal.org', `archive.org' --
;; are the agenda, and one dropped there later joins it without this file
;; being edited. Nothing under `wiki/', `book/' or `raw/' is picked up, and
;; neither is anything in a nested project. That root is also a git
;; repository, so those three names are ignored in its own .gitignore: the
;; notes stay private, the repository is published.
(setq org-directory (expand-file-name "~/Documents/llm-wiki/")
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-agenda-files (list org-directory)
      ;; One archive file for the whole tree, made on the first `C-c C-x C-s'.
      org-archive-location
      (concat (expand-file-name "archive.org" org-directory) "::"))

;; Reading and writing.
;;
;; `org-startup-folded' is nil: headings, body text and source blocks all open
;; unfolded, so a file reads as one continuous document. Drawers are the
;; exception and stay folded -- `org-cycle-hide-drawer-startup' defaults to t,
;; which keeps `:PROPERTIES:' and the like closed on opening (TAB on the drawer
;; still opens it). `showeverything' would be too much: it is the one value
;; that also overrides `org-cycle-hide-drawer-startup', i.e. it spills every
;; property drawer into the text. A file that asks for folding itself with
;; `#+STARTUP: content' (or `overview', `fold') still wins: that keyword sets
;; `org-startup-folded' buffer-locally. `org-indent-mode' is deliberately left
;; off (`org-startup-indented' stays nil): org-modern below draws its block
;; brackets in the fringe and turns that drawing off as soon as the text is
;; indented too, so the two only work together through the separate
;; `org-modern-indent' package.
;; `org-hide-emphasis-markers' and `org-pretty-entities' hide the markup Org
;; is written in; org-appear (below) brings it back at point.
(setq org-startup-folded nil
      org-hide-emphasis-markers t          ; */_/= simplified in the buffer
      org-pretty-entities t                ; \alpha -> α
      ;; Render a file's existing LaTeX fragments when it opens.  The preview
      ;; minor mode below only tracks fragments you insert or edit and hides
      ;; or shows previews already overlaying the buffer, so without this a
      ;; freshly opened file stays source until `C-c C-x C-l'.
      org-startup-with-latex-preview t
      org-ellipsis " …"                    ; marker on a folded heading
      org-catch-invisible-edits 'show-and-error
      org-special-ctrl-a/e t               ; `C-a' first on the stars, then the line
      org-return-follows-link t
      org-insert-heading-respect-content t
      org-use-sub-superscripts '{}         ; a_b stays literal; _{b} is a subscript
      ;; Tags flush against the headline with no realigning on every edit:
      ;; both are what org-modern expects, since it draws tags as boxes.
      org-auto-align-tags nil
      org-tags-column 0
      org-agenda-tags-column 0)

;; TODO workflow.
;;
;; In the fast-selection loop `(t)' names the key that picks a state, `!'
;; records a timestamp on entering or leaving it, `@' asks for a note, and
;; `@/!' asks for one only when the state is entered. The `|' separates active
;; states from the finished ones the agenda treats as done.
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANC(c@)"))
      org-use-fast-todo-selection 'expert ; one letter, no selection window
      org-log-done 'time
      org-log-redeadline 'note            ; why a deadline moved
      org-log-reschedule 'note)           ; why a date moved

;; Agenda.
;;
;; Finished entries drop out of the scheduled and deadline lists, so an old
;; item stops nagging. `org-agenda-custom-commands' adds a second view on
;; `C-c a n': today's agenda with every open TODO under it, one key for the
;; daily review.
(setq org-deadline-warning-days 14
      org-agenda-window-setup 'current-window
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-custom-commands
      '(("n" "Today and all TODOs"
         ((agenda "")
          (alltodo "")))))

;; Capture.
;;
;; Three templates on `C-c c', all writing into the tree above. `file+headline'
;; makes the headline when it is missing, so a fresh `inbox.org' grows its
;; `Tasks' and `Notes' sections on first use. `%U' is the inactive timestamp
;; and `%a' the link back to the buffer capture was called from, which is
;; worth keeping when a task starts life in a wiki page or a source file.
(setq org-capture-templates
      `(("t" "Task"
         entry (file+headline ,(expand-file-name "inbox.org" org-directory)
                              "Tasks")
         "* TODO %?\n%U\n%a")
        ("n" "Note"
         entry (file+headline ,(expand-file-name "inbox.org" org-directory)
                              "Notes")
         "* %?\n%U\n")
        ("j" "Journal entry"
         entry (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
         "* %?\n%U\n")))

;; Refile (`C-c C-w').
;;
;; The targets are the agenda files' headings down to level 3, each offered
;; with its file path -- so two headings that happen to share a title stay
;; apart -- and picked in one `completing-read' call rather than step by step,
;; which is the form that suits vertico. A parent heading named by hand is
;; created only after confirmation.
(setq org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)

;; Source blocks and Babel.
;;
;; `ob-*' libraries load on demand, so every language a block may use is named
;; here. `org-confirm-babel-evaluate' stays at t: a block is code, and running
;; one should ask first.
(setq org-src-fontify-natively t           ; real highlighting inside a block
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)  ; no re-indent when `C-c '' returns

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (shell . t) (python . t) (C . t)))
  ;; `org-export-backends' defaults to (ascii html icalendar latex odt); `md'
  ;; is the one exporter worth adding, for pasting a page into the wiki.
  (require 'ox-md nil t)
  (setq org-export-backends '(ascii html icalendar latex md odt)))

;; Org itself is built in, hence `:ensure nil'. `C-c a' is the agenda, `C-c c'
;; capture and `C-c l' stores a link; the other single-letter `C-c' keys taken
;; in this file are `C-c t' (theme), `C-c T' (dired sidecar), `C-c e' (mail)
;; and `C-c g' (ghostel).
;;
;; ALT+arrows are window navigation everywhere but in Org, whose
;; `org-mode-map' binds them to structure editing: `org-metaleft' and
;; `org-metaright' promote and demote the heading at point, `org-metaup' and
;; `org-metadown' move a subtree (or a table row, or a list item) past its
;; neighbours.  Neither has another default key, so rather than lose them the
;; Org map gives the arrows up: they are the window moves here too, and the
;; structure commands move to the same arrows under `C-c', which Org leaves
;; unbound.
(use-package org
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("M-<left>" . windmove-left)
         ("M-<right>" . windmove-right)
         ("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)
         ("C-c <left>" . org-metaleft)
         ("C-c <right>" . org-metaright)
         ("C-c <up>" . org-metaup)
         ("C-c <down>" . org-metadown)))

;; LaTeX previews.
;;
;; `org-latex-preview-mode' is the new preview system: fragments are rendered
;; asynchronously with dvisvgm (the default `org-latex-preview-process-default')
;; and cached on disk, the images take their colours from the surrounding
;; text and follow theme changes, and the image is swapped back to source
;; while point is inside the fragment.  It works together with
;; `org-startup-with-latex-preview' above: the startup flag renders every
;; fragment in the file, the mode then keeps them current as you edit.  The
;; mode is buffer-local and its autoload only exists once the package's
;; autoloads are loaded, so the library is required here and the hook is safe
;; on the first Org buffer.
;; `C-c C-x C-l' (`org-latex-preview') stays the bulk toggle: the redefined
;; command in this library keeps the old key.  Live previews while typing are
;; `org-latex-preview-mode-display-live', defaulting to block math only.
(when (require 'org-latex-preview nil t)
  (add-hook 'org-mode-hook #'org-latex-preview-mode))

;; Hard wrap at 100 columns, on save.
;;
;; The breaks live in the file, not in the display: `my-org-fill-on-save'
;; refills the buffer's paragraphs to 100 columns as it is written, so what
;; used to be wrapped on screen is wrapped on disk. Paragraphs are refilled
;; whole, which is `M-q' (`org-fill-paragraph') applied to the file: short
;; lines join as well as long ones break. Tables, source blocks, example and
;; verse blocks, LaTeX environments, property drawers and fixed-width lines
;; are not paragraphs and keep the layout they were written in. Inside a
;; paragraph, `fill-nobreak-predicate' is given org's own
;; `org-inside-LaTeX-fragment-p', because the filler has no rule of its own for
;; inline maths (measured: it split `$a + b = c + d +' / `e$' across two
;; lines) and a fragment broken over two lines stops being a fragment. That is
;; the same task AUCTeX's `texmathp' does for LaTeX buffers, done with the org
;; parser instead: AUCTeX is not installed here, so `texmathp' would be a void
;; function in every Org buffer. The same 100 goes into `fill-column'
;; buffer-locally, so an interactive `M-q' fills to the width the save uses.
(defcustom my-org-fill-width 100
  "Column Org paragraphs are refilled to, on save and with `M-q'."
  :type 'integer)

(defun my-org-fill-on-save ()
  "Refill every paragraph of the current Org buffer to `fill-column'.
Run from `before-save-hook', buffer-locally in Org buffers.

The paragraphs are taken from one `org-element-parse-buffer' and filled from
the end of the buffer backwards: filling rewrites the text it contains, so a
pass from the top would still be working towards positions that had moved.
Only `paragraph' elements are filled, which is what keeps tables, source
blocks, example and verse blocks, LaTeX environments, drawers and
fixed-width lines out of it. `org-with-wide-buffer' makes the pass cover the
whole file even when the buffer is saved from a narrowed state, and the outer
`save-excursion' leaves point where the user left it."
  (when (and (derived-mode-p 'org-mode)
             (not buffer-read-only))
    (org-with-wide-buffer
     (let ((fill-column (or my-org-fill-width fill-column))
           (begins (org-element-map (org-element-parse-buffer) 'paragraph
                     (lambda (paragraph)
                       (org-element-property :begin paragraph)))))
       (save-excursion
         (dolist (begin (sort begins #'>))
           (save-excursion
             (goto-char begin)
             (org-fill-paragraph))))))))

(defun my-org-hard-wrap-100 ()
  "Fill Org text at `my-org-fill-width' when the buffer is saved."
  (setq-local fill-column my-org-fill-width)
  (add-hook 'before-save-hook #'my-org-fill-on-save nil t)
  ;; "Inside a fragment" is answered by the parser, at every break the filler
  ;; considers, so no AUCTeX is needed and `$...$', `\\(...\\)' and
  ;; `\\[...\\]' are all covered.
  (add-hook 'fill-nobreak-predicate #'org-inside-LaTeX-fragment-p nil t))

(add-hook 'org-mode-hook #'my-org-hard-wrap-100)

;; org-modern -- headline stars, TODO boxes, tag boxes, table rules and source
;; frames drawn with text properties, plus headlines in the `variable-pitch'
;; face. The two hooks are the per-buffer setup its README gives; the agenda
;; needs the second one because it builds its buffer outside `org-mode'.
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;; org-appear -- the counterpart to the hidden markup above: emphasis markers,
;; link targets, entities and sub/superscripts reappear while point is inside
;; the element. Each of its four switches acts only where the matching org
;; variable hides the text, which is why all four can be on here.
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

;; ----------------------------------------------------------------------
;; mail: mu4e, mbsync and org-msg
;; ----------------------------------------------------------------------

;; Reading mail is two programs behind one buffer, with two more on the
;; outside: mbsync copies the Gmail mailboxes into a local Maildir over IMAP,
;; mu indexes that Maildir, and mu4e is the Emacs front end to the index.
;; Sending goes out through msmtp.  The outside half of it lives in files this
;; one only points at:
;;
;;   ~/.mbsyncrc        Gmail IMAP account, one channel per folder, and the
;;                      app password.  Every folder mbsync brings in is a
;;                      maildir under ~/Mail/gmail.
;;   ~/.msmtprc         Gmail submission through msmtp, same app password.
;;
;; `mu4e-get-mail-command' is what ties it together: mu4e runs `mbsync -a'
;; (often enough that `mu4e-update-interval' below matters), then hands the
;; changed Maildir to mu, which reindexes it and tells the views.
;;
;; The one mu4e setting Gmail cannot do without is
;; `mu4e-change-filenames-when-moving': mu4e records a flag change by
;; renaming the maildir file, and mbsync identifies a message by its name, so
;; without the rename mbsync would take the flagged message for a new one and
;; download the original back.

;; mu4e ships inside mu, not as an Emacs package, and the AUR build
;; (`yay -S mu', the binary is /usr/bin/mu) drops its lisp here.  The block
;; below is skipped when the directory is missing, so a machine without mu
;; still starts Emacs -- only the mail commands are absent.
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; Everything mail-related hangs off this one `when', closed after the icons
;; at the end of the section: without mu there is no `mu4e-compose-mode-hook'
;; for org-msg to hook, and no folders for the settings below to name.
(when (require 'mu4e nil t)

  ;; The mail buffers carry no file of their own, so lisp/my-tabs.el files
  ;; them under the workspace whose tab shows them -- but only while
  ;; `project-current' answers nil.  mu4e leaves the article and main buffers
  ;; with the directory of whatever buffer was current when they were made,
  ;; which here was ~/.emacs.d: a project, and its tab then took the frame
  ;; over as soon as one of them was clicked.  The workspace's own directory
  ;; belongs to no project, so they stay where they are shown.
  (defun my-mail-buffer-directory ()
    "Keep a mu4e buffer out of every project's workspace."
    (setq-local default-directory (expand-file-name "~/email")))

  (add-hook 'mu4e-view-mode-hook #'my-mail-buffer-directory)
  (add-hook 'mu4e-main-mode-hook #'my-mail-buffer-directory)

  ;; `mbsync -a' is what actually fetches; mu4e runs it, then reindexes.
  (setq mu4e-get-mail-command "mbsync -a"
        ;; Pull every five minutes, in the background, and reindex after
        ;; every pull.  A Gmail account with All Mail synced makes the first
        ;; sync the slow one; after that it is a few seconds.
        mu4e-update-interval 300
        mu4e-index-update-in-background t
        mu4e-change-filenames-when-moving t
        ;; The config's rule for exits and processes (see "quitting" above):
        ;; nothing about mail is worth a confirmation.
        mu4e-confirm-quit nil
        ;; vertico reads the prompts mu4e makes for folders, senders and
        ;; addresses; its own default is ido.
        mu4e-completing-read-function #'completing-read
        ;; All Mail duplicates the inbox and sent mail on disk.  Skipping
        ;; duplicates by Message-Id keeps one hit per message in searches;
        ;; the limit is off so a maildir query is not silently cut at 500.
        mu4e-search-skip-duplicates t
        mu4e-search-results-limit -1
        ;; Folders, relative to the root maildir.
        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder "/Sent"
        mu4e-trash-folder "/Trash"
        ;; Refiling to All Mail is how mu4e's manual archives on Gmail: mbsync
        ;; takes the file out of INBOX, which is what removes Gmail's Inbox
        ;; label, and files it under All Mail.  All Mail is synced here, so
        ;; whatever copy this leaves behind is on disk rather than lost, and
        ;; `mu4e-search-skip-duplicates' keeps it out of the search results.
        mu4e-refile-folder "/All Mail"
        ;; Gmail keeps its own copy of what you send, so mu4e must not save a
        ;; second one -- that is exactly the duplicate this setting avoids.
        mu4e-sent-messages-behavior 'delete
        ;; Headers on the left, the message on the right, both in this frame:
        ;; the folder view takes the frame's top left side window (see
        ;; `dired-sidecar-substitutes' below), so the three panes are the side
        ;; column, the headers and the article.  `display-buffer-alist'
        ;; decides how mu4e's own `display-buffer-in-direction' shares the
        ;; width; see `my-mail-display-article' further down.
        mu4e-split-view 'vertical
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-attachment-dir "~/Downloads"
        ;; Keys for the maildir shortcuts in the main view.
        mu4e-maildir-shortcuts
        '((:maildir "/INBOX" :key ?i)
          (:maildir "/All Mail" :key ?a)
          (:maildir "/Sent" :key ?s)
          (:maildir "/Drafts" :key ?d)
          (:maildir "/Trash" :key ?t))
        ;; Who we are, and where replies are sent from.
        user-mail-address "edgar1denny@gmail.com"
        user-full-name "Edgar Denny"
        ;; Sending: msmtp, called the way sendmail is, with `-t' so it reads
        ;; the recipients from the headers.  Nothing blocks while it runs.
        sendmail-program "msmtp"
        send-mail-function #'sendmail-send-it
        message-send-mail-function #'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t)

  ;; The inbox gets a bookmark of its own and is the one the modeline counts,
  ;; since "unread" would otherwise count All Mail's copies too.
  (add-to-list 'mu4e-bookmarks
               '(:name "Inbox"
                 :query "maildir:/INBOX AND flag:unread"
                 :key ?i
                 :favorite t)
               t)

  ;; org-msg, which composes and replies in Org and exports the HTML that the
  ;; other end expects.  It is a global minor mode: switching it on installs
  ;; `org-msg-post-setup' on `mu4e-compose-mode-hook' -- for mu4e it hooks
  ;; nothing else -- and it decides which functions to hook by reading
  ;; `mail-user-agent', so that has to name mu4e first.  `:after mu4e' keeps
  ;; the order when mu is not installed and the `when' above did nothing.
  (use-package org-msg
    :ensure t
    :after mu4e
    :init
    (setq mail-user-agent 'mu4e-user-agent)
    ;; htmlize, which org-msg uses to color the exported HTML, comes in as a
    ;; dependency.  Plain `setq' rather than `:custom', for the reason given
    ;; under magit above.
    (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil\\n:t")
    ;; No `hidestars indent' startup flags: org-modern draws this config's Org
    ;; buffers, and `org-msg-edit-mode' derives from `org-mode', so the compose
    ;; buffer already looks like the rest of Org here.
    (setq org-msg-startup nil)
    ;; Greet the person being replied to by first name, the Outlook way.  `%s'
    ;; becomes " Mark" or nothing when there is no name to use.
    (setq org-msg-greeting-fmt "\nHi%s,\n\n")
    (setq org-msg-greeting-name-limit 1)
    ;; New mail and replies to HTML mail go out as both a plain-text and an
    ;; HTML part; a reply to a plain-text message stays plain text, so mailing
    ;; lists keep getting text.
    (setq org-msg-default-alternatives '((new . (text html))
                                         (reply-to-html . (text html))
                                         (reply-to-text . (text))))
    (setq org-msg-convert-citation t)
    :config
    (org-msg-mode 1))

  ;; The mail windows are windows of one workspace: the `~/email' chip in the
  ;; tab bar.  mu4e's buffers carry no file of their own, so lisp/my-tabs.el
  ;; files them under the workspace whose tab they are shown in -- which is
  ;; why mu4e is entered by way of the workspace rather than from wherever
  ;; point happens to be.  `my-tab-bar-select-project' makes the tab when it
  ;; does not exist yet, so this is also the first-run command.
  (defun my-mail ()
    "Start mu4e in the email workspace, on the inbox.
The frame switches to that workspace's tab first, so the main, headers,
article and compose buffers are all tabs of the email workspace instead of
tabs of whichever project the frame was showing.  The inbox is searched
after `mu4e' because that command puts the main view in the selected window
as well as in the side column, where this config already keeps it; the
headers take the window over and the redundant copy goes away."
    (interactive)
    (my-tab-bar-select-project (expand-file-name "~/email"))
    (mu4e)
    (mu4e-search-maildir "/INBOX"))

  ;; The workspace's folder view is the mu4e main buffer, in the top left side
  ;; window that `dired-sidecar' keeps.  A listing of `~/email' would be a
  ;; listing of a directory that holds no files, and the main view is the one
  ;; thing in mu4e that answers "which folder, and how much is unread in it" --
  ;; its maildir shortcuts are clickable and refresh their counts themselves.
  ;; The function answers nil until mu4e has made the buffer, so the workspace
  ;; entered before mail is started shows its listing and nothing breaks.
  (defun my-mail-main-buffer ()
    "Return the mu4e main view buffer, once mu4e has created it.
`dired-sidecar-substitutes' calls this for the `~/email' workspace."
    (let ((buffer (get-buffer mu4e-main-buffer-name)))
      (when (and buffer (with-current-buffer buffer
                          (derived-mode-p 'mu4e-main-mode)))
        buffer)))

  (add-to-list 'dired-sidecar-substitutes
               (cons (directory-file-name (expand-file-name "~/email"))
                     #'my-mail-main-buffer))

  ;; The main view is wider than a listing is: its longest line -- the
  ;; database path under "Info", tab stops expanded -- is 60 columns, and the
  ;; maildir and bookmark counts push their lines past 45.  The column is the
  ;; whole left side area, so the pi sidecar is this wide in this workspace
  ;; too, and every other workspace goes back to the listing's width.  The
  ;; number is the window's total width; its body is two columns narrower, so
  ;; 64 gives the 60-column line a couple of columns to grow into.
  (add-to-list 'dired-sidecar-widths
               (cons (directory-file-name (expand-file-name "~/email")) 64))

  ;; `(mu4e)' draws the main view itself, and with a split view on it asks for
  ;; the whole frame (`display-buffer-full-frame' is its choice for that
  ;; buffer), which would cover the headers and the article.  Sent to the
  ;; sidecar column instead, it lands in the window the substitute keeps.
  (add-to-list 'display-buffer-alist
               `("\\*mu4e-main\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (slot . 0)
                 (window-width . ,(dired-sidecar--requested-width))
                 (preserve-size t)))

  ;; What is left of the frame after the side column is shared equally by the
  ;; headers and the article, and neither moves again when a message is
  ;; opened.  `display-buffer-in-direction' cannot do that on its own here --
  ;; it ignores a `window-width' entry -- so the split is done by hand, and
  ;; the size entries mu4e adds (`window-min-width' asked for
  ;; `fit-window-to-buffer', which grew the article on every display) are
  ;; dropped on the way through.
  (defvar my-mail-article-share 0.5
    "Share of the headers window's width the article takes.")

  (defun my-mail--without-size-params (alist)
    "Return ALIST without the entries that resize the window it lands in."
    (seq-remove (lambda (entry)
                  (memq (car-safe entry)
                        '(window-min-width window-min-height window-size
                          window-width window-height)))
                alist))

  (defun my-mail-display-article (buffer alist)
    "Display BUFFER in a right-hand split of the window showing the headers.
BUFFER takes `my-mail-article-share' of that window's width, measured in
columns, and the headers keep the rest.  The window mu4e recorded for the
article is reused, whichever buffer it holds at the time: an HTML message
hands that window to embr, and the next message has to take it back."
    (let* ((reference (or (get-buffer-window "*mu4e-headers*")
                          (selected-window)))
           (recorded (bound-and-true-p mu4e~headers-view-win))
           (existing (or (and (windowp recorded) (window-live-p recorded) recorded)
                         (get-buffer-window buffer)))
           (alist (my-mail--without-size-params alist)))
      (if (and (windowp existing) (window-live-p existing)
               (not (eq existing reference)))
          (window--display-buffer buffer existing 'reuse alist)
        (let ((window (split-window
                       reference
                       (- (max 40 (floor (* my-mail-article-share
                                             (window-body-width reference)))))
                       'right)))
          (window--display-buffer buffer window 'window alist)))))

  (add-to-list 'display-buffer-alist
               '("\\*mu4e-article\\*" (my-mail-display-article)))

  ;; A message is opened from the headers list, but `mu4e-view' ends by
  ;; selecting the window it displayed the article in
  ;; (`select-window mu4e~headers-view-win'), so every message read out of a
  ;; folder pulled the cursor into the article -- and for HTML mail into the
  ;; embr page -- and the next `n' went to the wrong buffer.  The window the
  ;; command came from takes the selection back.  Opened from anywhere else
  ;; -- `n'/`p' while the article already holds the selection, `y' between the
  ;; two -- mu4e's own choice stands; the article's window is still the
  ;; selected window while the buffer is rendered, so the rendering hooks that
  ;; depend on it are unaffected.
  (defun my-mail-keep-focus-in-headers (view-function &rest args)
    "Run VIEW-FUNCTION and give the selection back to the headers window.
The selection only moves if VIEW-FUNCTION took it out of a window that is
showing a `mu4e-headers-mode' buffer."
    (let ((origin (selected-window)))
      (unwind-protect
          (apply view-function args)
        (when (and (window-live-p origin)
                   (not (eq (selected-window) origin))
                   (buffer-live-p (window-buffer origin))
                   (with-current-buffer (window-buffer origin)
                     (derived-mode-p 'mu4e-headers-mode)))
          (select-window origin)))))

  (unless (advice-member-p #'my-mail-keep-focus-in-headers 'mu4e-view)
    (advice-add 'mu4e-view :around #'my-mail-keep-focus-in-headers))

  ;; HTML mail is rendered by Chromium in embr instead of being flattened by
  ;; shr: the article's window keeps its place in the layout and only its
  ;; buffer changes, so the message arrives with its own styling, its images
  ;; and its real links.  Text mail is left to mu4e, which is why this hangs
  ;; off the rendering hook rather than the display rule: the part list is
  ;; only known once Gnus has decoded the message.
  (defcustom my-mail-html-in-embr t
    "Non-nil means show HTML mail in embr instead of the article buffer.
Text mail goes to the article buffer either way."
    :type 'boolean)

  (defun my-mail--html-part-p ()
    "Non-nil when the article being rendered has a text/html part.
It reads Gnus's handle alist, so it only means anything from
`mu4e-view-rendered-hook'."
    (and (fboundp 'mu4e--view-mime-handle)
         (mu4e--view-mime-handle "text/html")
         t))

  (defun my-mail--embr-window ()
    "The window a mail's browser page belongs in, making one if there is none.
That is the window mu4e records for the article -- unless it has been taken
over by the headers again -- then whatever window already shows embr, then
the article's own window, and failing all of those the right half of the
headers window, which is where a message would have gone."
    (or (and (bound-and-true-p mu4e~headers-view-win)
             (window-live-p mu4e~headers-view-win)
             (not (eq (window-buffer mu4e~headers-view-win)
                      (get-buffer "*mu4e-headers*")))
             mu4e~headers-view-win)
        (and (bound-and-true-p embr--normal-buffer)
             (get-buffer-window embr--normal-buffer))
        (get-buffer-window "*mu4e-article*")
        (when (derived-mode-p 'mu4e-headers-mode)
          (split-window
           (selected-window)
           (- (max 40 (floor (* my-mail-article-share
                                (window-body-width (selected-window))))))
           'right))))

  (defun my-mail-embr-browse-url (url &rest _)
    "Open URL in embr, in the window that shows the mu4e article.
`mu4e-action-view-in-browser' hands its temporary HTML file to this,
through `browse-url-browser-function', and `my-mail-show-images' calls it
directly."
    (require 'embr)
    (let ((window (my-mail--embr-window))
          ;; A mail is not the browsing session that was saved last: start it
          ;; without restoring those tabs, so the mail is the only page up.
          ;; The flag is set with `setq' rather than bound with `let': in a
          ;; lexical-binding file a `defvar' here would not make it special,
          ;; so the binding would be a no-op, and embr's own `defcustom' is
          ;; what has to be respected.
          (restore embr-session-restore))
      (with-selected-window (if (window-live-p window) window (selected-window))
        (if (embr--buffer-daemon-live-p (bound-and-true-p embr--normal-buffer))
            ;; A mail and its images belong in one tab, so a tab already
            ;; showing a message is navigated; one showing anything else gets
            ;; the mail in a new tab, since that page is not ours to replace.
            (let ((shown (buffer-local-value 'embr--current-url embr--normal-buffer)))
              (set-window-buffer (selected-window) embr--normal-buffer)
              (if (and (stringp shown)
                       (string-prefix-p "file:///tmp/mu4e-" shown))
                  (with-current-buffer embr--normal-buffer
                    (embr-navigate url))
                (embr-browse url)))
          (unwind-protect
              (progn (setq embr-session-restore nil)
                     (embr-browse url))
            (setq embr-session-restore restore))))))

  ;; mu4e's HTML carries a Content-Security-Policy of `img-src data:', so
  ;; only images that arrived inside the message are displayed; the rest are
  ;; refused by the browser, which is the point.  When the images are wanted,
  ;; this rebuilds the same document with `https:' and `http:' added to that
  ;; one directive -- scripts, remote styles, forms and the base URI stay
  ;; blocked -- and shows it in embr.  Nothing loads until this is called:
  ;; the images come because they were asked for.  It works from the headers
  ;; as well as from the article, since for HTML mail the article's window is
  ;; showing embr and not the message.
  (defun my-mail--message ()
    "The message to act on: the article's, or the one at point."
    (or (and (derived-mode-p 'mu4e-view-mode) mu4e--view-message)
        (mu4e-message-at-point t)))

  (defun my-mail-show-images ()
    "Show this article in embr with its remote images allowed."
    (interactive)
    (let ((msg (my-mail--message)))
      (unless msg
        (user-error "No message at point"))
      (let* ((html (mu4e-view-html-text msg (mu4e--view-html-insert-headers msg)))
             (allowed (replace-regexp-in-string "img-src data:"
                                                "img-src data: https: http:"
                                                html t t)))
        (my-mail-embr-browse-url
         (concat "file://" (mu4e--view-html-temp-file allowed))))))

  (define-key mu4e-view-mode-map (kbd "I") #'my-mail-show-images)
  (define-key mu4e-headers-mode-map (kbd "I") #'my-mail-show-images)

  ;; ALT+arrows are window navigation everywhere in this config, and mu4e was
  ;; the exception twice over: `mu4e-search-minor-mode-map' takes
  ;; M-<left>/M-<right> for its search history -- a minor-mode map, so it
  ;; outranks every major mode and the global `windmove' bindings -- and the
  ;; headers and view maps take M-<up>/M-<down> for the previous and next
  ;; message.  Those two stay on `p' and `n', and the search history moves to
  ;; C-<left> and C-<right>, next to the `\' that already means "previous
  ;; search".  It goes in the headers and main maps rather than the minor
  ;; one, so that C-<left>/C-<right> keep meaning `left-word'/`right-word' in
  ;; the article, where text is actually read and selected.
  (dolist (map (list mu4e-search-minor-mode-map
                     mu4e-headers-mode-map
                     mu4e-view-mode-map))
    (when (keymapp map)
      (define-key map (kbd "M-<left>") #'windmove-left)
      (define-key map (kbd "M-<right>") #'windmove-right)
      (define-key map (kbd "M-<up>") #'windmove-up)
      (define-key map (kbd "M-<down>") #'windmove-down)))

  (dolist (map (list mu4e-headers-mode-map mu4e-main-mode-map))
    (when (keymapp map)
      (define-key map (kbd "C-<left>") #'mu4e-search-prev)
      (define-key map (kbd "C-<right>") #'mu4e-search-next)))

  (defun my-mail-html-in-embr-maybe ()
    "Show the article in embr when it is HTML; leave text to mu4e."
    (when (and my-mail-html-in-embr
               (derived-mode-p 'mu4e-view-mode)
               (my-mail--html-part-p))
      (condition-case error
          (let ((browse-url-browser-function #'my-mail-embr-browse-url))
            (mu4e-action-view-in-browser mu4e--view-message))
        (error (message "my-mail: html mail in embr failed: %S" error)))))

  ;; mu4e resizes the headers window to `mu4e-headers-visible-columns' (30 by
  ;; default) on every render, which is what let the message pane grow over
  ;; the headers.  The split above sets both widths, and keeps them, so the
  ;; function leaves the hook.
  (setq mu4e-view-rendered-hook
        (delq 'mu4e-resize-linked-headers-window mu4e-view-rendered-hook))
  (add-hook 'mu4e-view-rendered-hook #'my-mail-html-in-embr-maybe)

  ;; The tab line draws a file-type icon per tab, and mu4e's buffers are in
  ;; modes `nerd-icons' knows nothing about, so they would all wear the Emacs
  ;; logo that stands in for `fundamental-mode' -- the same gap ghostel had.
  ;; `nerd-icons-icon-for-mode' walks a mode's parents, so `org-msg-edit-mode'
  ;; (derived from `org-mode') and `mu4e-view-mode' (from
  ;; `gnus-article-mode') keep their own icons.
  (dolist (icon '((mu4e-main-mode nerd-icons-mdicon "nf-md-email_outline"
                     :face nerd-icons-lblue)
                  (mu4e-headers-mode nerd-icons-mdicon "nf-md-inbox"
                     :face nerd-icons-lblue)
                  (mu4e-view-mode nerd-icons-mdicon "nf-md-email_open_outline"
                     :face nerd-icons-lorange)
                  (mu4e-compose-mode nerd-icons-mdicon "nf-md-email_edit_outline"
                     :face nerd-icons-lyellow)))
    (add-to-list 'nerd-icons-mode-icon-alist icon))

  (global-set-key (kbd "C-c e") #'my-mail))

;; ----------------------------------------------------------------------
;; pilish -- Emacs frontend for the pi coding agent
;; ----------------------------------------------------------------------

;; MELPA package; `:ensure t' pulls in transient, magit-section, md-ts-mode
;; and markdown-table-wrap. The installed Emacs 31.1 `transient' already
;; satisfies pilish's >= 0.9 requirement, so no GNU ELPA `compat' upgrade is
;; needed. `defalias' gives the short `M-x pi' entry point, and the alias
;; must come after the package is loaded, hence `:init'.  The quit
;; confirmation is off for the same reason ghostel's is (see "quitting"
;; above): a live pi process is not worth an exit question.
(use-package pilish
  :ensure t
  :init
  (defalias 'pi 'pilish)
  (setq pilish-quit-without-confirmation t))

(use-package diminish
  :after use-package)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(## cape consult corfu diff-hl diminish dired-subtree doom-modeline
	doom-themes easysession ef-themes embark embark-consult
	ghostel git helpful indent-bars magit marginalia markdown-mode
	markdown-toc nerd-icons nerd-icons-corfu nerd-icons-dired
	nerd-icons-ibuffer org org-appear org-modern outline-indent pilish
	rainbow-delimiters vertico with-editor)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
