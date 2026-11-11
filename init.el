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
;; in this file are `C-c t' (theme), `C-c T' (dired sidecar) and `C-c g'
;; (ghostel).
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

;; Soft wrap at 100 columns.
;;
;; A fixed measure rather than the window width: `visual-fill-column-mode'
;; sets the window margins from `fill-column', so a paragraph wraps at column
;; 100 even in a full-frame window. Nothing in the file changes -- this is
;; display only -- and in a window narrower than 100 columns the margins
;; collapse to 0, so text still fills the frame. `visual-line-mode' is the
;; companion it expects: it turns on `word-wrap', so the break falls between
;; words rather than mid-word, and makes movement act on the lines shown
;; instead of the long ones stored. `fill-column' is set buffer-locally to the
;; same 100 so that `M-q' (`org-fill-paragraph') fills to the measure on
;; screen.
(defun my-org-soft-wrap-100 ()
  "Wrap text in the current buffer at 100 columns."
  (setq-local fill-column 100)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . my-org-soft-wrap-100))

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
