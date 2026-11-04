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
;; up.  Both companion packages live in ~/.emacs.d/lisp (already on
;; `load-path' above):
;;
;;   pi-subagents.el  live views of subagent runs, in a term buffer
;;   pi-annotate.el   /annotate: mark up a file beside its review sidecar

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'pi-subagents)
(require 'pi-annotate)

;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; completion stack: vertico + consult + corfu + marginalia + embark
;; ----------------------------------------------------------------------

;; Vertico -- vertical completion UI in the minibuffer.
;; Keys are already good by default (remapped): M-n/M-p or C-n/C-p move,
;; TAB/SPC complete, RET submit, C-g cancel.
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

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
;; which-key and project (both built in)
;; ----------------------------------------------------------------------

;; which-key -- show the completions of a prefix key after a short delay.
;; Already enabled by default in Emacs 30+; these are the usual tunings.
(use-package which-key
  :ensure nil                          ; built in
  :init
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.05
        which-key-add-column-padding 1
        which-key-show-early-on-C-h t))

;; project -- per-project commands on the built-in `C-x p' prefix.
(use-package project
  :ensure nil                          ; built in
  :init
  (setq project-list-file (expand-file-name "projects" user-emacs-directory)
        project-vc-extra-root-markers '(".project" ".projectile"
                                        "compile_commands.json" "pyproject.toml")
        ;; What `C-x p p' offers; consult replaces `switch-to-buffer' here.
        project-switch-commands '((project-find-file "Find file")
                                  (consult-project-buffer "Buffer")
                                  (project-find-regexp "Find regexp")
                                  (project-dired "Dired")))
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
  :bind (("C-c t" . my-toggle-theme))
  :config
  ;; Its extras restyle other packages in palette terms; treemacs is loaded
  ;; later in this file, so wait for it.
  (with-eval-after-load 'treemacs
    (doom-themes-treemacs-config)))

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

;; Tab bar -- window-configuration tabs across the top of the frame, under
;; the `C-x t' prefix (see the treemacs note above). Both are built in, so
;; no `use-package'. `tab-bar-show' defaults to t here (Emacs 31), so the
;; bar is always visible; set it to 1 to show it only once a second tab
;; exists, or nil to hide it while keeping the `C-x t' commands.
(tab-bar-mode 1)

;; The bar as a project switcher
;;
;; A tab holds a window configuration, so one tab per project is what keeps
;; each project's layout -- but `tab-bar.el' never consults `project.el', and
;; its own chips are named after buffers, which the tab line above each
;; window already lists. So drop `tab-bar-format-tabs' from `tab-bar-format'
;; and draw one chip per project instead: the same list `C-x p p' prompts
;; with (`project-known-project-roots'). Clicking a chip selects that
;; project's tab, creating it the first time. A tab's `group' entry records
;; which project the tab belongs to: it holds the project root, which is what
;; identifies a project, while the chip shows that root abbreviated under
;; $HOME, the way `C-x p p' spells it.
(require 'project)

(defun my-tab-bar-project-directory (root)
  "Project ROOT without its trailing slash."
  (directory-file-name (expand-file-name root)))

(defun my-tab-bar-project-name (root)
  "Chip label for project ROOT: its full path, abbreviated under $HOME.
So `~/.pi' and `~/Documents/llm' rather than bare directory names, which is
how `C-x p p' spells them too; two projects called `name' therefore stay
distinguishable."
  (abbreviate-file-name (my-tab-bar-project-directory root)))

(defun my-tab-bar-project-tab-index (root)
  "Index among `tab-bar-tabs-function' of the tab for project ROOT.
Nil when no tab belongs to it yet."
  (let ((dir (my-tab-bar-project-directory root)))
    (cl-loop for tab in (funcall tab-bar-tabs-function)
             for i from 0
             when (and (alist-get 'group tab)
                       (equal (my-tab-bar-project-directory
                               (alist-get 'group tab))
                              dir))
             return i)))

(defun my-tab-bar-select-project (root)
  "Switch to the tab for project ROOT, creating that tab if necessary.
The new tab shows the project's most recent buffer, or Dired at ROOT when
the project has no buffer yet. A remembered project whose directory has
since disappeared says so instead of half-creating a tab."
  (if-let ((index (my-tab-bar-project-tab-index root)))
      (tab-bar-select-tab (1+ index))
    (let* ((proj (ignore-errors (project-current nil root)))
           (buf  (and proj (car (project-buffers proj)))))
      (unless (or (buffer-live-p buf) (file-directory-p root))
        (user-error "Project directory missing: %s" root))
      (tab-bar-new-tab)
      (if (buffer-live-p buf)
          (switch-to-buffer buf)
        (dired root))
      (tab-bar-rename-tab (my-tab-bar-project-name root))
      (tab-bar-change-tab-group (my-tab-bar-project-directory root)))))

(defun my-tab-bar-format-projects ()
  "Produce one clickable chip per known project.
Chip keys are `project-N' rather than `tab-N', which keeps
`tab-bar-auto-width' from stretching them across the whole bar."
  (let* ((current (ignore-errors (project-current)))
         (current-dir (and current
                           (my-tab-bar-project-directory
                            (project-root current))))
         (i 0))
    (mapcan
     (lambda (root)
       (setq i (1+ i))
       (let* ((dir (my-tab-bar-project-directory root))
              (current-p (equal dir current-dir)))
         `((,(intern (format "sep-project-%d" i)) menu-item
            ,(tab-bar-separator) ignore)
           (,(intern (format "project-%d" i)) menu-item
            ,(propertize (my-tab-bar-project-name root)
                         'face (if current-p
                                   'tab-bar-tab 'tab-bar-tab-inactive)
                         'mouse-face 'tab-bar-tab-highlight)
            ;; The root is spliced in as a literal so the chip keeps working
            ;; when the bar is rebuilt from a cached keymap.
            (lambda () (interactive) (my-tab-bar-select-project ,root))
            :help ,(format "Switch to project %s" dir)))))
     (sort (project-known-project-roots) #'string<))))

;; A tab that wanders into another project becomes that project's tab: the
;; group follows the project of the buffer you land in, and tabs of the same
;; project stay adjacent (`tab-bar-tab-post-change-group-functions').
(defun my-tab-bar-group-by-project (&rest _)
  "Put the current tab into the tab group of its project.
Tabs outside a project (*scratch*, say) keep whatever group they had."
  (when-let* ((tab (assq 'current-tab (funcall tab-bar-tabs-function)))
              (proj (project-current)))
    (let ((root (my-tab-bar-project-directory (project-root proj))))
      (unless (equal (alist-get 'group tab) root)
        (tab-bar-change-tab-group root)))))

(add-hook 'tab-bar-tab-post-select-functions #'my-tab-bar-group-by-project)
(add-hook 'tab-bar-tab-post-open-functions #'my-tab-bar-group-by-project)

;; The tab that exists at startup is never opened or selected, so group it
;; here; that way its chip selects it rather than creating a second tab.
(my-tab-bar-group-by-project)

(setq tab-bar-format '(tab-bar-format-history
                       my-tab-bar-format-projects
                       tab-bar-separator
                       tab-bar-format-add-tab))

;; Tabs belonging to no project (the startup `*scratch*' one, say) get no
;; chip; the `C-x t' commands still reach them, and `C-x t 0' closes one.

;; Tab line -- a tab per buffer at the top of each window. The buffer-local
;; mode is `tab-line-mode'; this is the global entry point.
(global-tab-line-mode 1)

;; ----------------------------------------------------------------------
;; terminal: ghostel
;; ----------------------------------------------------------------------

;; Ghostel -- terminal emulator on libghostty-vt. The native module was
;; downloaded already; `ghostel-module-auto-install' stays at its default
;; 'ask, so a package upgrade asks before fetching a new one.
(use-package ghostel
  :ensure t
  :bind (("C-c g" . ghostel)))

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
  :hook (dired-mode . auto-revert-mode))

;; ----------------------------------------------------------------------
;; icons in dired and ibuffer
;; ----------------------------------------------------------------------

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

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
;; treemacs -- file tree in a side window
;; ----------------------------------------------------------------------

;; `C-x t' is the tab-bar prefix in Emacs 27+, so the tree gets `C-c T'.
;; Inside the tree: TAB fold, `?' help, `g' refresh, `q' close.
(use-package treemacs
  :ensure t
  :bind (("C-c T" . treemacs))
  :init
  (setq treemacs-follow-mode t
        treemacs-filewatch-mode t
        treemacs-fringe-indicator-mode 'always)
  :config
  (treemacs-git-mode 'deferred))         ; async git status, cheaper

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
;; in this file are `C-c t' (theme), `C-c T' (treemacs) and `C-c g' (ghostel).
(use-package org
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)))

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
;; must come after the package is loaded, hence `:init'.
(use-package pilish
  :ensure t
  :init (defalias 'pi 'pilish))

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
   '(## diminish git vertico consult corfu marginalia embark embark-consult
     magit with-editor cape diff-hl doom-modeline easysession ef-themes ghostel
     helpful indent-bars markdown-mode markdown-toc nerd-icons nerd-icons-corfu
     nerd-icons-dired nerd-icons-ibuffer org-appear org-modern outline-indent
     pilish rainbow-delimiters treemacs doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
