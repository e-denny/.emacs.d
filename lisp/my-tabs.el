;;; my-tabs.el --- Project chips in the tab bar, buffers in the tab line  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Edgar

;; Author: Edgar
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, convenience

;;; Commentary:

;; Two tab widgets sit at the top of the frame, and this file dresses both
;; after Amit Patel's "Emacs: prettier tab-line" (amitp.blogspot.com,
;; 2020-06).  Neither of them is `powerline': the wave-shaped tab edges are
;; Nerd Font glyphs (the powerline block of U+E0B0..U+E0B7, which a Nerd
;; Font supplies and `doom-modeline' already requires), so nothing has to be
;; added to the package list and the edges stay crisp at any font size.
;;
;; The two bars hold different things, so they are styled as one system
;; rather than as one widget:
;;
;;   tab bar   (frame-wide)   one chip per project, over `C-x t'
;;   tab line  (per window)   the buffers of the current project
;;
;; ---------------------------------------------------------------- tab bar
;;
;; A tab bar tab holds a window configuration, so one tab per project is
;; what keeps each project's layout -- but `tab-bar.el' never consults
;; `project.el', and its own chips are named after buffers, which the tab
;; line above each window already lists.  So `tab-bar-format-tabs' is
;; dropped from `tab-bar-format' and one chip per known project is drawn
;; instead: the same list `C-x p p' prompts with
;; (`project-known-project-roots').  Clicking a chip selects that project's
;; tab, creating it the first time.  A tab's `group' entry records which
;; project the tab belongs to: it holds the project root, which is what
;; identifies a project, while the chip shows that root abbreviated under
;; $HOME, the way `C-x p p' spells it.
;;
;; --------------------------------------------------------------- tab line
;;
;; The tab line is the buffer switcher, and it follows the blog's shape: the
;; tabs are the buffers of the current project, sorted alphabetically
;; (`tab-line-tabs-buffer-group-sort-function'), with no new-tab.  Each tab
;; starts with its buffer's file type icon (`nerd-icons') and ends with a
;; cross that closes it, which is `tab-line-close-button' redrawn as a Nerd
;; Font glyph so it sits in the tab's own color instead of as an SVG image.
;; `tab-line-tabs-buffer-groups' would group by project too,
;; but since Emacs 30 it leads the group with a tab that names the project
;; and expands into a list of groups; the tab bar above already has one chip
;; per project, so that group tab would only repeat it above every window.
;; `my-tabs-tab-line-buffers' returns the buffers of the current group
;; directly, which is what the blog's Emacs 27 did.
;;
;; ----------------------------------------------------------------- color
;;
;; The blog hard-codes a light gray strip with a purple `#b34cb3' selected
;; tab.  Here the bars take their colors from the active theme instead, so
;; `C-c t' keeps flipping dark and light.  Both bars mark every chip with a
;; vertical bar at its left edge: blue in the theme's accent color for the
;; selected chip, grey for the others.  The selected chip's background is
;; pulled toward that blue (`my-tabs-active-tint'), because the theme's own
;; selected background sits too close to the bar's to be told apart; its
;; left edge is straight, so the blue bar runs unbroken from the chip's top
;; to its bottom, while its right edge keeps the rounded cap.  An inactive
;; chip keeps the bar's background and carries only the grey bar, so the
;; names stay apart without a chip-shaped slab per buffer.  Set
;; `my-tabs-edge-style' to `slanted' for a powerline triangle as the right
;; cap, or to `flat' for a square one.

;;; Code:

(require 'cl-lib)
(require 'nerd-icons)
(require 'project)
(require 'seq)
(require 'tab-bar)
(require 'tab-line)

(defun my-tabs--redraw (&rest _)
  "Rebuild both bars from scratch.
The tab line caches its formatted strings per window, and a chip's
background and accent bar are computed while those strings are made, so
both caches have to go when the theme or `my-tabs-active-tint' changes."
  (tab-line-force-update t)
  (force-mode-line-update t))

;; ----------------------------------------------------------------------
;; edges
;; ----------------------------------------------------------------------

(defcustom my-tabs-edge-style 'rounded
  "Shape of the right edge of a tab.
`rounded' is a half circle, the closest a glyph gets to the wave
`powerline-wave-right' draws in the blog post this file follows;
`slanted' is the powerline triangle; `flat' draws no cap at all.
The left edge is always straight: it is where the accent bar that
`my-tabs--pill' puts in front of a chip meets the chip's background."
  :type '(choice (const :tag "Rounded (half circle)" rounded)
                 (const :tag "Slanted (powerline triangle)" slanted)
                 (const :tag "Flat (no edge glyph)" flat))
  :group 'tab-line
  :version "31.1")

(defcustom my-tabs-active-tint 0.22
  "How far the selected tab's background is pulled toward the accent.
The themes leave the selected tab barely lighter than the bar, which is
hard to see.  Blending its background this far toward the accent color
makes the chip stand out; 0 keeps the theme's own color, 1 makes it the
accent color outright.  The blue bar at the chip's left edge is drawn
in the accent color whether or not the background is tinted."
  :type 'number
  :set (lambda (symbol value)
         (set-default symbol value)
         (my-tabs--redraw))
  :group 'tab-line
  :version "31.1")

(defconst my-tabs--caps
  `((rounded . ,(string #xe0b4))   ; fills its left half, rounds its right
    (slanted . ,(string #xe0b0))
    (flat . ""))
  "Glyph ending a tab whose background it is drawn in, by style.
The powerline block of the Nerd Font supplies it: U+E0B4 fills its left
half and rounds its right, so the tab's color flows into the bar.")

(defconst my-tabs--accent-fallback "#51afef"
  "Blue used for the selected chip's bar when the theme names none.")

(defun my-tabs--bar-width (selected)
  "Width in pixels of the bar in front of a chip.
SELECTED asks for the bar on the chip whose window or project is
current; it is `doom-modeline-bar-width' when doom-modeline is
installed, so it is as wide as the one that starts the mode line, and
four pixels otherwise.  The bars on the other chips are half that, two
pixels: at the accent's width they read as loudly as the accent itself,
and they are only there to hold the chips apart."
  (let ((width (if (and (boundp 'doom-modeline-bar-width)
                        (integerp doom-modeline-bar-width)
                        (> doom-modeline-bar-width 0))
                   doom-modeline-bar-width
                 4)))
    (if selected width (max 1 (/ width 2)))))

(defun my-tabs--color (attribute face)
  "Value of ATTRIBUTE in FACE, or nil when it is unspecified.
FACE is a face name or a face spec, and `:inherit' is followed, so a
tab wearing `(:inherit (tab-line-tab-special tab-line-tab-inactive))'
reports the background of whichever parent has one."
  (cond
   ((null face) nil)
   ((symbolp face)
    (let ((value (ignore-errors (face-attribute face attribute nil t))))
      (unless (eq value 'unspecified) value)))
   ((consp face)
    (let ((value (plist-get face attribute)))
      (cond
       ((and value (not (eq value 'unspecified))) value)
       ((plist-get face :inherit)
        (seq-some (lambda (parent) (my-tabs--color attribute parent))
                  (let ((parents (plist-get face :inherit)))
                    (if (listp parents) parents (list parents))))))))))

(defun my-tabs--first (property string)
  "First non-nil PROPERTY among the characters of STRING, or nil."
  (let ((pos 0) value)
    (while (and (null value) (< pos (length string)))
      (setq value (get-text-property pos property string))
      (setq pos (1+ pos)))
    value))

(defun my-tabs--blend (color other weight)
  "Move COLOR toward OTHER by WEIGHT, a number between 0 and 1.
Returns COLOR unchanged when either one is not a color the display
knows, so a theme that leaves an attribute unspecified still renders."
  (let ((from (and (stringp color) (color-name-to-rgb color)))
        (to (and (stringp other) (color-name-to-rgb other))))
    (if (or (null from) (null to))
        color
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (a b)
                                  (+ (* (- 1 weight) a) (* weight b)))
                                from to)
                     '(2))))))

(defun my-tabs--accent ()
  "The theme's accent color, used for the selected chip's bar.
Themes give `link' the color they use for emphasis, which is a blue in
the ones this config ships with; fall back to a plain blue when a theme
leaves it unspecified."
  (or (my-tabs--color :foreground 'link)
      (my-tabs--color :foreground 'minibuffer-prompt)
      my-tabs--accent-fallback))

(defun my-tabs--scale ()
  "Scale that makes a chip's bar and cap as tall as the mode line.
The mode line is taller than the frame's font, and both bars are made
to match it.  The text keeps the font's size, and a face's background
fills whatever height the row has, so only the two cells whose glyphs
have to span the row -- the accent bar and the cap -- are scaled.
The height is read off any window of the frame that has a mode line,
because a window without one (the pi sidecar, say) reports zero."
  (let* ((char (frame-char-height))
         (height (and (fboundp 'window-mode-line-height)
                      (seq-some (lambda (window)
                                  (let ((line (window-mode-line-height window)))
                                    (and line (> line char) line)))
                                (window-list nil t)))))
    (if (and char (> char 0) height)
        (/ (float height) char)
      1.0)))

(defun my-tabs--accent-face (selected)
  "Face for the bar in front of a chip.
SELECTED picks the accent color for the bar and grey for the others.
The bar is a space as wide as `my-tabs--bar-width' suggests, filled
with this face's background and scaled by `my-tabs--scale' so its color
runs from the top of the row to the bottom."
  (let ((color (if selected
                   (my-tabs--accent)
                 (or (my-tabs--color :foreground 'tab-bar-tab-inactive)
                     (my-tabs--color :foreground 'tab-line-tab-inactive)
                     "grey50"))))
    (list :foreground color
          :background color
          :height (my-tabs--scale))))

(defun my-tabs--pill (text face bar-face)
  "Put TEXT in a chip: an accent bar, a space, the text, a pad and a cap.
TEXT should already carry FACE itself; the bar, the space after it, the
pad and the cap are propertized here.  The bar's cell is drawn in
BAR-FACE's background so the bar looks like it closes the gap before the
chip; the chip's own background is FACE's, pulled toward the accent by
`my-tabs-active-tint' when the theme makes it differ from the bar --
which is how a selected chip is told apart from an inactive one.  An
inactive chip, whose background is the bar's, gets only the grey bar and
the padding, and so runs into the bar the way it always did."
  (let* ((background (my-tabs--color :background face))
         (bar (my-tabs--color :background bar-face))
         (selected (and background bar (not (equal background bar))))
         (surface (if selected
                      (my-tabs--blend background (my-tabs--accent)
                                      my-tabs-active-tint)
                    background))
         ;; A space of its own between the bar and the name or the icon.
         (body (concat (propertize " " 'face face)
                       text
                       (propertize " " 'face face)))
         ;; The bar is a space stretched to a fixed width and painted in
         ;; its face's background: a block glyph would be a fraction of a
         ;; character wide, and no fractions wider than an eighth fill
         ;; from the right.
         (bar-cell (propertize " "
                               'display (list 'space :width
                                              (list (my-tabs--bar-width
                                                     selected)))
                               'face (my-tabs--accent-face selected)))
         (cap (cdr (assq my-tabs-edge-style my-tabs--caps))))
    (when (and selected (not (equal surface background)))
      ;; Prepend, because the first face to set an attribute wins and the
      ;; text already carries FACE, whose background is the theme's.
      (add-face-text-property 0 (length body)
                              (list :background surface) nil body))
    ;; The bar is part of the chip, so hovering it should light up the chip.
    (when-let* ((mouse (my-tabs--first 'mouse-face text)))
      (put-text-property 0 1 'mouse-face mouse bar-cell))
    (concat bar-cell
            body
            (when (and selected cap (not (equal cap "")))
              (propertize cap 'face (list :foreground (or surface background)
                                          :background (or bar background)
                                          :height (my-tabs--scale)))))))

;; ----------------------------------------------------------------------
;; tab bar: a project switcher
;; ----------------------------------------------------------------------

;; `tab-bar-show' defaults to t here (Emacs 31), so the bar is always
;; visible; set it to 1 to show it only once a second tab exists, or nil to
;; hide it while keeping the `C-x t' commands.

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

(defun my-tab-bar-rename-tab-if-anonymous (&optional tab)
  "Name TAB explicitly after the name it already shows.
`tab-bar-tab-name-function' recomputes the name of every tab that was never
named by hand, so a tab with no `explicit-name' is renamed as soon as the
name function changes value -- including while a new tab is being made with
it bound.  Pinning the tab being left keeps the chip it was already showing."
  (let ((tab (or tab (assq 'current-tab (funcall tab-bar-tabs-function)))))
    (when (and tab (null (alist-get 'explicit-name tab)))
      (tab-bar-rename-tab (alist-get 'name tab)))))

(defun my-tab-bar-select-project (root)
  "Switch to the tab for project ROOT, creating that tab if necessary.
The new tab shows the project's most recent buffer, or Dired at ROOT when
the project has no buffer yet.  A remembered project whose directory has
since disappeared says so instead of half-creating a tab.

What the tab shows, how it is named and which group it belongs to are all
put in place *before* the tab exists, through the bindings `tab-bar' offers
for exactly that (`tab-bar-new-tab-choice', `tab-bar-tab-name-function',
`tab-bar-new-tab-group'), rather than switching its buffer and renaming it
afterwards.  Anything that reads the new tab while it is being made -- the
bar's own chip, a treemacs workspace keyed by tab name -- then sees the
project itself, not the tab this one was split off from."
  (if-let* ((index (my-tab-bar-project-tab-index root)))
      (tab-bar-select-tab (1+ index))
    (let* ((proj (ignore-errors (project-current nil root)))
           (buf  (and proj (car (project-buffers proj))))
           (name (my-tab-bar-project-name root)))
      (unless (or (buffer-live-p buf) (file-directory-p root))
        (user-error "Project directory missing: %s" root))
      (my-tab-bar-rename-tab-if-anonymous)
      (let ((tab-bar-new-tab-choice
             ;; Called by `tab-bar-new-tab-to' before it names the tab; the
             ;; Dired listing is made without being shown, so that the tab is
             ;; created on it rather than the old tab being repurposed.
             (lambda () (or (and (buffer-live-p buf) buf)
                            (dired-noselect root))))
            (tab-bar-tab-name-function (lambda () name))
            (tab-bar-new-tab-group (my-tab-bar-project-directory root)))
        (tab-bar-new-tab))
      ;; Tie the name down: a tab without an explicit name has its chip
      ;; recomputed from whatever buffer it happens to be showing.
      (tab-bar-rename-tab name))))

(defun my-tab-bar-visit-project ()
  "Make or select the tab of the project being switched to.
This is the command `project-switch-project' runs for `C-x p p': that
command has already read a project, remembered it, and left it in
`project-current-directory-override', so all that is left is the tab bar's
own answer, which is also what the chips do -- select the project's tab,
creating it the first time.  Set as a single symbol in
`project-switch-commands' it replaces the dispatch menu, and with it the
second prompt that used to ask what to do inside a project that already
has a tab."
  (interactive)
  (my-tab-bar-select-project (project-root (project-current t))))

(defun my-tab-bar-format-projects ()
  "Produce one clickable chip per known project, dressed as a pill.
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
              (face (if (equal dir current-dir)
                        'tab-bar-tab 'tab-bar-tab-inactive))
              (label (propertize (my-tab-bar-project-name root)
                                 'face face
                                 'mouse-face 'tab-bar-tab-highlight)))
         `((,(intern (format "project-%d" i)) menu-item
            ,(my-tabs--pill label face 'tab-bar)
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

(tab-bar-mode 1)

;; ----------------------------------------------------------------------
;; tab line: the current project's buffers
;; ----------------------------------------------------------------------

(defun my-tabs-buffer-group (buffer)
  "The group BUFFER belongs to: its project root, or nil for no project.
Buffers outside any project therefore group together the way they did
in the blog's Emacs 27."
  (with-current-buffer buffer
    (when-let* ((project (ignore-errors (project-current))))
      (my-tab-bar-project-directory (project-root project)))))

(defun my-tabs-tab-line-buffers ()
  "The buffers of the current buffer's group, alphabetically.
This is `tab-line-tabs-buffer-groups' minus its group tab: same list,
same `tab-line-tabs-buffer-group-sort-function', but the project's name
is not repeated above every window when the tab bar already names it."
  (let* ((group (my-tabs-buffer-group (current-buffer)))
         (buffers (seq-filter (lambda (buffer)
                                (equal group (my-tabs-buffer-group buffer)))
                              (funcall tab-line-tabs-buffer-list-function))))
    (if (functionp tab-line-tabs-buffer-group-sort-function)
        (seq-sort tab-line-tabs-buffer-group-sort-function buffers)
      buffers)))

(defun my-tabs-buffer-icon (buffer face)
  "Icon naming BUFFER's file type, followed by a space, or nil.
A Dired buffer gets a folder, a file buffer the icon for its extension,
anything else the icon for its major mode.  FACE, the tab's own face, is
appended under the glyph so the pill's background runs beneath it.  It
has to come after the glyph's own face, because the first value given for
a face attribute in a face list wins, and the glyph's color is the one to
keep: prepending the tab's face would paint every icon in the tab's
foreground."
  (when (bufferp buffer)
    (let ((icon (ignore-errors
                  (with-current-buffer buffer
                    (cond ((and (derived-mode-p 'dired-mode)
                                (bound-and-true-p dired-directory))
                           (nerd-icons-icon-for-dir dired-directory))
                          (buffer-file-name
                           (nerd-icons-icon-for-file buffer-file-name))
                          (t (nerd-icons-icon-for-mode major-mode)))))))
      (when (and icon (not (equal icon "")))
        ;; Copied first: `nerd-icons' can hand back a string it keeps (see
        ;; `nerd-icons-cache'), and `add-face-text-property' mutates its
        ;; argument, so the tab's face would pile up on that shared glyph.
        (setq icon (copy-sequence icon))
        (add-face-text-property 0 (length icon) face t icon)
        ;; The space is propertized too: a bare one would show the frame's
        ;; background between the icon and the name, a notch in the pill.
        (concat icon (propertize " " 'face face))))))

(defun my-tabs-tab-line-name-format (tab tabs)
  "Format TAB as a pill: its file type icon, name and close button.
The default formatter does the real work -- the name, the close button,
the keymap, the face `tab-line-tab-face-functions' settles on -- and its
result is then wrapped in the edge glyphs, with the buffer's icon put in
front of the name."
  (let* ((text (tab-line-tab-name-format-default tab tabs))
         ;; Read the face off the name, before the icon's own face is in
         ;; front of it: the icon has no background of its own.
         (face (get-text-property 0 'face text)))
    (my-tabs--pill (concat (my-tabs-buffer-icon tab face) text)
                   face 'tab-line)))

;; The blog's close cross is `tab-line-close-button', drawn as an SVG image
;; on a graphical frame; redraw it as the Material Design "x", which the
;; Nerd Font already installed for `doom-modeline' supplies.  The default
;; formatter adds the tab's face over the whole button afterwards, so the
;; glyph ends up in the tab's own foreground and the space in front of it in
;; the tab's background, rather than as a grey image that ignores the theme.
;; The glyph is drawn at 70% of its usual size: at full size the cross is
;; as tall as the buffer icon and crowds the name it sits next to.  It is
;; also raised, because a glyph smaller than the font sits on the same
;; baseline as the text and so ends up below the middle of a row that is
;; taller than the text; `nerd-icons-mdicon' turns `:v-adjust' into the
;; `raise' property, and 0.3 puts the cross in the middle of the row.
;; The height goes through `nerd-icons-mdicon' rather than a face of its
;; own, so that the family face that finds the glyph stays on the string;
;; a second face would replace it and the cross would come out as tofu.
(setq tab-line-close-button
      (propertize (concat " " (nerd-icons-mdicon "nf-md-close"
                                                  :height 0.7 :v-adjust 0.3))
                  'rear-nonsticky nil ; important to not break auto-scroll
                  'keymap tab-line-tab-close-map
                  'mouse-face 'tab-line-close-highlight
                  'help-echo "Click to close tab"))

;; `ghostel' terminals are the one buffer in this config that `nerd-icons'
;; has no icon for, so they would all wear the Emacs logo that stands in for
;; `fundamental-mode'.  Give them the terminal glyph the other shells get.
(add-to-list 'nerd-icons-mode-icon-alist
             '(ghostel-mode nerd-icons-devicon "nf-dev-terminal"
               :face nerd-icons-lgreen))

(setq tab-line-tabs-function #'my-tabs-tab-line-buffers
      tab-line-tab-name-format-function #'my-tabs-tab-line-name-format
      ;; The blog's switch: no plus-shaped button.  The close cross stays,
      ;; on every tab; set this to `selected' to keep it only on the tab
      ;; the window is showing.
      tab-line-new-button-show nil
      tab-line-close-button-show t
      ;; The pills butt against each other; `my-tabs--pill' supplies the
      ;; padding, so the tab line's own separator would only add a gap.
      tab-line-separator "")

;; `tab-line-mode' is the buffer-local mode; this is the global entry point.
(global-tab-line-mode 1)

;; ----------------------------------------------------------------------
;; following the theme
;; ----------------------------------------------------------------------

(defun my-tabs--flatten (&rest _)
  "Drop the box around both bars and redraw them in the theme's colors.
Called on load and after every `load-theme', since a theme that draws a
box around its tabs (or changes their backgrounds) would otherwise leave
the pills in the previous theme's colors."
  (dolist (face '(tab-bar tab-bar-tab tab-bar-tab-inactive
                  tab-line tab-line-tab tab-line-tab-current
                  tab-line-tab-inactive))
    (set-face-attribute face nil :box nil))
  (my-tabs--redraw))

(add-hook 'enable-theme-functions #'my-tabs--flatten)
(add-hook 'disable-theme-functions #'my-tabs--flatten)
(my-tabs--flatten)

(provide 'my-tabs)
;;; my-tabs.el ends here
