;;; dired-sidecar.el --- The workspace's directory listing in the left column  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Edgar

;; Author: Edgar
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (dired-subtree "0.0.1"))
;; Keywords: files, convenience

;;; Commentary:

;; A Dired buffer that follows the workspace: the tab bar's tab is the
;; workspace, and each one keeps a listing of its own, opened at that
;; workspace's project root.  It sits in the top left corner of the frame -- a
;; left side window, slot 0 -- takes the width of the column the pi sidecar
;; shares with it, since side windows on one side are all as wide as the side
;; area is, and takes the height that is left above the pi sidecar, which
;; keeps its own.
;;
;; The listing is an ordinary Dired buffer, so every Dired command works in
;; it: RET and the mouse open a file in the main window and leave the listing
;; where it is, `m' marks, `C' copies, `g' reads the directory again, and `q'
;; hides the sidecar in every tab (`C-c T' brings it back).  What makes it a
;; tree is `dired-subtree', bound to TAB over all Dired buffers in init.el:
;; TAB on a directory inserts the directory's own listing indented below its
;; line, TAB again takes it away, and `dired-subtree-cycle' opens a level per
;; TAB.  That is what the tree is here -- no package of its own, no separate
;; keymap, no second idea of a workspace to keep in step with `project.el'.
;;
;; Each entry also carries the state it has in the repository the listing
;; belongs to: a bar in front of the line, drawn the way the tab bar draws the
;; bar in front of a chip -- a space stretched to a few pixels and painted in
;; the state's colour -- green for a new file, yellow for a modified one, red
;; for a deleted one or for a conflict, purple for a rename.  A directory
;; takes the strongest state of anything below it, so a closed directory
;; still says that something in it changed.  A bar and not a colour on the
;; name, because Dired already speaks through its text: marks, the `D' flag
;; and the highlight are all name-level colouring, and a second meaning laid
;; over them would be read against them.
;;
;; One buffer per project root, not one per tab: a listing belongs to a
;; directory, so two tabs of the same project share one listing, with the
;; subtrees opened in it, and each project's listing is still there, expanded
;; as it was left, when its tab comes round again.  The buffer is named
;; " *dired-sidecar /path*" -- the leading space keeps the tab line, which
;; skips space-prefixed buffers, from listing it above every window -- and
;; the buffer-local `dired-sidecar-root' marks it as one of ours, so the
;; package knows which window to fill and which listings are none of its
;; business.
;;
;; Following the workspace is the tab bar's own job: `--follow-tab' runs from
;; `tab-bar-tab-post-select-functions' and `-post-open-functions', draws the
;; listing of whichever project the tab has landed in, and replaces the
;; buffer of whatever sidecar window the tab restored.  A tab made by
;; `tab-bar-new-tab' copies the layout of the tab it was made from, so
;; without that the new workspace would open on the previous project's
;; listing.  `tab-bar-tab-post-change-group-functions' covers the other way a
;; workspace moves: the my-tabs hook that puts a tab into the group of the
;; project it has wandered into.
;;
;; Setup, in init.el, next to the other companion packages:
;;
;;   (require 'dired-sidecar)
;;   (global-set-key (kbd "C-c T") #'dired-sidecar-toggle)
;;
;; `dired-sidecar-toggle' shows the sidecar or hides it; hiding deletes the
;; window only, so the buffer comes back with its directory and its open
;; subtrees intact.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'project)
(require 'seq)
(require 'subr-x)

(defgroup dired-sidecar nil
  "The workspace's directory listing in a left side window."
  :group 'tools
  :prefix "dired-sidecar-")

(defcustom dired-sidecar-width nil
  "Width to ask for when this listing creates the left side column.
When nil, follow `pi-sidecar-width' (35 columns by default), so both
sidecars ask for the same column.  Once a side window exists the side
area's width is what counts -- this listing and the pi sidecar share it --
and the windows can be resized together."
  :type '(choice (const :tag "Follow the pi sidecar" nil)
                 (integer :tag "Columns"))
  :group 'dired-sidecar)

(defcustom dired-sidecar-hide-details t
  "Whether the listing starts with `dired-hide-details-mode' on.
The window is about 35 columns wide, where the permissions, owner and
date columns would leave a handful of characters for the names.  `('
brings them back."
  :type 'boolean
  :group 'dired-sidecar)

(defvar-local dired-sidecar-root nil
  "Project root this Dired listing belongs to.
Nil in every buffer this package does not own.")

(defvar-local dired-sidecar--ready nil
  "Non-nil once the current sidecar buffer has its keys and looks.")

(defun dired-sidecar--directory (root)
  "ROOT as an absolute directory name without its trailing slash."
  (directory-file-name (expand-file-name root)))

(defun dired-sidecar--buffer-name (directory)
  "Hidden buffer name for the sidecar listing of DIRECTORY."
  (format " *dired-sidecar %s*" (abbreviate-file-name directory)))

(defun dired-sidecar--listing-of-p (directory)
  "Whether the current buffer already lists DIRECTORY as a sidecar.
A listing restored with the session comes back as a Dired buffer of that
directory under this package's name, but without its marker, so the
directory it lists stands in for the marker once."
  (or (equal dired-sidecar-root directory)
      (and (null dired-sidecar-root)
           (equal dired-directory (file-name-as-directory directory)))))

(defun dired-sidecar--read-in (directory)
  "Read DIRECTORY into the current buffer as a Dired listing.
This is the new-buffer path of `dired-internal-noselect' spelled out:
Dired only reads a directory in whole when its name ends in a slash, and
`dired-mode' alone does not read it at all."
  (setq default-directory (file-name-as-directory directory))
  ;; `dired-mode' runs `dired-mode-hook' before anything is read, and it does
  ;; so with `dired-subdir-alist' nil while the buffer still holds whatever
  ;; listing it had.  A hint that walks that listing then -- `nerd-icons-dired'
  ;; asks `dired-get-filename' for each line -- fails on the missing alist and
  ;; takes the read-in down with it, leaving the buffer half set up.  That is
  ;; the state a session restores the sidecar in, so the old listing goes
  ;; first; Dired erases the buffer itself, so nothing is lost by it.
  (let ((inhibit-read-only t))
    (erase-buffer))
  (dired-mode (file-name-as-directory directory)
              (connection-local-value dired-listing-switches))
  (dired-readin))

(defun dired-sidecar--setup-buffer ()
  "Give the current sidecar buffer its own keys and looks.
Only what a narrow side window needs: the tab line off, since the listing
is not a buffer to switch to; file names only; no wrapping; and RET and
`f' opening in the main window, so that walking the tree never replaces
the tree itself."
  (setq-local tab-line-exclude t)
  (setq-local tab-line-format nil)
  (tab-line-mode -1)
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (when dired-sidecar-hide-details
    (dired-hide-details-mode 1))
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (define-key map (kbd "RET") #'dired-find-file-other-window)
    (define-key map (kbd "f") #'dired-find-file-other-window)
    ;; `quit-window' would bury the listing and have it come back on the next
    ;; tab switch; hiding it puts it away everywhere instead, as `C-c T' does.
    (define-key map (kbd "q") #'dired-sidecar-toggle)
    (use-local-map map))
  ;; The repository's state is read again whenever Dired re-reads the
  ;; directory, and whenever a subtree opens or closes -- both of which erase
  ;; or insert the text the bars were attached to.
  (add-hook 'dired-after-readin-hook #'dired-sidecar--git-annotate nil t)
  (add-hook 'dired-subtree-after-insert-hook #'dired-sidecar--git-annotate nil t)
  (add-hook 'dired-subtree-after-remove-hook #'dired-sidecar--git-annotate nil t)
  (dired-sidecar--git-annotate))

(defun dired-sidecar--buffer (root)
  "Return the sidecar listing of project ROOT, making it if needed."
  (let* ((directory (dired-sidecar--directory root))
         (buffer (get-buffer-create (dired-sidecar--buffer-name directory))))
    (with-current-buffer buffer
      (unless (and (derived-mode-p 'dired-mode)
                   (dired-sidecar--listing-of-p directory))
        (dired-sidecar--read-in directory))
      (setq-local dired-sidecar-root directory)
      (unless dired-sidecar--ready
        (dired-sidecar--setup-buffer)
        (setq-local dired-sidecar--ready t)))
    buffer))

;;;; git state

;; The states come from `git status --porcelain', read with `-z' so that a
;; name with a newline in it cannot break the parse, and with
;; `--untracked-files=all', which lists every file of an untracked directory:
;; the listing is a browser, so a directory that opens to show a tree of new
;; files should mark them all, and the directory itself still gets one bar
;; from the walk up.  Paths are matched as absolute names without their
;; trailing slash, the form Dired hands out for a line and the form the
;; sidecar buffer keys its entries by.
;;
;; The repository is found from the listing's own directory, so a listing
;; that has been walked up above the project still finds the same repository,
;; and a directory that belongs to none simply shows no bars.  A bar's colour
;; is the state's face foreground, the way the tab bar takes its chip's accent
;; from the theme: the faces are inherited from `success', `warning', `error'
;; and `font-lock-constant-face', so `C-c t' flips them along with everything
;; else.

(defcustom dired-sidecar-git-marks t
  "Whether entries show the state they have in the repository.
The state is a bar in front of the entry: green for a file that is new to
the repository, yellow for one that changed, red for one that was deleted
or that is in conflict, and purple for one that was renamed.  A directory
shows the strongest state of anything below it, and an entry with no state
still gets the bar's column, empty, so that the names line up.

Nothing runs while no listing is on screen.  `git status' is read when a
listing is first made, when Dired reads the directory again, when a
subtree opens or closes, when a tab is selected, and otherwise no more
often than `dired-sidecar-git-refresh-interval' seconds of idleness."
  :type 'boolean
  :group 'dired-sidecar)

(defcustom dired-sidecar-git-bar-width nil
  "Width in pixels of the bar in front of an entry.
Nil means `doom-modeline-bar-width', the mode line's own bar, so the two
match, and four pixels otherwise."
  :type '(choice (const :tag "Match the mode line" nil)
                 (integer :tag "Pixels"))
  :group 'dired-sidecar)

(defcustom dired-sidecar-git-refresh-interval 5
  "Seconds of idleness between git checks of the listings on screen.
Nil leaves the check to `revert-buffer', to a subtree being opened or
closed, and to a tab being selected."
  :type '(choice (const :tag "Off" nil)
                 (number :tag "Seconds"))
  :group 'dired-sidecar)

(defface dired-sidecar-git-new
  '((t :inherit success))
  "Bar in front of an entry that is new to the repository."
  :group 'dired-sidecar)

(defface dired-sidecar-git-modified
  '((t :inherit warning))
  "Bar in front of an entry that changed since the last commit."
  :group 'dired-sidecar)

(defface dired-sidecar-git-deleted
  '((t :inherit error))
  "Bar in front of an entry whose file is gone from the working tree."
  :group 'dired-sidecar)

(defface dired-sidecar-git-conflict
  '((t :inherit error))
  "Bar in front of an entry that is unmerged."
  :group 'dired-sidecar)

(defface dired-sidecar-git-renamed
  '((t :inherit font-lock-constant-face))
  "Bar in front of an entry that was renamed."
  :group 'dired-sidecar)

(defconst dired-sidecar--git-faces
  '((new . dired-sidecar-git-new)
    (modified . dired-sidecar-git-modified)
    (deleted . dired-sidecar-git-deleted)
    (conflict . dired-sidecar-git-conflict)
    (renamed . dired-sidecar-git-renamed))
  "Face drawing the bar for each git state.")

(defconst dired-sidecar--git-order
  '((conflict . 5) (deleted . 4) (modified . 3) (renamed . 2) (new . 1))
  "How loud each state is, for a directory holding several of them.")

(defvar dired-sidecar--git-timer nil
  "Idle timer that re-checks the states of the listings on screen.")

(defun dired-sidecar--git-strength (state)
  "How loud STATE is, as a number; 0 when there is no state."
  (or (alist-get state dired-sidecar--git-order) 0))

(defun dired-sidecar--git-root (directory)
  "Return the repository root above DIRECTORY, or nil."
  (when-let* ((root (ignore-errors (locate-dominating-file directory ".git"))))
    (directory-file-name (expand-file-name root))))

(defun dired-sidecar--git-category (xy)
  "Return the state named by XY, the two letters of a git status entry."
  (cond
   ((equal xy "??") 'new)
   ((string-match-p "U" xy) 'conflict)
   ((member xy '("AA" "DD")) 'conflict)
   ((string-match-p "[RC]" xy) 'renamed)
   ((string-match-p "D" xy) 'deleted)
   ((string-match-p "[MT]" xy) 'modified)
   ((string-match-p "A" xy) 'new)))

(defun dired-sidecar--git-mark (states root path xy)
  "Record XY, the state of PATH, for PATH and every directory above it.
The walk up stops at ROOT.  A directory keeps the strongest state of the
things below it, which is what `dired-sidecar--git-order' decides."
  (let ((state (dired-sidecar--git-category xy))
        (root (directory-file-name (expand-file-name root)))
        (prefix (file-name-as-directory
                 (directory-file-name (expand-file-name root))))
        (entry (directory-file-name (expand-file-name path root))))
    (while (and state entry
                (or (equal entry root)
                    (string-prefix-p prefix (file-name-as-directory entry))))
      (when (> (dired-sidecar--git-strength state)
               (dired-sidecar--git-strength (gethash entry states)))
        (puthash entry state states))
      (setq entry (unless (equal entry root)
                    (directory-file-name (file-name-directory entry)))))))

(defun dired-sidecar--git-read (root output)
  "Return the states in OUTPUT, a `git status' listing from ROOT."
  (let ((states (make-hash-table :test #'equal))
        (records (split-string output "\0" t)))
    (while records
      (let* ((record (pop records))
             (xy (substring record 0 2))
             (path (substring record 3)))
        ;; A rename or a copy names the path it came from in the record
        ;; that follows.
        (when (memq (aref xy 0) '(?R ?C))
          (pop records))
        (dired-sidecar--git-mark states root path xy)))
    states))

(defun dired-sidecar--git-statuses (directory)
  "Return a hash of path to git state for the changes below DIRECTORY.
Directories are included, carrying the strongest state of anything below
them.  Nil when DIRECTORY is in no repository, or when git cannot be
asked."
  (when-let* ((root (dired-sidecar--git-root directory)))
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory root))
            ;; Reading the state should not take the index lock away from a
            ;; commit running in another window.
            (process-environment (cons "GIT_OPTIONAL_LOCKS=0" process-environment)))
        (when (and (ignore-errors
                     (eq 0 (call-process "git" nil t nil
                                         "status" "--porcelain=v1" "-z"
                                         "--untracked-files=all")))
                   (> (point-max) (point-min)))
          (dired-sidecar--git-read root (buffer-string)))))))

(defun dired-sidecar--git-state-at-point (states)
  "Return the git state of the file on this line, or nil.
STATES is the hash `dired-sidecar--git-statuses' returned."
  (save-excursion
    (when (dired-move-to-filename nil)
      (let* ((directory (dired-current-directory))
             (start (point))
             (end (next-single-property-change start 'dired-filename nil
                                               (line-end-position)))
             (name (buffer-substring-no-properties start end)))
        ;; A line the subdir alist does not know about -- one whose directory
        ;; has just been removed, say -- has no path to look up.
        (when (and (stringp directory) (not (member name '("." ".."))))
          (gethash (directory-file-name (expand-file-name name directory))
                   states))))))

(defun dired-sidecar--git-bar-width ()
  "Width in pixels of the bar in front of an entry."
  (or dired-sidecar-git-bar-width
      (let ((width (bound-and-true-p doom-modeline-bar-width)))
        (and (integerp width) width))
      4))

(defun dired-sidecar--git-bar (state)
  "Return the bar marking STATE, to be shown in front of a line.
With no STATE the bar is the empty share of the same column, so that an
entry without a state still lines up with the ones that have one.  The bar
itself is a space stretched to a few pixels and painted in the state's
colour, the way the tab bar and the mode line draw their own bars."
  (let ((color (and state
                    (face-attribute (alist-get state dired-sidecar--git-faces)
                                    :foreground nil t))))
    (unless (stringp color)
      (setq color (and state (face-attribute 'default :foreground nil t))))
    (propertize " "
                'display (list 'space :width
                               (list (dired-sidecar--git-bar-width)))
                'face (and (stringp color) (list :background color)))))

(defun dired-sidecar--git-annotate-line (states)
  "Put this line's bar in place, or take it away.
STATES is the hash `dired-sidecar--git-statuses' returned, or nil when
there is nothing to mark.  A line that is an entry gets the bar or, with
no state to show, its empty share of the column, so that the names of a
listing stay in one column; a subdirectory's header line gets neither."
  (let ((bol (line-beginning-position)))
    (when (save-excursion (dired-move-to-filename nil))
      (let ((state (and states (dired-sidecar--git-state-at-point states))))
        (cond
         (state
          (put-text-property bol (1+ bol) 'before-string
                             (dired-sidecar--git-bar state))
          (put-text-property bol (1+ bol) 'dired-sidecar-git state))
         (dired-sidecar-git-marks
          (put-text-property bol (1+ bol) 'before-string
                             (dired-sidecar--git-bar nil))
          (remove-text-properties bol (1+ bol) '(dired-sidecar-git nil)))
         (t
          (remove-text-properties bol (1+ bol)
                                  '(before-string nil dired-sidecar-git nil))))))))

(defun dired-sidecar--git-annotate ()
  "Mark every entry of this listing with its state in the repository.
With `dired-sidecar-git-marks' off, the walk still runs, so that the bars
already drawn are taken away again."
  (when (derived-mode-p 'dired-mode)
    (let ((states (and dired-sidecar-git-marks
                       (dired-sidecar--git-statuses
                        (or (and (consp dired-directory)
                                 (car dired-directory))
                            dired-directory
                            default-directory))))
          (inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (dired-sidecar--git-annotate-line states)
          (forward-line 1))))))

(defun dired-sidecar--git-refresh ()
  "Re-check the states of every sidecar listing that is on screen."
  (dolist (window (window-list nil 'nomini))
    (with-current-buffer (window-buffer window)
      (when (and dired-sidecar-root (derived-mode-p 'dired-mode))
        (dired-sidecar--git-annotate)))))

(when dired-sidecar-git-refresh-interval
  (setq dired-sidecar--git-timer
        (run-with-idle-timer dired-sidecar-git-refresh-interval
                             dired-sidecar-git-refresh-interval
                             #'dired-sidecar--git-refresh)))

;; The bars take their colour from faces that inherit from the theme, but a
;; bar already drawn keeps the colour it was given, so a theme change has to
;; read the state again -- `C-c t' should recolor the bars as it recolors
;; everything else.  `enable-theme-functions' hands the function the theme it
;; has just enabled, so this takes an argument; without it every theme change
;; -- and `load-theme' does not catch it -- ends in a wrong-number-of-arguments
;; error after the theme is half applied.
(defun dired-sidecar--after-enable-theme (&rest _)
  "Re-check the bars after a theme has been enabled."
  (dired-sidecar--git-refresh))

(add-hook 'enable-theme-functions #'dired-sidecar--after-enable-theme)

;;;; Display

(defun dired-sidecar--requested-width ()
  "Width to ask for when the listing creates the left side column."
  (or dired-sidecar-width
      (let ((width (bound-and-true-p pi-sidecar-width)))
        (and (integerp width) width))
      35))

(defun dired-sidecar--pi-window (&optional frame)
  "Return FRAME's pi sidecar window, or nil."
  (seq-find (lambda (window)
              (equal (buffer-name (window-buffer window)) "*pi-sidecar*"))
            (window-list frame 'nomini)))

(defun dired-sidecar--requested-height ()
  "Lines to ask for when the listing joins an occupied left column.
A side window created into a column that already has one is handed half
of it, which would leave the listing and a mostly empty pi sidecar
looking at each other.  So when the pi sidecar is already there, ask for
the column minus the height it is meant to keep, and it shrinks to that.
Nil when this listing is the first window in the column, where it takes
the whole side area without asking."
  (when (dired-sidecar--pi-window)
    (max window-min-height
         (- (window-total-height (frame-root-window))
            (let ((height (bound-and-true-p pi-sidecar-height)))
              (cond ((integerp height) height)
                    ((floatp height) (round (* height (frame-height))))
                    (t 14)))))))

(defun dired-sidecar--display (buffer)
  "Show BUFFER at the top of the left side window column.
No height is asked for when the column is still empty: a side window
without one takes the side area it is the first into, and the pi sidecar's
own height is what it carves out below this one.

The window is dedicated to the listing it shows, which is what keeps the
pi sidecar's `display-buffer' -- or any other -- from taking the window for
its own buffer.  `--show' drops that mark when it has to put another
project's listing in it, because `set-window-buffer' refuses a dedicated
window; a tab that restores the window with the listing it saved is why
that is needed and not just a precaution.

The width is preserved for the same reason the pi sidecar's is: a session
restores windows while the frame is still at its start-up size, and the
frame's own resizing routine sizes every window proportionally, so an
unpreserved column comes back much wider than it was saved."
  (let ((window (display-buffer
                 buffer
                 `((display-buffer-in-side-window)
                   (side . left)
                   (slot . 0)
                   (window-width . ,(dired-sidecar--requested-width))
                   (preserve-size t)
                   ,@(when-let* ((height (dired-sidecar--requested-height)))
                       `((window-height . ,height)))))))
    (when (window-live-p window)
      (set-window-parameter window 'no-delete-other-windows t)
      ;; Dedicated to the listing, not to the side: this keeps
      ;; `display-buffer' -- the pi sidecar's -- from taking the window over
      ;; for its own buffer, while the package itself can still put another
      ;; project's listing in it (`--show' clears the mark to do so).
      (when (eq (window-buffer window) buffer)
        (set-window-dedicated-p window buffer)))
    window))

(defun dired-sidecar--window (&optional frame)
  "Return FRAME's dired sidecar window, or nil."
  (seq-find (lambda (window)
              (buffer-local-value 'dired-sidecar-root (window-buffer window)))
            (window-list frame 'nomini)))

(defun dired-sidecar--in-place-p (window)
  "Non-nil when WINDOW is the left column's first side window.
A tab can restore the listing as an ordinary window -- the window state it
was saved with had already lost the side parameters, because the window was
in the wrong place when the tab was left -- and a window like that keeps
its place in the main area through every tab switch until it is given up."
  (and (eq (window-parameter window 'window-side) 'left)
       (equal (window-parameter window 'window-slot) 0)))

(defun dired-sidecar--movable-p (window)
  "Non-nil when WINDOW may be removed to put a panel back in its column.
The frame's main window may not: it is the only place the workspace's own
buffer can be shown, and replacing what it displays belongs to
`my-tabs--restore-main-windows', which runs before this package's hooks."
  (and (window-parent window)
       (not (eq window (window-main-window (window-frame window))))))

;;;; Following the workspace

(defun dired-sidecar--workspace-root ()
  "Return the directory the current workspace is rooted at, or nil.
The workspace is the tab, and my-tabs records the project root of the tab
in its `group', so that is the answer: a tab keeps its project even while
it shows a terminal whose shell sits in the home directory.  Without a
group, the project of the selected window's buffer, or that buffer's own
`default-directory' when it belongs to no project."
  (let* ((tab (assq 'current-tab (funcall tab-bar-tabs-function)))
         (group (and tab (alist-get 'group tab)))
         (window (selected-window)))
    (when (window-minibuffer-p window)
      (setq window (or (minibuffer-selected-window) window)))
    (when-let* ((directory
                 (or (and (stringp group) group)
                     (when (window-live-p window)
                       (with-current-buffer (window-buffer window)
                         (or (ignore-errors
                               (project-root (project-current)))
                             default-directory))))))
      (dired-sidecar--directory directory))))

(defun dired-sidecar--preserve-width (&optional window)
  "Keep WINDOW's width when the frame is resized.
A session restores the column while the frame is still at its start-up
size, and the frame's own resizing routine (`window--resize-root-window')
then sizes every window proportionally; an unpreserved column comes back
much wider than it was saved.  Resizing the window by hand clears this."
  (let ((window (or window (dired-sidecar--window))))
    (when (window-live-p window)
      (window-preserve-size window t t))
    window))

(defun dired-sidecar--show (&rest _)
  "Show the listing of the current workspace's project root.
A sidecar window is reused -- the one the tab restored, with its size --
and its buffer replaced when the tab brought another project's listing
along."
  (when-let* ((root (dired-sidecar--workspace-root))
              (buffer (dired-sidecar--buffer root)))
    (let ((window (dired-sidecar--window)))
      (cond
       ((and window (eq (window-buffer window) buffer)
             (dired-sidecar--in-place-p window))
        (dired-sidecar--preserve-width window)
        nil)
       ((and window (not (dired-sidecar--in-place-p window))
             (dired-sidecar--movable-p window))
        ;; The listing is out of the column, in a window that can be given
        ;; up: take the window away and show the listing again where it
        ;; belongs.  The frame's main window is left alone; a listing there
        ;; is the main area's problem, not this package's.
        (delete-window window)
        (dired-sidecar--display buffer))
       (window
        ;; The window is ours, so its dedication is ours to drop: it holds
        ;; the listing that was there before, or a tab restored a window
        ;; strongly dedicated to something else in its place, and
        ;; `set-window-buffer' refuses either.
        (when (window-dedicated-p window)
          (set-window-dedicated-p window nil))
        (set-window-buffer window buffer)
        (set-window-dedicated-p window buffer))
       (t (dired-sidecar--display buffer))))
    ;; The listing that is now on screen may have been read before the
    ;; repository moved on, so its bars are re-checked here.
    (dired-sidecar--git-refresh)))

(defvar dired-sidecar--hidden nil
  "Non-nil when the listing was closed on purpose.
The tab bar's hooks keep the listing in every workspace; this is what
stops them from bringing back one that `C-c T' or `q' has just closed.
`C-c T' clears it.")

(defun dired-sidecar--follow-tab (&rest _)
  "Show the workspace's listing, or take the sidecar out when it was hidden.
A tab restores the window it was saved with -- the listing among them --
so a hidden sidecar has to be removed here and not merely left unmade."
  (if dired-sidecar--hidden
      (when-let* ((window (dired-sidecar--window)))
        (ignore-errors (delete-window window)))
    (dired-sidecar--show)))

;;;###autoload
(defun dired-sidecar-toggle ()
  "Show the directory sidecar, or hide it.
Hiding deletes the window only: the buffer keeps its directory and the
subtrees opened in it, so toggling back on returns to the listing as it
was left.  It hides in every workspace, not just this one, since the hooks
that keep it visible respect the mark until this is called again."
  (interactive)
  (if-let* ((window (dired-sidecar--window)))
      (progn
        (setq dired-sidecar--hidden t)
        (delete-window window))
    (setq dired-sidecar--hidden nil)
    (dired-sidecar--show)))

;; The listing is a sidebar and the package calls it a sidecar, so `M-x' should
;; answer to both names: the autoloaded alias is what puts the word the feature
;; is looked up by into the completion list, before this file has been read.
;;;###autoload
(defalias 'dired-sidebar-toggle #'dired-sidecar-toggle
  "Show the directory sidebar, or hide it.
An alias for `dired-sidecar-toggle'.")

(add-hook 'tab-bar-tab-post-select-functions #'dired-sidecar--follow-tab)
(add-hook 'tab-bar-tab-post-open-functions #'dired-sidecar--follow-tab)
(add-hook 'tab-bar-tab-post-change-group-functions #'dired-sidecar--follow-tab)

;; A session brings its tabs back with the windows they were saved with, and
;; `easysession' restores at depth 102; the listing is put in its place after
;; that, since the saved window may predate this package or hold another
;; project's listing.
(add-hook 'emacs-startup-hook #'dired-sidecar--follow-tab 110)

(provide 'dired-sidecar)
;;; dired-sidecar.el ends here
