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
;; where it is, `m' marks, `C' copies, `g' reads the directory again and `q'
;; closes the window.  What makes it a tree is `dired-subtree' (bound to TAB
;; over all Dired buffers in init.el): TAB on a directory inserts the
;; directory's own listing indented below its line, TAB again takes it away,
;; and `dired-subtree-cycle' opens a level per TAB.  That is what the tree is
;; here -- no package of its own, no separate keymap, no second idea of a
;; workspace to keep in step with `project.el'.
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
;; Following the workspace is the tab bar's own job: `--show' runs from
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
    (use-local-map map)))

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

The window is not dedicated, unlike the pi sidecar's.  A tab restores the
sidecar window it saved, listing and all, and `set-window-buffer' on a
dedicated window refuses a different buffer -- \"Window is dedicated to
...\", out of the tab switch itself -- while a plain one takes the
listing the restore brought and has it corrected afterwards."
  (let ((window (display-buffer
                 buffer
                 `((display-buffer-in-side-window)
                   (side . left)
                   (slot . 0)
                   (window-width . ,(dired-sidecar--requested-width))
                   ,@(when-let* ((height (dired-sidecar--requested-height)))
                       `((window-height . ,height)))))))
    (when (window-live-p window)
      (set-window-parameter window 'no-delete-other-windows t))
    window))

(defun dired-sidecar--window (&optional frame)
  "Return FRAME's dired sidecar window, or nil."
  (seq-find (lambda (window)
              (buffer-local-value 'dired-sidecar-root (window-buffer window)))
            (window-list frame 'nomini)))

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

(defun dired-sidecar--show (&rest _)
  "Show the listing of the current workspace's project root.
A sidecar window is reused -- the one the tab restored, with its size --
and its buffer replaced when the tab brought another project's listing
along."
  (when-let* ((root (dired-sidecar--workspace-root))
              (buffer (dired-sidecar--buffer root)))
    (let ((window (dired-sidecar--window)))
      (cond
       ((and window (eq (window-buffer window) buffer)) nil)
       (window (set-window-buffer window buffer))
       (t (dired-sidecar--display buffer))))))

(defun dired-sidecar-toggle ()
  "Show the directory sidecar, or hide it.
Hiding deletes the window only: the buffer keeps its directory and the
subtrees opened in it, so toggling back on returns to the listing as it
was left."
  (interactive)
  (if-let* ((window (dired-sidecar--window)))
      (delete-window window)
    (dired-sidecar--show)))

(add-hook 'tab-bar-tab-post-select-functions #'dired-sidecar--show)
(add-hook 'tab-bar-tab-post-open-functions #'dired-sidecar--show)
(add-hook 'tab-bar-tab-post-change-group-functions #'dired-sidecar--show)

;; A session brings its tabs back with the windows they were saved with, and
;; `easysession' restores at depth 102; the listing is put in its place after
;; that, since the saved window may predate this package or hold another
;; project's listing.
(add-hook 'emacs-startup-hook #'dired-sidecar--show 110)

(provide 'dired-sidecar)
;;; dired-sidecar.el ends here
