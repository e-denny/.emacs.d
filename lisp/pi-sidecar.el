;;; pi-sidecar.el --- Status sidecar for pi agents in ghostel -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Edgar

;; Author: Edgar
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ghostel "0.58"))
;; Keywords: tools, terminals, convenience
;; URL: https://github.com/earendil-works/pi

;;; Commentary:

;; A sidecar listing every pi process running in a ghostel terminal of
;; this Emacs, with its state, model and working directory.  It is meant
;; to share the left side window column with the dired sidecar, sitting
;; below that listing, and it looks like the agent panel at the bottom of
;; herdr's sidebar: one row per agent, a colored dot and the model on the
;; first line, the status right-aligned, and the path on a dim second
;; line.
;;
;; Status comes from the terminal, not from pi's RPC mode.  Pilish drives
;; a headless pi in RPC mode and follows `agent_start', `agent_settled',
;; `message_end' and compaction events (plus `get_state' and its
;; `isStreaming' flag) to know whether pi is idle, streaming or sending.
;; A pi in ghostel is the interactive TUI instead, but that TUI reports
;; the same lifecycle to its terminal with OSC 7501, the program status
;; protocol: `working' during a run or compaction, `blocked' while a
;; dialog waits for the user, then `done', `error' or `idle' when the run
;; settles.  Ghostel's `ghostel-status.el' already parses those reports
;; into the buffer-local `ghostel-program-status' records, so the sidecar
;; reads the state from there.  No pi extension, RPC socket or screen
;; scraping is involved.  (The package therefore needs a ghostel build
;; that answers the OSC 7501 query, as 0.58 and later do.)
;;
;; The model is not in the status report.  Pi appends `model_change' and
;; assistant message entries with the model and provider to its session
;; JSONL, so the sidecar locates the session file for the running pi and
;; reads the last model entry from the end of it.  The file is found from
;; the process' own working directory: the session timestamp encoded in
;; the file name is matched against the process start time, and a resumed
;; session (`pi --session', `pi --continue') falls back to the newest
;; session of that directory.  The result is cached by file modification
;; time, so the 5 second refresh does not re-read unchanged sessions.
;;
;; Only pi processes that are the foreground process of a ghostel buffer
;; count.  The path is the process' cwd (from /proc), which is the
;; project pi was started in.  The model and path are read locally; pi is
;; not expected to run under tmux, herdr or TRAMP here.
;;
;; Placement: a left side window in slot 1, which puts it below the dired
;; sidecar's listing (slot 0).  Side windows on the same side share their
;; width, so the sidecar is exactly as wide as that listing; alone in the
;; column, the width follows `pi-sidecar-width' (35 columns by default).
;;
;; It is kept in every workspace: the tab bar's own hooks put it into the
;; tab being selected, so switching tabs never leaves a workspace without
;; it, and a session restores it with the windows it was saved with.  Only
;; one buffer is involved -- the agent list is the same everywhere -- but
;; each tab keeps its own side window for it, with the height the tab was
;; left with.
;;
;; Setup, in init.el, next to the other companion packages:
;;
;;   (require 'pi-sidecar)
;;   (setq pi-sidecar-switch-project-function #'my-tab-bar-select-project)
;;   (global-set-key (kbd "C-c P") #'pi-sidecar-toggle)
;;
;; `pi-sidecar-toggle' opens the sidecar and refreshes it; with it open,
;; `q' closes it.  Either one closes it in every workspace, not only the
;; one being looked at: the hooks that keep it visible respect that until
;; `C-c P' asks for it back.  In the sidecar, `n'/`p' (or the arrow keys) move
;; between agents, `s' switches the ordering, `g' refreshes now, and
;; `RET' or `mouse-1' visits the agent's ghostel buffer in its project.
;; The list refreshes every `pi-sidecar-refresh-interval' seconds, and
;; immediately after pi reports a state change or a terminal changes its
;; foreground process.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)

(require 'ghostel)

;; Optional: the mode line at the bottom of the sidecar is drawn by
;; doom-modeline, with a segment of its own.  When that package is
;; missing the sidecar simply has no mode line.
(require 'doom-modeline nil t)

(defgroup pi-sidecar nil
  "Status sidecar for pi agents running in ghostel terminals."
  :group 'tools
  :prefix "pi-sidecar-")

(defcustom pi-sidecar-refresh-interval 5
  "Seconds between automatic sidecar refreshes.
The sidecar also refreshes a moment after ghostel reports a program
status or foreground process change, so the interval only bounds how
long an unnoticed change can stay stale."
  :type 'number
  :group 'pi-sidecar)

(defcustom pi-sidecar-height 14
  "Height of the sidecar window.
An integer is a number of lines; a float is a share of the frame height.
The window is created below the dired sidecar's listing, so this is how
much of the left column the sidecar takes from it."
  :type '(choice (integer :tag "Lines")
                 (float :tag "Share of frame"))
  :group 'pi-sidecar)

(defcustom pi-sidecar-width nil
  "Width of the sidecar window when it is the first in the left column.
When nil, ask for 35 columns.  While the dired sidecar's listing is
displayed its actual window width wins, because both windows live in the
same side area and share that width."
  :type '(choice (const :tag "Default (35 columns)" nil)
                 (integer :tag "Columns"))
  :group 'pi-sidecar)

(defcustom pi-sidecar-sort 'project
  "Ordering of the agents in the sidecar.
`project' groups agents by working directory, like herdr's spaces.
`status' puts the agents that need attention first: blocked, error,
running, finished, then idle."
  :type '(choice (const :tag "By project" project)
                 (const :tag "By status" status))
  :group 'pi-sidecar)

(defcustom pi-sidecar-switch-project-function nil
  "Function called with a project root before an agent's terminal is shown.
It is how the rest of the frame follows the agent.  Set it to
my-tab-bar-select-project (see my-tabs.el) and visiting an agent moves
the tab bar's chip -- and with it the tab line -- to the project the
agent runs in, instead of leaving the frame on whichever project it
happened to be showing.  The argument is the root directory
`project-current' finds for the agent's working directory; an agent
outside any project leaves the frame where it is.  When nil, visiting
an agent only shows its buffer."
  :type '(choice (const :tag "Do not switch projects" nil)
                 (function :tag "Function"))
  :group 'pi-sidecar)

(defcustom pi-sidecar-status-labels
  '((idle . "idle")
    (running . "running")
    (finished . "finished")
    (blocked . "needs input")
    (error . "error"))
  "Status words shown at the right edge of an agent row.
The keys are the sidecar's own states.  Pi's OSC 7501 `working' is
shown as `running' and its `done' as `finished'."
  :type '(alist :key-type symbol :value-type string)
  :group 'pi-sidecar)

(defcustom pi-sidecar-pi-programs '("pi" "pi-coding-agent")
  "Executable names that identify a pi process.
Matched against the process' comm field, the basename of argv[0] and any
path component of the command line, so the npm launcher and a compiled
binary are both recognized."
  :type '(repeat string)
  :group 'pi-sidecar)

(defcustom pi-sidecar-session-root (expand-file-name "~/.pi/agent/sessions/")
  "Fallback root of pi's session store.
The running pi's `PI_CODING_AGENT_SESSION_DIR' and
`PI_CODING_AGENT_DIR' environment variables are honored first, as pi
honors them."
  :type 'directory
  :group 'pi-sidecar)

(defcustom pi-sidecar-session-match-window 300
  "Seconds after a pi process starts that its session file may appear."
  :type 'natnum
  :group 'pi-sidecar)

(defcustom pi-sidecar-session-match-grace 30
  "Seconds before a pi process starts that its session file may appear."
  :type 'natnum
  :group 'pi-sidecar)

(defconst pi-sidecar-buffer-name "*pi-sidecar*"
  "Name of the sidecar buffer.")

(defconst pi-sidecar--status-icon "\u25cf" ; ●
  "Circle drawn in front of every agent, colored by status.")

(defconst pi-sidecar--status-priority
  '((blocked . 0) (error . 1) (running . 2) (finished . 3) (idle . 4))
  "Sort order for `pi-sidecar-sort' set to `status'.")

(defconst pi-sidecar--model-tail-bytes (* 1024 1024)
  "Bytes read from the end of a session file when looking for its model.")

;;;; Faces
;;
;; The dot and the status word share a face per state.  The colors follow
;; herdr's agent panel: green idle, yellow working, teal finished, red
;; blocked, red bold error.  Dark and light variants keep the panel
;; readable when the theme is flipped.

(defface pi-sidecar-idle
  '((((background dark)) :foreground "#98be65")
    (((background light)) :foreground "#2f7d32")
    (t :inherit success))
  "Face for an idle pi agent."
  :group 'pi-sidecar)

(defface pi-sidecar-running
  '((((background dark)) :foreground "#ECBE7B")
    (((background light)) :foreground "#8a6d00")
    (t :inherit warning))
  "Face for a pi agent that is running."
  :group 'pi-sidecar)

(defface pi-sidecar-finished
  '((((background dark)) :foreground "#4db5bd")
    (((background light)) :foreground "#006d77")
    (t :inherit font-lock-constant-face))
  "Face for a pi agent whose last run finished."
  :group 'pi-sidecar)

(defface pi-sidecar-blocked
  '((((background dark)) :foreground "#ff6c6b")
    (((background light)) :foreground "#b00020")
    (t :inherit error))
  "Face for a pi agent waiting for the user."
  :group 'pi-sidecar)

(defface pi-sidecar-error
  '((((background dark)) :foreground "#ff6c6b" :weight bold)
    (((background light)) :foreground "#b00020" :weight bold)
    (t :inherit error :weight bold))
  "Face for a pi agent whose last run failed."
  :group 'pi-sidecar)

(defface pi-sidecar-unknown
  '((((background dark)) :foreground "#5B6268")
    (((background light)) :foreground "#6c6c6c")
    (t :inherit shadow))
  "Face for a pi agent whose status is not known yet."
  :group 'pi-sidecar)

(defface pi-sidecar-model
  '((t :inherit bold))
  "Face for the model name in an agent row."
  :group 'pi-sidecar)

(defface pi-sidecar-path
  '((t :inherit shadow))
  "Face for the working directory line of an agent row."
  :group 'pi-sidecar)

(defface pi-sidecar-selected
  '((((background dark)) :background "#32363f")
    (((background light)) :background "#e4e4e4")
    (t :inherit highlight))
  "Face for the agent row at point."
  :group 'pi-sidecar)

(defface pi-sidecar-mouse
  '((t :inherit highlight))
  "Face for an agent row under the mouse."
  :group 'pi-sidecar)

(defface pi-sidecar-empty
  '((t :inherit shadow :slant italic))
  "Face for the empty-state line."
  :group 'pi-sidecar)

(defface pi-sidecar-separator
  '((((background dark)) :foreground "#5b6268" :underline t)
    (((background light)) :foreground "#c4c4c4" :underline t)
    (t :inherit shadow :underline t))
  "Face for the rule drawn between two agent rows.
The rule is the underline of a line of spaces, so it is as wide as the
window and costs no glyph of its own."
  :group 'pi-sidecar)

;;;; State

(defvar pi-sidecar--timer nil
  "Repeating refresh timer, or nil while the sidecar is closed.")

(defvar pi-sidecar--hidden nil
  "Non-nil when the sidecar was closed on purpose.
The tab bar's hooks keep the sidecar in every workspace; this is what
stops them from bringing back one that `C-c P' or `q' has just closed.
`C-c P' clears it.")

(defvar pi-sidecar--refresh-pending nil
  "Non-nil while a hook-triggered refresh is already scheduled.")

(defvar pi-sidecar--model-cache (make-hash-table :test #'equal)
  "Session file to (MTIME . (MODEL . PROVIDER)) cache.")

(defvar-local pi-sidecar--entries nil
  "Agent plists currently shown in this sidecar buffer.")

(defvar-local pi-sidecar--selection-overlay nil
  "Overlay marking the agent row at point.")

;;;; Mode line
;;
;; The sidecar gets a strip of its own at the bottom of its window: a
;; modeline with the bar at its start and the active/inactive faces that
;; doom-modeline picks for the window the strip belongs to, so selecting
;; the sidecar lights it up and selecting another window dims it.

(defun pi-sidecar--agent-count ()
  "Return a summary of the agents currently listed."
  (let* ((entries pi-sidecar--entries)
         (total (length entries))
         (busy (seq-count (lambda (entry)
                            (memq (plist-get entry :status)
                                  '(running blocked)))
                          entries)))
    (cond ((zerop total) "no agents")
          ((zerop busy) (format "%d agent%s"
                                total (if (= total 1) "" "s")))
          (t (format "%d/%d busy" busy total)))))

(when (fboundp 'doom-modeline-def-modeline)
  (doom-modeline-def-segment pi-sidecar-agents
    "Number of agents listed in the sidecar."
    (concat (propertize (pi-sidecar--agent-count)
                        'face (doom-modeline-face
                               'doom-modeline-buffer-minor-mode))
            (doom-modeline-spc)))
  ;; The bar and the mode name on the left, the count on the right, in the
  ;; shape of the modelines in the windows beside it.
  (doom-modeline-def-modeline 'pi-sidecar
    '(bar " " major-mode)
    '(pi-sidecar-agents)))

(defun pi-sidecar--setup-mode-line ()
  "Use the sidecar's own doom-modeline strip in the current buffer."
  (when (fboundp 'doom-modeline-format--pi-sidecar)
    (doom-modeline-set-modeline 'pi-sidecar)))

;;;; Process and session inspection
;;
;; Everything below reads the local system: /proc for the pi process and
;; its cwd, and pi's JSONL sessions for the model.  All calls are
;; defensive; a vanished process or an unreadable session simply leaves a
;; field empty rather than breaking the refresh.

(defun pi-sidecar--proc-env (pid variable)
  "Return VARIABLE from PID's environment, or nil."
  (let ((path (format "/proc/%d/environ" pid)))
    (when (file-readable-p path)
      (ignore-errors
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path)
          (let ((prefix (concat variable "=")))
            (cl-loop for entry in (split-string (buffer-string) "\0" t)
                     when (string-prefix-p prefix entry)
                     return (decode-coding-string
                             (substring entry (length prefix)) 'utf-8))))))))

(defun pi-sidecar--proc-cwd (pid)
  "Return PID's working directory as a directory file name, or nil."
  (when-let* ((link (format "/proc/%d/cwd" pid))
              ((file-exists-p link)))
    (ignore-errors
      (let ((dir (file-truename link)))
        (and (file-directory-p dir)
             (directory-file-name dir))))))

(defun pi-sidecar--pi-process-p (pid)
  "Return non-nil when PID looks like a pi process."
  (when (and (integerp pid) (> pid 0))
    (when-let* ((attrs (ignore-errors (process-attributes pid))))
      (let* ((comm (alist-get 'comm attrs))
             (args (alist-get 'args attrs))
             (argv (cond ((listp args) args)
                         ((stringp args) (split-string args " " t))
                         (t nil)))
             (argv0 (car argv))
             (line (mapconcat #'identity argv " ")))
        (or (member comm pi-sidecar-pi-programs)
            (member (and argv0 (file-name-nondirectory argv0))
                    pi-sidecar-pi-programs)
            (seq-some (lambda (program)
                        (string-match-p
                         (concat "\\(?:^\\|/\\)"
                                 (regexp-quote program)
                                 "\\(?:$\\|/\\)")
                         line))
                      pi-sidecar-pi-programs))))))

(defun pi-sidecar--session-root (pid)
  "Return pi's session store for the pi process PID."
  (file-name-as-directory
   (or (when-let* ((dir (pi-sidecar--proc-env
                         pid "PI_CODING_AGENT_SESSION_DIR")))
         (expand-file-name dir))
       (when-let* ((agent (pi-sidecar--proc-env
                           pid "PI_CODING_AGENT_DIR")))
         (expand-file-name "sessions/" (expand-file-name agent)))
       pi-sidecar-session-root)))

(defun pi-sidecar--session-dir (cwd root)
  "Return pi's session directory for CWD under ROOT.
Pi drops the leading slash, turns every slash, backslash and colon into
a dash, and wraps the result in double dashes."
  (expand-file-name
   (concat "--"
           (replace-regexp-in-string
            "[/\\\\:]" "-"
            (replace-regexp-in-string "\\`[/\\\\]" "" cwd))
           "--")
   root))

(defun pi-sidecar--session-files (cwd root)
  "Return the pi session files created for CWD under ROOT."
  (let ((dir (pi-sidecar--session-dir cwd root)))
    (and (file-directory-p dir)
         (directory-files dir t "\\`[0-9].*\\.jsonl\\'"))))

(defun pi-sidecar--session-name-time (file)
  "Return the creation time encoded in the pi session FILE name, or nil.
Pi names sessions TIMESTAMP_ID.jsonl, TIMESTAMP being ISO 8601 in UTC
with colons replaced by dashes."
  (let ((name (file-name-nondirectory file)))
    (when (string-match
           (concat "\\`\\([0-9]\\{4\\}\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T"
                   "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)"
                   "\\(?:-[0-9]+\\)?Z")
           name)
      (ignore-errors
        (date-to-time (format "%s-%s-%sT%s:%s:%sZ"
                              (match-string 1 name) (match-string 2 name)
                              (match-string 3 name) (match-string 4 name)
                              (match-string 5 name) (match-string 6 name)))))))

(defun pi-sidecar--file-mtime (file)
  "Return FILE's modification time as a float, or 0."
  (or (ignore-errors
        (float-time (file-attribute-modification-time (file-attributes file))))
      0.0))

(defun pi-sidecar--session-near (files start claimed)
  "Return the session file created closest to process start time START.
FILES are the candidates, CLAIMED a hash of files already taken by
another terminal.  The file must fall inside the match window."
  (let ((start (float-time start))
        best
        best-distance)
    (dolist (file files)
      (unless (gethash file claimed)
        (when-let* ((created (pi-sidecar--session-name-time file))
                    (delta (- (float-time created) start)))
          (when (and (<= delta pi-sidecar-session-match-window)
                     (>= delta (- pi-sidecar-session-match-grace))
                     (or (null best-distance)
                         (< (abs delta) best-distance)))
            (setq best file
                  best-distance (abs delta))))))
    best))

(defun pi-sidecar--assign-sessions (entries)
  "Attach a session file and its model to each agent in ENTRIES.
Start-time matches are assigned first so a resumed session's newest file
cannot steal the file of a pi that has just started in the same
directory; leftovers then take the newest unclaimed session."
  (let ((claimed (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (when-let* ((start (plist-get entry :start))
                  (file (pi-sidecar--session-near
                         (plist-get entry :files) start claimed)))
        (plist-put entry :session file)
        (puthash file t claimed)))
    (dolist (entry entries)
      (unless (plist-get entry :session)
        (let ((files (sort (copy-sequence (plist-get entry :files))
                           (lambda (a b)
                             (> (pi-sidecar--file-mtime a)
                                (pi-sidecar--file-mtime b))))))
          (when-let* ((file (seq-find (lambda (f) (not (gethash f claimed)))
                                      files)))
            (plist-put entry :session file)
            (puthash file t claimed)))))
    (dolist (entry entries)
      (when-let* ((file (plist-get entry :session))
                  (model (pi-sidecar--session-model file)))
        (plist-put entry :model (car model))
        (plist-put entry :provider (cdr model))))))

;;;; Model

(defun pi-sidecar--model-from-line (line)
  "Return (MODEL . PROVIDER) from a session JSONL LINE, or nil.
Both `model_change' entries and assistant messages carry the model used
at that point in the session."
  (condition-case nil
      (let* ((entry (json-parse-string line :object-type 'alist))
             (type (alist-get 'type entry)))
        (cond
         ((equal type "model_change")
          (let ((model (alist-get 'modelId entry)))
            (when (and (stringp model) (not (string-empty-p model)))
              (cons model (alist-get 'provider entry)))))
         ((equal type "message")
          (let* ((message (alist-get 'message entry))
                 (model (alist-get 'model message)))
            (when (and (equal (alist-get 'role message) "assistant")
                       (stringp model) (not (string-empty-p model)))
              (cons model (alist-get 'provider message)))))))
    (error nil)))

(defun pi-sidecar--model-in-lines (lines)
  "Return the last model entry found in LINES."
  (catch 'found
    (dolist (line (reverse lines))
      (when (and (not (string-empty-p line))
                 (string-match-p
                  "\"type\":\"model_change\"\\|\"role\":\"assistant\""
                  line))
        (when-let* ((model (pi-sidecar--model-from-line line)))
          (throw 'found model))))))

(defun pi-sidecar--read-model-tail (file size)
  "Read the last part of session FILE and return its model entry.
SIZE is the file size.  Only complete lines in the tail are considered."
  (let ((beg (max 0 (- size pi-sidecar--model-tail-bytes))))
    (with-temp-buffer
      (set-buffer-multibyte t)
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file nil beg size))
      (when (> beg 0)
        ;; The chunk starts in the middle of a line; drop that fragment.
        (goto-char (point-min))
        (forward-line 1))
      (pi-sidecar--model-in-lines (split-string (buffer-string) "\n")))))

(defun pi-sidecar--read-model-full (file)
  "Scan all of session FILE for its last model entry."
  (with-temp-buffer
    (set-buffer-multibyte t)
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (let (result)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (and (not (string-empty-p line))
                     (string-match-p
                      "\"type\":\"model_change\"\\|\"role\":\"assistant\""
                      line))
            (setq result (or (pi-sidecar--model-from-line line) result))))
        (forward-line 1))
      result)))

(defun pi-sidecar--session-model (file)
  "Return (MODEL . PROVIDER) for session FILE, cached by modification time."
  (let* ((attrs (ignore-errors (file-attributes file)))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (size (and attrs (file-attribute-size attrs)))
         (cached (gethash file pi-sidecar--model-cache)))
    (cond
     ((and cached (equal (car cached) mtime))
      (cdr cached))
     ((and (integerp size) (> size 0))
      (let ((result (or (pi-sidecar--read-model-tail file size)
                        (and cached (cdr cached))
                        (pi-sidecar--read-model-full file))))
        (puthash file (cons mtime result) pi-sidecar--model-cache)
        result))
     (t (and cached (cdr cached))))))

;;;; Status

(defun pi-sidecar--status-info (records)
  "Return status information derived from ghostel RECORDS.
RECORDS is the buffer-local `ghostel-program-status' alist.  The result
is a plist with `:status' (idle, running, finished, blocked, error),
`:state' (pi's own state), and `:message'/`:kind' when pi sent them."
  (let* ((states (mapcar (lambda (record) (plist-get (cdr record) :state))
                         records))
         (state (cond ((memq 'blocked states) 'blocked)
                      ((memq 'error states) 'error)
                      ((memq 'working states) 'working)
                      ((memq 'done states) 'done)
                      ((memq 'idle states) 'idle)))
         (root (cdr (assoc "" records)))
         (record (when state
                   (if (and root (eq (plist-get root :state) state))
                       root
                     (cdr (seq-find (lambda (entry)
                                      (eq (plist-get (cdr entry) :state)
                                          state))
                                    records))))))
    (list :state state
          :status (pcase state
                    ('working 'running)
                    ('done 'finished)
                    ('blocked 'blocked)
                    ('error 'error)
                    (_ 'idle))
          :message (plist-get record :msg)
          :kind (plist-get record :kind))))

;;;; Collecting agents

(defun pi-sidecar--buffer-entry (buffer)
  "Return an agent plist for ghostel BUFFER, or nil when it runs no pi."
  (with-current-buffer buffer
    (let* ((fg (and (fboundp 'ghostel-foreground-pid)
                    (ignore-errors (ghostel-foreground-pid))))
           (pid (and fg (pi-sidecar--pi-process-p fg) fg))
           (records (and (boundp 'ghostel-program-status)
                         ghostel-program-status))
           (info (pi-sidecar--status-info records))
           (status (plist-get info :status))
           (pi-record (seq-some (lambda (entry)
                                  (equal (plist-get (cdr entry) :app) "pi"))
                                records)))
      ;; A foreground pi is the normal case.  The record fallback only
      ;; covers platforms where the PTY foreground probe is unavailable,
      ;; and only while pi is busy: `done'/`error'/`idle' records survive
      ;; pi's exit, so trusting them then would keep a dead agent listed.
      (when (or pid
                (and pi-record (memq (plist-get info :state)
                                     '(working blocked))))
        (let* ((cwd (or (and pid (pi-sidecar--proc-cwd pid))
                        (and (boundp 'default-directory)
                             (expand-file-name default-directory))))
               (root (if pid
                         (pi-sidecar--session-root pid)
                       (file-name-as-directory pi-sidecar-session-root)))
               (files (and cwd (pi-sidecar--session-files cwd root))))
          (list :buffer buffer
                :pid pid
                :cwd (and cwd (directory-file-name cwd))
                :start (and pid (alist-get
                                 'start (ignore-errors
                                          (process-attributes pid))))
                :files files
                :status status
                :message (plist-get info :message)
                :kind (plist-get info :kind)))))))

(defun pi-sidecar--entry-priority (entry)
  "Return the attention rank of ENTRY."
  (or (alist-get (plist-get entry :status) pi-sidecar--status-priority) 9))

(defun pi-sidecar--compare-project (a b)
  "Compare entries A and B by project, then status and buffer name."
  (let ((ca (or (plist-get a :cwd) ""))
        (cb (or (plist-get b :cwd) "")))
    (cond
     ((not (equal ca cb)) (string< ca cb))
     ((/= (pi-sidecar--entry-priority a) (pi-sidecar--entry-priority b))
      (< (pi-sidecar--entry-priority a) (pi-sidecar--entry-priority b)))
     (t (string< (buffer-name (plist-get a :buffer))
                 (buffer-name (plist-get b :buffer)))))))

(defun pi-sidecar--compare-status (a b)
  "Compare entries A and B by status, then project and buffer name."
  (let ((pa (pi-sidecar--entry-priority a))
        (pb (pi-sidecar--entry-priority b)))
    (if (/= pa pb)
        (< pa pb)
      (pi-sidecar--compare-project a b))))

(defun pi-sidecar--collect ()
  "Return the sorted list of pi agents in this Emacs."
  (let (entries)
    (dolist (buffer (ghostel-buffer-list))
      (when-let* ((entry (pi-sidecar--buffer-entry buffer)))
        (push entry entries)))
    (setq entries (nreverse entries))
    (pi-sidecar--assign-sessions entries)
    (when (> (hash-table-count pi-sidecar--model-cache) 512)
      (clrhash pi-sidecar--model-cache))
    (sort entries (if (eq pi-sidecar-sort 'status)
                      #'pi-sidecar--compare-status
                    #'pi-sidecar--compare-project))))

;;;; Rendering

(defvar-keymap pi-sidecar-entry-map
  :doc "Keymap on a pi agent row."
  "<mouse-1>" #'pi-sidecar-visit-mouse)

(defun pi-sidecar--requested-width ()
  "Return the width to ask for when the sidecar window is created.
When the dired sidecar's listing is already in the column, the side area
has a width and both windows share it; this is the answer for when the
pi sidecar is the first one in."
  (or pi-sidecar-width 35))

(defun pi-sidecar--render-width ()
  "Return the number of columns available to the sidecar text."
  (or (when-let* ((window (get-buffer-window (current-buffer) t)))
        (window-body-width window))
      (pi-sidecar--requested-width)))

(defun pi-sidecar--truncate (string width)
  "Truncate STRING to WIDTH columns with an ellipsis."
  (if (<= (string-width string) width)
      string
    (truncate-string-to-width string width nil nil "\u2026")))

(defun pi-sidecar--ellipsize-left (string width)
  "Shorten STRING to WIDTH columns, keeping its trailing path components.
A path that does not fit is reduced to `…/last/components', so the
project's own name stays visible.  A single component that is too long
keeps the end of that component instead."
  (if (<= (string-width string) width)
      string
    (let* ((budget (max 0 (- width 2)))
           (components (split-string string "/" t))
           (tail nil))
      (when (cdr components)
        (catch 'stop
          (dolist (component (reverse components))
            (let ((candidate (if tail
                                 (concat component "/" tail)
                               component)))
              (if (<= (string-width candidate) budget)
                  (setq tail candidate)
                (throw 'stop nil))))))
      (if tail
          (concat "\u2026/" tail)
        ;; A single long component (or a string without slashes): keep
        ;; as much of its end as fits.
        (let* ((budget (max 0 (1- width)))
               (chars (nreverse (string-to-list string)))
               (kept nil)
               (used 0))
          (dolist (char chars)
            (let ((char-width (char-width char)))
              (when (<= (+ used char-width) budget)
                (push char kept)
                (cl-incf used))))
          (concat "\u2026" (apply #'string kept)))))))

(defun pi-sidecar--status-face (status)
  "Return the face for STATUS."
  (pcase status
    ('idle 'pi-sidecar-idle)
    ('running 'pi-sidecar-running)
    ('finished 'pi-sidecar-finished)
    ('blocked 'pi-sidecar-blocked)
    ('error 'pi-sidecar-error)
    (_ 'pi-sidecar-unknown)))

(defun pi-sidecar--status-label (status)
  "Return the label for STATUS."
  (or (alist-get status pi-sidecar-status-labels)
      (symbol-name status)))

(defun pi-sidecar--entry-help (entry)
  "Return the help echo string for ENTRY."
  (let ((model (plist-get entry :model))
        (session (plist-get entry :session))
        (message (plist-get entry :message))
        (kind (plist-get entry :kind)))
    (string-join
     (delq nil
           (list (buffer-name (plist-get entry :buffer))
                 (format "status: %s"
                         (pi-sidecar--status-label (plist-get entry :status)))
                 (format "pid: %s" (or (plist-get entry :pid) "?"))
                 (plist-get entry :cwd)
                 (when model
                   (format "model: %s%s" model
                           (if-let* ((provider (plist-get entry :provider)))
                               (format " (%s)" provider)
                             "")))
                 (when kind (format "kind: %s" kind))
                 (when message (format "message: %s" message))
                 (when session (format "session: %s" session))))
     "\n")))

(defun pi-sidecar--insert-entry (entry width)
  "Insert one agent row for ENTRY, WIDTH columns wide."
  (let* ((start (point))
         (key (plist-get entry :buffer))
         (status (plist-get entry :status))
         (face (pi-sidecar--status-face status))
         (status-text (pi-sidecar--truncate
                       (pi-sidecar--status-label status)
                       (max 1 (- width 2))))
         (status-width (string-width status-text))
         (model (or (plist-get entry :model) "model ?"))
         (model-width (max 1 (- width 4 status-width)))
         (model (pi-sidecar--truncate model model-width))
         (model-face (if (plist-get entry :model)
                         'pi-sidecar-model
                       'pi-sidecar-unknown))
         (path (or (plist-get entry :cwd) "?"))
         (path (pi-sidecar--ellipsize-left
                (abbreviate-file-name path) (max 1 (- width 3)))))
    ;; First line: dot, model, right-aligned status.
    (insert " ")
    (insert (propertize pi-sidecar--status-icon 'face face))
    (insert " ")
    (insert (propertize model 'face model-face))
    ;; The space ends STATUS-WIDTH+1 columns before the right edge, so the
    ;; status word that follows ends one column short of it.
    (insert (propertize " " 'display
                        `(space :align-to (- right ,(1+ status-width)))))
    (insert (propertize status-text 'face face))
    (insert "\n")
    ;; Second line: the project path, indented like herdr's second row.
    ;; Pad it to the window width so the whole row, not just the path text,
    ;; is a mouse target.
    (insert "   ")
    (insert (propertize path 'face 'pi-sidecar-path))
    (insert (make-string (max 0 (- width 3 (string-width path))) ?\s))
    (insert "\n")
    (add-text-properties
     start (point)
     (list 'pi-sidecar-key key
           'help-echo (pi-sidecar--entry-help entry)
           'keymap pi-sidecar-entry-map))
    ;; Keep the hover face off the trailing newline: with it there, the
    ;; property would run unbroken from the first row to the last and Emacs
    ;; would highlight every row at once.
    (add-text-properties start (1- (point))
                         (list 'mouse-face 'pi-sidecar-mouse))))

(defun pi-sidecar--insert-separator (width)
  "Insert a horizontal rule WIDTH columns wide.
The rule is the underline of a row of spaces in the
pi-sidecar-separator face, which keeps it free of glyphs and lets it
run the whole width of the window."
  (insert (propertize (make-string (max 1 width) ?\s)
                      'face 'pi-sidecar-separator))
  (insert "\n"))

(defun pi-sidecar--insert-empty ()
  "Insert the placeholder shown when no pi is running."
  (insert "  ")
  (insert (propertize "no pi agents running" 'face 'pi-sidecar-empty))
  (insert "\n"))

;;;; Navigation and commands

(defun pi-sidecar--key-at-point ()
  "Return the ghostel buffer of the agent row at point, or nil."
  (get-text-property (point) 'pi-sidecar-key))

(defun pi-sidecar--goto-key (key)
  "Move point to the agent row for KEY."
  (goto-char (point-min))
  (let ((pos (point-min))
        (found nil))
    (while (and (not found) (< pos (point-max)))
      (if (equal (get-text-property pos 'pi-sidecar-key) key)
          (progn
            (goto-char pos)
            (setq found t))
        (setq pos (next-single-property-change
                   pos 'pi-sidecar-key nil (point-max)))))
    (unless found
      (goto-char (point-min)))))

(defun pi-sidecar--move (n)
  "Move N rows forward (or backward when N is negative)."
  (let* ((keys (mapcar (lambda (entry) (plist-get entry :buffer))
                       pi-sidecar--entries))
         (current (pi-sidecar--key-at-point))
         (length (length keys)))
    (cond
     ((zerop length)
      (user-error "No pi agents running"))
     (t
      (let* ((index (seq-position keys current #'eq))
             (next (if index
                       (mod (+ index n) length)
                     (if (> n 0) 0 (1- length)))))
        (pi-sidecar--goto-key (nth next keys))
        (pi-sidecar--highlight))))))

(defun pi-sidecar-next ()
  "Move to the next pi agent."
  (interactive)
  (pi-sidecar--move 1))

(defun pi-sidecar-previous ()
  "Move to the previous pi agent."
  (interactive)
  (pi-sidecar--move -1))

(defun pi-sidecar--entry-at-key (key)
  "Return the agent entry for buffer KEY, or nil."
  (seq-find (lambda (entry) (eq (plist-get entry :buffer) key))
            pi-sidecar--entries))

(defun pi-sidecar--entry-root (entry)
  "Return the project root of the directory ENTRY's agent runs in, or nil.
The directory comes from the agent's process, not from the buffer it
runs in: `default-directory' follows the shell, which may have been in
another project when the terminal was opened."
  (when-let* ((directory (or (plist-get entry :cwd)
                             (when (buffer-live-p (plist-get entry :buffer))
                               (with-current-buffer (plist-get entry :buffer)
                                 default-directory)))))
    (ignore-errors (project-root (project-current nil directory)))))

(defun pi-sidecar--show-buffer (buffer)
  "Show BUFFER in a window of this frame and select it.
A window already displaying BUFFER -- one restored together with the
agent's project tab, say -- is selected rather than replaced."
  (let ((window (get-buffer-window buffer 'visible)))
    (if (and window (not (window-parameter window 'window-side)))
        (progn
          (select-frame-set-input-focus (window-frame window))
          (select-window window))
      (pop-to-buffer buffer '((display-buffer-reuse-window)
                              (reusable-frames . visible))))))

(defun pi-sidecar-visit ()
  "Visit the ghostel buffer of the pi agent at point.
With `pi-sidecar-switch-project-function' set, the frame first moves to
the project the agent works in, so the tab bar's chip and the tab line
describe that project rather than the one the frame was showing."
  (interactive)
  (let ((key (pi-sidecar--key-at-point)))
    (unless (buffer-live-p key)
      (user-error "No pi agent at point"))
    (when (functionp pi-sidecar-switch-project-function)
      (when-let* ((root (pi-sidecar--entry-root
                         (pi-sidecar--entry-at-key key))))
        (funcall pi-sidecar-switch-project-function root)))
    (pi-sidecar--show-buffer key)
    (message "pi: %s" (buffer-name key))))

(defun pi-sidecar-visit-mouse (event)
  "Visit the pi agent clicked in EVENT.
Point is set in EVENT's buffer without selecting its window: visiting
selects the agent's window, and a `with-selected-window' around the
call would put the sidecar back in front as it unwound."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position))
         (point (posn-point position)))
    (when (and (windowp window) (integer-or-marker-p point))
      (with-current-buffer (window-buffer window)
        (goto-char point)
        (pi-sidecar-visit)))))

(defun pi-sidecar-toggle-sort ()
  "Switch between project and status ordering."
  (interactive)
  (setq pi-sidecar-sort (if (eq pi-sidecar-sort 'status) 'project 'status))
  (pi-sidecar-refresh)
  (message "Pi sidecar sorted by %s" pi-sidecar-sort))

(defun pi-sidecar-quit ()
  "Close the sidecar window, in this workspace and the others.
`pi-sidecar-open' -- `C-c P' -- is what brings it back."
  (interactive)
  (setq pi-sidecar--hidden t)
  (pi-sidecar--stop-timer)
  (pi-sidecar--remove-hooks)
  (let ((windows (pi-sidecar--windows)))
    (if windows
        (dolist (window windows)
          (quit-window nil window))
      (when-let* ((buffer (get-buffer pi-sidecar-buffer-name)))
        (kill-buffer buffer)))))

;;;; Highlight

(defun pi-sidecar--window-selected-p ()
  "Return non-nil when a window showing this buffer is selected on its frame."
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (eq window (frame-selected-window (window-frame window)))))

(defun pi-sidecar--highlight (&rest _)
  "Mark the whole entry at point with `pi-sidecar-selected'.
The overlay is shown only while the sidecar's window is selected: it is
the cursor replacement for moving between rows, not an inactive marker."
  (when (overlayp pi-sidecar--selection-overlay)
    (let ((key (and (pi-sidecar--window-selected-p)
                    (pi-sidecar--key-at-point))))
      (if (null key)
          (move-overlay pi-sidecar--selection-overlay (point-min) (point-min))
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (when (and (> start (point-min))
                     (equal (get-text-property (1- start) 'pi-sidecar-key) key))
            (setq start (line-beginning-position 0)))
          (when (and (< end (point-max))
                     (equal (get-text-property (1+ end) 'pi-sidecar-key) key))
            (setq end (line-end-position 2)))
          (move-overlay pi-sidecar--selection-overlay
                        start end (current-buffer)))))))

(defun pi-sidecar--selection-change (window)
  "Update the row highlight when WINDOW is selected or deselected."
  (when (eq window (get-buffer-window (current-buffer) t))
    (pi-sidecar--highlight)))

;;;; Refresh

(defun pi-sidecar-refresh ()
  "Rebuild the sidecar from the live pi agents."
  (interactive)
  (unless (derived-mode-p 'pi-sidecar-mode)
    (user-error "Not in a pi sidecar buffer"))
  (let ((key (pi-sidecar--key-at-point))
        (inhibit-read-only t))
    (setq pi-sidecar--entries (pi-sidecar--collect))
    (erase-buffer)
    (let ((width (pi-sidecar--render-width))
          (first t))
      (if pi-sidecar--entries
          (dolist (entry pi-sidecar--entries)
            (unless first
              (pi-sidecar--insert-separator width))
            (setq first nil)
            (pi-sidecar--insert-entry entry width))
        (pi-sidecar--insert-empty)))
    (if key
        (pi-sidecar--goto-key key)
      (goto-char (point-min)))
    (pi-sidecar--highlight)))

(defun pi-sidecar--revert (&rest _)
  "Revert function for `special-mode'."
  (pi-sidecar-refresh))

;;;; Timer and hooks

(defun pi-sidecar--timer-refresh ()
  "Refresh the sidecar from its repeating timer."
  (let ((buffer (get-buffer pi-sidecar-buffer-name)))
    (when (and (buffer-live-p buffer)
               (get-buffer-window buffer t))
      (with-current-buffer buffer
        (pi-sidecar-refresh)))))

(defun pi-sidecar--start-timer ()
  "Start the repeating refresh timer."
  (unless (timerp pi-sidecar--timer)
    (setq pi-sidecar--timer
          (run-at-time pi-sidecar-refresh-interval
                       pi-sidecar-refresh-interval
                       #'pi-sidecar--timer-refresh))))

(defun pi-sidecar--stop-timer ()
  "Cancel the repeating refresh timer."
  (when (timerp pi-sidecar--timer)
    (cancel-timer pi-sidecar--timer))
  (setq pi-sidecar--timer nil))

(defun pi-sidecar--note-change (&rest _)
  "Schedule a refresh after ghostel reports a status or process change."
  (when (and (timerp pi-sidecar--timer)
             (not pi-sidecar--refresh-pending)
             (get-buffer-window pi-sidecar-buffer-name t))
    (setq pi-sidecar--refresh-pending t)
    (run-at-time 0.15 nil #'pi-sidecar--flush-refresh)))

(defun pi-sidecar--flush-refresh ()
  "Run the refresh scheduled by `pi-sidecar--note-change'."
  (setq pi-sidecar--refresh-pending nil)
  (let ((buffer (get-buffer pi-sidecar-buffer-name)))
    (when (and (buffer-live-p buffer)
               (get-buffer-window buffer t))
      (with-current-buffer buffer
        (pi-sidecar-refresh)))))

(defun pi-sidecar--install-hooks ()
  "Listen for ghostel status and foreground changes."
  (add-hook 'ghostel-program-status-functions #'pi-sidecar--note-change)
  (add-hook 'ghostel-foreground-change-functions #'pi-sidecar--note-change))

(defun pi-sidecar--remove-hooks ()
  "Stop listening for ghostel status and foreground changes."
  (remove-hook 'ghostel-program-status-functions #'pi-sidecar--note-change)
  (remove-hook 'ghostel-foreground-change-functions #'pi-sidecar--note-change))

(defun pi-sidecar--cleanup ()
  "Stop the timer and drop hooks when the sidecar buffer dies."
  (pi-sidecar--stop-timer)
  (pi-sidecar--remove-hooks)
  (when (overlayp pi-sidecar--selection-overlay)
    (delete-overlay pi-sidecar--selection-overlay)))

;;;; Display

(defun pi-sidecar--display (buffer)
  "Display BUFFER in the left side window below the dired listing."
  (let* ((width (pi-sidecar--requested-width))
         (window (display-buffer
                  buffer
                  `((display-buffer-in-side-window)
                    (side . left)
                    (slot . 1)
                    (window-width . ,width)
                    (window-height . ,pi-sidecar-height)))))
    (when (window-live-p window)
      (unless (eq (window-buffer window) buffer)
        ;; A restored window can come back dedicated to what it held before;
        ;; it is the sidecar's window now, so take it over.
        (when (window-dedicated-p window)
          (set-window-dedicated-p window nil))
        (set-window-buffer window buffer))
      (set-window-dedicated-p window t)
      (set-window-parameter window 'no-delete-other-windows t))
    window))

(defun pi-sidecar--window (&optional frame)
  "Return FRAME's pi sidecar window, or nil."
  (seq-find (lambda (window)
              (with-current-buffer (window-buffer window)
                (derived-mode-p 'pi-sidecar-mode)))
            (window-list frame 'nomini)))

(defun pi-sidecar--in-place-p (window)
  "Non-nil when WINDOW is the left column's second side window.
A restored tab can leave the sidecar in the main area: the window state it
was saved with had already lost the side parameters, because the tab was
left while the sidecar was in the wrong place.  Displaying it again in that
window would keep it there."
  (and (eq (window-parameter window 'window-side) 'left)
       (equal (window-parameter window 'window-slot) 1)))

(defun pi-sidecar--restore-window (window)
  "Move the sidecar out of WINDOW, a window of the main area.
The window is removed first, so that `display-buffer' has to make a side
window for the sidecar instead of reusing this one.  The frame's main
window is left where it is: it is the only place the workspace's own
buffer can go, and `my-tabs--restore-main-windows' takes the sidecar out of
it before this package's hooks run."
  (let ((buffer (window-buffer window)))
    (when (and (window-parent window)
               (not (eq window (window-main-window (window-frame window)))))
      (delete-window window)
      (pi-sidecar--display buffer))))

(defun pi-sidecar--windows ()
  "Return every pi sidecar window, on every frame."
  (seq-filter (lambda (window)
                (with-current-buffer (window-buffer window)
                  (derived-mode-p 'pi-sidecar-mode)))
              (mapcan (lambda (frame) (window-list frame 'nomini))
                      (frame-list))))

(defun pi-sidecar--follow-tab (&rest _)
  "Put the sidecar into the current workspace, or take it out of it.
Run from the tab bar's hooks, so the agent list is there in every tab the
way the dired listing above it is.  A tab that restores its own sidecar
window already has one; only a tab without it asks for a new one, which is
what keeps each tab's height its own.

A hidden sidecar is removed here rather than merely left unmade, because a
tab restores the windows it was saved with -- the sidecar among them -- and
would otherwise bring back what `C-c P' has just closed."
  (if pi-sidecar--hidden
      (when-let* ((window (pi-sidecar--window)))
        (ignore-errors (delete-window window)))
    (if-let* ((window (pi-sidecar--window)))
        (progn
          (unless (pi-sidecar--in-place-p window)
            (pi-sidecar--restore-window window))
          (pi-sidecar--start-timer)
          (pi-sidecar--install-hooks))
      (pi-sidecar-open))))

;;;###autoload
(defun pi-sidecar-open ()
  "Open the pi agent sidecar, below the dired listing."
  (interactive)
  (setq pi-sidecar--hidden nil)
  (let ((origin (current-buffer))
        (buffer (get-buffer-create pi-sidecar-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'pi-sidecar-mode)
        (pi-sidecar-mode))
      (pi-sidecar-refresh)
      ;; Start on the terminal the command was invoked from, when it is one
      ;; of the listed agents.
      (when (and (not (eq origin buffer))
                 (seq-some (lambda (entry)
                             (eq (plist-get entry :buffer) origin))
                           pi-sidecar--entries))
        (pi-sidecar--goto-key origin)
        (pi-sidecar--highlight)))
    (pi-sidecar--display buffer)
    (pi-sidecar--start-timer)
    (pi-sidecar--install-hooks)
    buffer))

;;;###autoload
(defun pi-sidecar-toggle ()
  "Show or hide the pi agent sidecar.
Hiding it hides it from every workspace: the hooks that keep it visible
stay out of the way until this is called again."
  (interactive)
  (if (pi-sidecar--window)
      (pi-sidecar-quit)
    (pi-sidecar-open)))

;; Each tab shows the sidecar, so the tab bar's hooks are where it follows
;; the workspace; a session puts it back after the windows are restored.
;; The depths keep the order readable: the tab's group is settled first
;; (my-tabs uses -20), the dired listing follows at 0, the sidecar at 10.
(add-hook 'tab-bar-tab-post-select-functions #'pi-sidecar--follow-tab 10)
(add-hook 'tab-bar-tab-post-open-functions #'pi-sidecar--follow-tab 10)
(add-hook 'emacs-startup-hook #'pi-sidecar--follow-tab 110)

;;;; Mode

(defvar-keymap pi-sidecar-mode-map
  :doc "Keymap for `pi-sidecar-mode'."
  "n" #'pi-sidecar-next
  "p" #'pi-sidecar-previous
  "C-n" #'pi-sidecar-next
  "C-p" #'pi-sidecar-previous
  "TAB" #'pi-sidecar-next
  "<backtab>" #'pi-sidecar-previous
  "<down>" #'pi-sidecar-next
  "<up>" #'pi-sidecar-previous
  "RET" #'pi-sidecar-visit
  "s" #'pi-sidecar-toggle-sort
  "q" #'pi-sidecar-quit)

(define-derived-mode pi-sidecar-mode special-mode "Pi-Sidecar"
  "Major mode for the pi agent status sidecar.

The sidecar lists every pi process running in a ghostel terminal of
this Emacs.  Each row shows the status dot and model on the first line,
the status word right-aligned, and the working directory below it, with
a horizontal rule between two agents.  The buffer has no tab line or
cursor; the highlighted row marks point instead, and the mode line at
the bottom of the window belongs to the sidecar alone.

\\{pi-sidecar-mode-map}"
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (setq-local header-line-format nil)
  ;; The sidecar is a status panel, not an editing buffer: no tab line, no
  ;; cursor (the selected-row overlay shows where point is).  Turning the
  ;; mode off here is not enough -- global-tab-line-mode turns it on for
  ;; every new buffer from after-change-major-mode-hook, which runs after
  ;; this function -- so the buffer also excludes itself, which is what
  ;; the globalised mode consults.
  (setq-local tab-line-exclude t)
  (setq-local tab-line-format nil)
  (tab-line-mode -1)
  (pi-sidecar--setup-mode-line)
  (setq-local cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local revert-buffer-function #'pi-sidecar--revert)
  (setq-local pi-sidecar--entries nil)
  (setq-local pi-sidecar--selection-overlay
              (make-overlay (point-min) (point-min)))
  (overlay-put pi-sidecar--selection-overlay 'face 'pi-sidecar-selected)
  (add-hook 'post-command-hook #'pi-sidecar--highlight nil t)
  (add-hook 'window-selection-change-functions
            #'pi-sidecar--selection-change nil t)
  (add-hook 'kill-buffer-hook #'pi-sidecar--cleanup nil t))

(provide 'pi-sidecar)
;;; pi-sidecar.el ends here
