;;; ghostel-restore.el --- Restore ghostel terminals and pi sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Edgar

;; Author: Edgar
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ghostel "0.51") (easysession "1.1"))
;; Keywords: terminals, convenience

;;; Commentary:

;; EasySession restores frames, windows and file buffers, but a ghostel
;; terminal is a live process: its pty, the shell in it and everything the
;; shell was running die with Emacs.  This package registers `ghostel-mode'
;; as an EasySession "managed major mode", so every terminal is saved (name,
;; working directory, ghostel identity) and respawned in the same directory
;; on the next start.
;;
;; It also puts pi back.  A pi session cannot be read off the terminal -- the
;; scrollback is gone -- so it is identified from the *running* pi process at
;; save time, before Emacs exits.  The terminal's shell pid is known
;; (`ghostel--pid'), so the shells' descendants are enumerated from /proc
;; (which also catches a pi started inside tmux or under a wrapper script),
;; and any descendant running the pi executable marks the terminal as a pi
;; terminal.  Its session file is then picked in this order:
;;
;;   1. an exact answer, when some descendant has PI_SESSION_FILE in its
;;      environment.  pi exports that variable to the processes it spawns,
;;      so a running tool usually pins the session down;
;;   2. the session pi created for itself: the file in
;;      ~/.pi/agent/sessions/<cwd>/ whose name-encoded timestamp is closest
;;      to the process start time.  pi names a fresh session after the
;;      moment it started, which is what makes this match reliable;
;;   3. otherwise a *guessed* one -- the newest session of that directory
;;      not already claimed by another terminal of this Emacs.  This is the
;;      session a `pi --continue' or `pi --session FILE' picked earlier is
;;      still appending to, and claiming keeps two terminals of the same
;;      directory from restoring the same session.
;;
;; pi itself is no help here: it sets `process.title', which overwrites its
;; argv (`--session' / `--continue' are gone from /proc/PID/cmdline within
;; milliseconds), and it keeps no session file open, so neither the command
;; line nor its file descriptors can be read back.
;;
;; On restore the shell is respawned in the saved directory and, once it has
;; had time to reach a prompt, `pi --session <file>' is typed into it.  The
;; shell stays the terminal's process, so pi keeps its usual scratch shell.
;;
;; What is not restored: the terminal's scrollback, pi's unsent draft
;; prompt, and command buffers (`ghostel-exec', `ghostel-compile'), which are
;; skipped rather than re-run -- the same policy as ghostel's own desktop.el
;; support.  A pi started with `--no-session', or with a `--session-dir'
;; other than the configured default, is not found either: both are decided
;; at run time and leave no trace in /proc.
;;
;; Setup, in init.el, after ghostel and easysession are loaded:
;;
;;   (use-package ghostel-restore
;;     :ensure nil
;;     :demand t
;;     :config (ghostel-restore-mode 1))
;;
;; `:demand t' matters.  EasySession asks for the mode's restore function
;; while it loads the session on `emacs-startup-hook', and an entry for a
;; mode that is not registered yet is dropped without a word -- the
;; `:restore' function is simply never called and no error is raised.  With
;; `:after (ghostel easysession)' and ghostel loading on demand, the
;; registration arrives only when the first terminal is opened, well after
;; the restore, and the saved terminals do not come back.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'easysession)
(require 'ghostel)

(defgroup ghostel-restore nil
  "Restore ghostel terminals, and the pi sessions running in them."
  :group 'ghostel
  :prefix "ghostel-restore-")

(defcustom ghostel-restore-pi t
  "Whether to resume the pi session running in a restored terminal.
When nil, only the shell is restored, in its working directory."
  :type 'boolean)

(defcustom ghostel-restore-pi-command "pi"
  "Command that starts pi in a restored terminal.
It is followed by `--session FILE' when the session is known."
  :type 'string)

(defcustom ghostel-restore-pi-programs '("pi" "pi-coding-agent")
  "Executable names that count as a pi process.
Matched against the process' short comm name and the basename of argv[0],
so a compiled binary and a script wrapper are both found."
  :type '(repeat string))

(defcustom ghostel-restore-pi-session-root
  (expand-file-name "~/.pi/agent/sessions")
  "Root directory of pi's session store, one directory per working directory.
`PI_CODING_AGENT_SESSION_DIR' and `PI_CODING_AGENT_DIR' in the pi process'
own environment override this, as they do in pi."
  :type 'directory)

(defcustom ghostel-restore-pi-match-window 300
  "How many seconds after a pi process start its session file may appear.
pi writes the session file at startup, so the two are seconds apart; the
window only has to absorb a slow or busy disk."
  :type 'natnum)

(defcustom ghostel-restore-pi-match-grace 30
  "How many seconds before a pi process start its session file may appear.
Covers the kernel clock tick and clock skew between the process start time
and the session file name; a resumed session is far outside this window."
  :type 'natnum)

(defcustom ghostel-restore-pi-guess-session t
  "Whether to fall back to the newest session of the terminal's directory.
Only used when the running pi's session file could not be identified; it
is what a `pi --session' or `pi --continue' is still appending to.  When
nil, such a terminal comes back as a plain shell."
  :type 'boolean)

(defcustom ghostel-restore-shell-delay 1.0
  "Seconds to wait before typing into a freshly spawned shell.
Input written earlier is not lost -- the pty queues it -- but a shell that
is still reading its rc files may hand it to whatever runs there, so a
short pause is safer.  The text and the Return are sent separately,
`ghostel-restore-return-delay' apart."
  :type 'number)

(defcustom ghostel-restore-return-delay 0.25
  "Seconds between typing the restore command and pressing Return."
  :type 'number)

(defvar ghostel-restore--claimed nil
  "pi session files handed to a terminal during the current save pass.
Cleared by the `easysession-before-save-hook' entry so each pass starts
empty, which is what keeps two terminals of the same directory from
restoring the same session.")

(defvar ghostel-restore--cache nil
  "Detection results of the current save pass, as (BUFFER-NAME . RESULT).
Scanning /proc once per terminal per pass is enough; the pass is one save.")

;; ----------------------------------------------------------------------
;; process inspection (/proc)
;; ----------------------------------------------------------------------

(defun ghostel-restore--proc-stat (pid)
  "Return (COMM . PPID) for PID, read from /proc/PID/stat.
The second field of that file is the executable name in parentheses and
may itself contain spaces and parentheses, so the remaining fields are
split after the last closing parenthesis."
  (let ((file (format "/proc/%d/stat" pid)))
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (let* ((text (buffer-string))
                 (close (cl-position ?\) text :from-end t)))
            (when close
              (let* ((open (or (cl-position ?\( text) -1))
                     (comm (substring text (1+ open) close))
                     (fields (split-string (substring text (1+ close)) " " t)))
                (cons comm (string-to-number (or (nth 1 fields) "")))))))))))

(defun ghostel-restore--children-table ()
  "Return a hash table mapping each pid to the list of its child pids."
  (let ((table (make-hash-table :test #'eql)))
    (dolist (name (directory-files "/proc" nil "\\`[0-9]+\\'"))
      (when-let* ((stat (ghostel-restore--proc-stat (string-to-number name)))
                  (ppid (cdr stat)))
        (when (> ppid 0)
          (push (string-to-number name) (gethash ppid table)))))
    table))

(defun ghostel-restore--descendants (pid)
  "Return the pids below PID, breadth first (nearest descendants first)."
  (let ((table (ghostel-restore--children-table))
        (queue (list pid))
        (seen (list pid))
        (result nil))
    (while queue
      (dolist (child (gethash (pop queue) table))
        (unless (memq child seen)
          (push child seen)
          (setq result (nconc result (list child))
                queue (nconc queue (list child))))))
    result))

(defun ghostel-restore--proc-cmdline (pid)
  "Return the argv of PID as a list of strings, or nil."
  (let ((path (format "/proc/%d/cmdline" pid)))
    (when (file-readable-p path)
      (ignore-errors
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path)
          (delq nil (split-string (buffer-string) "\0")))))))

(defun ghostel-restore--proc-cwd (pid)
  "Return PID's working directory, resolved, or nil."
  (ignore-errors
    (let ((dir (file-truename (format "/proc/%d/cwd" pid))))
      (and (file-directory-p dir) dir))))

(defun ghostel-restore--proc-env (pid variable)
  "Return the value of VARIABLE in PID's environment, or nil.
/proc/PID/environ holds the environment the process was started with, so
this sees what the shell exported, not what the program set for itself."
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

(defun ghostel-restore--terminal-pid (buffer)
  "Return the pid of BUFFER's shell process, or nil.
`ghostel--pid' is the terminal child for both the native pty path and the
Emacs-owned one; the process object is only a fallback."
  (with-current-buffer buffer
    (let ((pid (and (boundp 'ghostel--pid) ghostel--pid)))
      (or (and (integerp pid) (> pid 0) pid)
          (let ((proc (and (boundp 'ghostel--process) ghostel--process)))
            (and (processp proc)
                 (process-live-p proc)
                 (process-id proc)))))))

;; ----------------------------------------------------------------------
;; pi sessions
;; ----------------------------------------------------------------------

(defun ghostel-restore--pi-p (pid)
  "Return non-nil when PID is a pi process."
  (let ((comm (car (ghostel-restore--proc-stat pid)))
        (argv0 (car (ghostel-restore--proc-cmdline pid))))
    (or (member comm ghostel-restore-pi-programs)
        (member (and argv0 (file-name-nondirectory argv0))
                ghostel-restore-pi-programs))))

(defun ghostel-restore--session-root (pid)
  "Return pi's session store for the pi process PID.
pi reads both overrides from its environment at run time; when the shell
exported them, /proc has them, otherwise the configured default applies."
  (or (when-let* ((dir (ghostel-restore--proc-env
                        pid "PI_CODING_AGENT_SESSION_DIR")))
        (file-name-as-directory (expand-file-name dir)))
      (when-let* ((agent (ghostel-restore--proc-env
                          pid "PI_CODING_AGENT_DIR")))
        (expand-file-name "sessions/" (expand-file-name agent)))
      (file-name-as-directory ghostel-restore-pi-session-root)))

(defun ghostel-restore--session-dir (cwd root)
  "Return pi's session directory for CWD under ROOT.
pi names it from the path: the leading slash is dropped and every slash,
backslash and colon becomes a dash, all wrapped in double dashes."
  (expand-file-name
   (concat "--"
           (replace-regexp-in-string
            "[/\\\\:]" "-"
            (replace-regexp-in-string "\\`[/\\\\]" "" cwd))
           "--")
   root))

(defun ghostel-restore--session-files (cwd &optional root)
  "Return the pi session files created for CWD.
ROOT defaults to `ghostel-restore-pi-session-root'."
  (let ((dir (ghostel-restore--session-dir
              cwd (or root ghostel-restore-pi-session-root))))
    (and (file-directory-p dir)
         (directory-files dir t "\\`[0-9].*\\.jsonl\\'"))))

(defun ghostel-restore--session-name-time (file)
  "Return the creation time encoded in the pi session FILE name, or nil.
pi names sessions TIMESTAMP_ID.jsonl, TIMESTAMP being ISO 8601 in UTC with
colons replaced by dashes."
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

(defun ghostel-restore--session-created-near (cwd start root)
  "Return the pi session file for CWD created at process start time START.
ROOT is the session store.  The file whose name timestamp is closest to
START within the match window wins; nil when nothing is that close."
  (let ((best nil)
        (best-distance nil))
    (dolist (file (ghostel-restore--session-files cwd root))
      (when-let* ((created (ghostel-restore--session-name-time file))
                  (delta (- (float-time created) (float-time start))))
        (when (and (<= delta ghostel-restore-pi-match-window)
                   (>= delta (- ghostel-restore-pi-match-grace))
                   (or (null best-distance) (< (abs delta) best-distance)))
          (setq best file
                best-distance (abs delta)))))
    best))

(defun ghostel-restore--session-file-modtime (file)
  "Return FILE's modification time as a float, or 0."
  (or (ignore-errors
        (float-time (file-attribute-modification-time (file-attributes file))))
      0.0))

(defun ghostel-restore--session-newest (cwd root)
  "Return the most recently written pi session file for CWD under ROOT.
Files already claimed by another terminal during this save are skipped."
  (let ((files (cl-remove-if (lambda (file)
                               (member file ghostel-restore--claimed))
                             (ghostel-restore--session-files cwd root))))
    (car (sort files
               (lambda (a b)
                 (> (ghostel-restore--session-file-modtime a)
                    (ghostel-restore--session-file-modtime b)))))))

(defun ghostel-restore--session-file (pid descendants cwd)
  "Return the pi session file PID (running in CWD) writes to, or nil.
DESCENDANTS are PID's descendants, consulted for an exported
PI_SESSION_FILE, which is exact whenever pi has spawned a process."
  (let ((root (ghostel-restore--session-root pid)))
    (or (cl-loop for child in (cons pid descendants)
                 for exported = (ghostel-restore--proc-env child "PI_SESSION_FILE")
                 when (and exported (file-readable-p exported))
                 return (file-truename exported))
        (let ((start (alist-get 'start (ignore-errors (process-attributes pid)))))
          (and start (ghostel-restore--session-created-near cwd start root)))
        (and ghostel-restore-pi-guess-session
             (ghostel-restore--session-newest cwd root)))))

(defun ghostel-restore--pi-in (buffer)
  "Return a plist describing the pi session running in ghostel BUFFER.
Nil when the terminal runs no pi.  The plist holds `:session' (file or
nil), `:cwd' and `:pid'."
  (let* ((shell (ghostel-restore--terminal-pid buffer))
         (dir (buffer-local-value 'default-directory buffer)))
    (when (and ghostel-restore-pi
               shell
               (not (file-remote-p dir))
               (file-directory-p "/proc"))
      (let* ((descendants (ghostel-restore--descendants shell))
             (pid (cl-find-if #'ghostel-restore--pi-p descendants)))
        (when pid
          (let* ((cwd (or (ghostel-restore--proc-cwd pid) dir))
                 (session (ghostel-restore--session-file pid descendants cwd)))
            (when session
              (cl-pushnew session ghostel-restore--claimed :test #'equal))
            (list :session session :cwd cwd :pid pid)))))))

;; ----------------------------------------------------------------------
;; what is saved
;; ----------------------------------------------------------------------

(defun ghostel-restore--reset-cache ()
  "Forget what the previous save pass claimed and detected."
  (setq ghostel-restore--claimed nil
        ghostel-restore--cache nil))

(defun ghostel-restore--save ()
  "Return the EasySession data for the current ghostel buffer."
  (let* ((name (buffer-name))
         (cached (assoc name ghostel-restore--cache))
         (pi (if cached (cdr cached) (ghostel-restore--pi-in (current-buffer)))))
    (unless cached
      (push (cons name pi) ghostel-restore--cache))
    (append
     (list (cons 'identity ghostel-identity))
     (when (plist-get pi :session)
       (list (cons 'pi (list (cons 'session (plist-get pi :session))
                             (cons 'cwd (plist-get pi :cwd)))))))))

(defun ghostel-restore--command-buffer-p (identity)
  "Return non-nil when IDENTITY belongs to a command buffer.
`ghostel-exec' and `ghostel-compile' buffers hold a program ghostel ran,
not a shell; respawning them would re-run a saved command, so they are
left out of the session, as ghostel's desktop.el support does."
  (or (alist-get 'command identity)
      (eq (alist-get 'kind identity) 'compile)))

(defun ghostel-restore--validate (item)
  "Return non-nil when the terminal described by ITEM should be restored."
  (not (ghostel-restore--command-buffer-p
        (alist-get 'identity (alist-get 'data item)))))

;; ----------------------------------------------------------------------
;; what is restored
;; ----------------------------------------------------------------------

(defun ghostel-restore--existing-dir (dir)
  "Return DIR if it still exists, else its nearest existing ancestor."
  (if (and (stringp dir) (file-directory-p dir))
      dir
    (let ((parent (and (stringp dir)
                       (file-name-directory (directory-file-name dir)))))
      (cond ((and parent (file-directory-p parent)) parent)
            ((file-directory-p "~") (expand-file-name "~"))
            (t default-directory)))))

(defun ghostel-restore--same-dir-p (a b)
  "Return non-nil when A and B name the same existing directory."
  (and (stringp a) (stringp b)
       (file-directory-p a) (file-directory-p b)
       (ignore-errors (file-equal-p a b))))

(defun ghostel-restore--restore (item)
  "Restore the ghostel terminal described by the EasySession ITEM."
  (let* ((data (alist-get 'data item))
         (dir (ghostel-restore--existing-dir (alist-get 'default-directory item)))
         (identity (alist-get 'identity data))
         (pi (alist-get 'pi data))
         (buffer (let ((default-directory dir))
                   (ghostel-create (alist-get 'buffer-name item) nil
                                   (and (consp identity) identity)))))
    (when (and buffer pi)
      (ghostel-restore--resume-pi buffer pi dir))
    buffer))

(defun ghostel-restore--resume-command (session)
  "Return the shell command that resumes pi session SESSION, or nil."
  (and (stringp session)
       (file-readable-p session)
       (format "%s --session %s" ghostel-restore-pi-command
               (shell-quote-argument session))))

(defun ghostel-restore--resume-pi (buffer pi dir)
  "Type pi's resume command into restored terminal BUFFER.
PI is the saved session data; DIR is the directory the shell started in."
  (when-let* ((command (ghostel-restore--resume-command
                        (alist-get 'session pi))))
    ;; pi's working directory is where it was started, which is not
    ;; necessarily where the shell is now (a backgrounded pi, a later cd).
    (let* ((cwd (alist-get 'cwd pi))
           (line (if (and (stringp cwd)
                          (not (ghostel-restore--same-dir-p cwd dir)))
                     (format "cd %s && %s" (shell-quote-argument cwd) command)
                   command)))
      (ghostel-restore--send-later buffer line))))

(defun ghostel-restore--send-later (buffer text)
  "Type TEXT into BUFFER after `ghostel-restore-shell-delay', then Return."
  (run-at-time ghostel-restore-shell-delay nil
               #'ghostel-restore--send-text buffer text)
  (run-at-time (+ ghostel-restore-shell-delay ghostel-restore-return-delay)
               nil #'ghostel-restore--send-return buffer))

(defun ghostel-restore--sendable-p (buffer)
  "Return non-nil when BUFFER is a live ghostel terminal with a terminal."
  (and (buffer-live-p buffer)
       (buffer-local-value 'ghostel--term buffer)
       t))

(defun ghostel-restore--send-text (buffer text)
  "Send TEXT to the shell in BUFFER, when it is still there."
  (when (ghostel-restore--sendable-p buffer)
    (with-current-buffer buffer
      (ghostel-send-string text))))

(defun ghostel-restore--send-return (buffer)
  "Send a Return key to the shell in BUFFER, when it is still there."
  (when (ghostel-restore--sendable-p buffer)
    (with-current-buffer buffer
      (ghostel-send-key "return"))))

;; ----------------------------------------------------------------------
;; entry points
;; ----------------------------------------------------------------------

(defun ghostel-restore--warn-if-too-late ()
  "Warn when the mode is enabled after EasySession has restored the session.
EasySession drops the saved buffers of a managed mode it does not know yet
without a word, so enabling this mode later loses the terminals of the
previous run, silently, until the next restart."
  (when (and (fboundp 'easysession-get-session-name)
             (easysession-get-session-name))
    (display-warning
     'ghostel-restore
     (concat "ghostel-restore was enabled after EasySession had already "
             "restored the session, so the terminals saved for it were "
             "skipped.\nLoad this package during startup (`:demand t', not "
             "`:after') and the next Emacs start will restore them."))))

;;;###autoload
(define-minor-mode ghostel-restore-mode
  "Restore ghostel terminals and their pi sessions across Emacs restarts.
The terminals are registered with EasySession as a managed major mode.

This mode must be enabled while Emacs starts, before EasySession loads the
session on `emacs-startup-hook': a mode registered later is skipped by the
restore handler without a warning being raised there."
  :global t
  :group 'ghostel-restore
  (if ghostel-restore-mode
      (progn
        (easysession-add-managed-major-mode
         'ghostel-mode
         :restore #'ghostel-restore--restore
         :validate #'ghostel-restore--validate
         :save #'ghostel-restore--save)
        (add-hook 'easysession-before-save-hook #'ghostel-restore--reset-cache)
        (ghostel-restore--warn-if-too-late))
    (easysession-remove-managed-major-mode 'ghostel-mode)
    (remove-hook 'easysession-before-save-hook #'ghostel-restore--reset-cache)))

(defun ghostel-restore-describe ()
  "Describe what the current ghostel terminal would restore.
Reports the shell pid, the pi process found below it, the session file
matched to it and the command that would be typed back on restart."
  (interactive)
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "Not a ghostel terminal"))
  (ghostel-restore--reset-cache)
  (let ((shell (ghostel-restore--terminal-pid (current-buffer)))
        (pi (ghostel-restore--pi-in (current-buffer))))
    (message "shell pid %s; pi %s; command %s"
             (or shell "none")
             (if pi (format "pid %s in %s, session %s"
                            (plist-get pi :pid)
                            (plist-get pi :cwd)
                            (or (plist-get pi :session) "none"))
               "none")
             (or (and pi (ghostel-restore--resume-command
                          (plist-get pi :session)))
                 "none"))))

(provide 'ghostel-restore)
;;; ghostel-restore.el ends here
