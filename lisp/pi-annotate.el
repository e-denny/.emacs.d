;;; pi-annotate.el --- Annotate a document for a pi agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Edgar

;; Author: Edgar
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/earendil-works/pi

;;; Commentary:

;; Companion package for the pi `annotate' extension.  `/annotate notes.md'
;; (or notes.org) in pi calls `pi-annotate-open' through `emacsclient --eval';
;; this package opens the reviewed file on the left and its review sidecar
;; (`notes.comments.md' / `notes.comments.org') on the right, at 42% of the
;; frame:
;;
;;   * region in the reviewed file -> `C-c C c' records a comment, and the
;;     cursor moves into the new note so it can be typed at once;
;;   * `C-c C d' marks the region for deletion (press it again on the same
;;     range to remove the mark);
;;   * `C-c C j' jumps between a note and the text it belongs to, `C-c C k'
;;     removes the annotation at the cursor;
;;   * `C-c C q' finishes the review, `C-c C x' aborts it.
;;
;; Commented spans get a yellow background in the reviewed file, deleted spans
;; get strikethrough, and every annotation in the sidecar is set off by a drawn
;; rule above and below it, aligned with the text it refers to.  Alignment and
;; scroll sync both count the screen rows of the reviewed window rather than its
;; line numbers, so a paragraph that soft-wraps still puts its notes on the
;; rows they belong to.  The note itself is plain text, so the cursor in it
;; behaves like any other cursor.  The sidecar stays a plain Markdown/Org file
;; on disk: the rules are text.
;;
;; On finish the sidecar is written and its path is reported back to pi through
;; the result file named by the extension, which then hands the annotations to
;; the agent.  An abort writes "aborted" instead, so nothing is applied and the
;; sidecar is kept for a later run.
;;
;; This package belongs to your Emacs configuration, not to the pi extension:
;; setup, in init.el,
;;
;;   (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
;;   (require 'pi-annotate)
;;
;; and the Emacs server must be running, since that is how pi reaches this
;; instance:
;;
;;   (require 'server)
;;   (unless (server-running-p)
;;     (server-start))

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup pi-annotate nil
  "Annotate a document for a pi agent."
  :group 'tools
  :prefix "pi-annotate-")

(defcustom pi-annotate-split-ratio 0.42
  "Width of the sidecar window as a share of the frame width."
  :type 'number
  :group 'pi-annotate)

(defcustom pi-annotate-min-width 34
  "Minimum width of the sidecar window, in columns."
  :type 'integer
  :group 'pi-annotate)

(defcustom pi-annotate-max-width 90
  "Maximum width of the sidecar window, in columns."
  :type 'integer
  :group 'pi-annotate)

(defcustom pi-annotate-scroll-sync t
  "Whether the reviewed file and the sidecar scroll together.
The sidecar is laid out on the same *screen rows* as the reviewed file --
lines that soft-wrap in the reviewed window take extra rows there and extra
padding lines in the sidecar -- so keeping the two window starts on the same
row keeps every note beside the text it annotates."
  :type 'boolean
  :group 'pi-annotate)

(defcustom pi-annotate-autosave t
  "Whether the sidecar is written as you annotate."
  :type 'boolean
  :group 'pi-annotate)

(defcustom pi-annotate-redraw-idle 0.45
  "How long typing must pause before the review is redrawn, in seconds.
The redraw re-reads the sidecar, re-aligns the rules and writes the file, so
it waits for the user to stop; anything shorter is spent while keys are still
arriving."
  :type 'number
  :group 'pi-annotate)

(defcustom pi-annotate-autoformat t
  "Whether notes are wrapped to the sidecar width.
The wrapping happens on a redraw -- a moment after typing stops -- rather
than on the keystroke that crosses the margin.  Set this to nil to leave
notes exactly as typed."
  :type 'boolean
  :group 'pi-annotate)

(defcustom pi-annotate-prefix-key "C-c C"
  "Prefix key for the annotate commands, as a `kbd' string.
Commands are bound to this prefix plus `c' (comment), `d' (delete), `j'
\(jump), `k' (clear), `q' (finish), `x' (abort) and `g' (redraw).
Changing this after the review buffers exist takes effect on the next
review."
  :type 'string
  :group 'pi-annotate)

;;;; Faces
;;
;; The palette mirrors annotate.nvim: amber comment spans, struck-through grey
;; deletions, and blue/red/yellow rules that brighten for the annotation under
;; the cursor.

(defface pi-annotate-comment
  '((((background dark)) :background "#4c4400" :foreground "#ffe58a")
    (((background light)) :background "#fff2a8" :foreground "#4b3d00")
    (t :inherit highlight))
  "Face for a commented span in the reviewed file."
  :group 'pi-annotate)

(defface pi-annotate-comment-active
  '((((background dark)) :background "#5f5600" :foreground "#fff3b8" :weight bold)
    (((background light)) :background "#ffe066" :foreground "#3d3100" :weight bold)
    (t :inherit highlight))
  "Face for the commented span whose annotation is at the cursor."
  :group 'pi-annotate)

(defface pi-annotate-delete
  '((((background dark)) :foreground "#8c8c8c" :strike-through t)
    (((background light)) :foreground "#a0a0a0" :strike-through t)
    (t :inherit shadow))
  "Face for a span marked for deletion."
  :group 'pi-annotate)

(defface pi-annotate-delete-active
  '((((background dark)) :foreground "#c0c0c0" :strike-through t :weight bold)
    (((background light)) :foreground "#6c6c6c" :strike-through t :weight bold)
    (t :inherit shadow))
  "Face for the deletion span whose annotation is at the cursor."
  :group 'pi-annotate)

(defface pi-annotate-box
  '((((background dark)) :foreground "#7aa2f7")
    (((background light)) :foreground "#1e66f5")
    (t :inherit default))
  "Face for the rules in the sidecar."
  :group 'pi-annotate)

(defface pi-annotate-box-comment
  '((((background dark)) :foreground "#7aa2f7" :weight bold)
    (((background light)) :foreground "#1e66f5" :weight bold)
    (t :inherit default))
  "Face for the rules around a comment."
  :group 'pi-annotate)

(defface pi-annotate-box-delete
  '((((background dark)) :foreground "#f7768e" :weight bold)
    (((background light)) :foreground "#d20f39" :weight bold)
    (t :inherit default))
  "Face for the rules around a deletion."
  :group 'pi-annotate)

(defface pi-annotate-box-active
  '((((background dark)) :foreground "#ffd75f" :weight bold)
    (((background light)) :foreground "#b8860b" :weight bold)
    (t :inherit default))
  "Face for the rules around the annotation at the cursor."
  :group 'pi-annotate)

;;;; Source formats
;;
;; Markdown sidecars hold their markers in HTML comments (invisible in
;; rendered Markdown); Org sidecars use `# annotate:' comment lines.  Parsing
;; accepts both styles whatever the file extension, so a sidecar written by an
;; earlier run is still picked up.

(defconst pi-annotate--rule-char ?\u2500
  "Character that draws the rule around a note.")

(defconst pi-annotate--rule-suffix-regexp
  (format "[ \t]*[%c-]*[ \t]*" pi-annotate--rule-char)
  "Regexp matching the rule a marker line may carry after its text.")

(defconst pi-annotate--marker-regexps
  (list
   (concat "^<!--[ \t]*annotate:#\\([0-9]+\\)[ \t]+"
           "\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\)[ \t]+"
           "\\([a-zA-Z_]+\\)[ \t]*-->"
           pi-annotate--rule-suffix-regexp "$")
   (concat "^#[ \t]*annotate:#\\([0-9]+\\)[ \t]+"
           "\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\)[ \t]+"
           "\\([a-zA-Z_]+\\)"
           pi-annotate--rule-suffix-regexp "$"))
  "Regexps matching an annotation marker line, either comment style.")

(defconst pi-annotate--end-marker-regexps
  (list (concat "^<!--[ \t]*/annotate[ \t]*-->"
                pi-annotate--rule-suffix-regexp "$")
        (concat "^#[ \t]*/annotate" pi-annotate--rule-suffix-regexp "$"))
  "Regexps matching the end marker of a block.")

(defconst pi-annotate--source-regexps
  (list "^<!--[ \t]*annotate:source\\b"
        "^#[ \t]*annotate:source\\b")
  "Regexps matching the sidecar's source header line.")

(defun pi-annotate--org-sidecar-p (path)
  "Return non-nil when PATH's sidecar uses Org comment markers."
  (string-match-p "\\.org\\'" path))

(defun pi-annotate--header-line (style source-name)
  "Return the sidecar header line for STYLE naming SOURCE-NAME."
  (if (eq style 'org)
      (format "# annotate:source %s" source-name)
    (format "<!-- annotate:source %s -->" source-name)))

(defun pi-annotate--rule (text width)
  "Return TEXT followed by enough rules to fill WIDTH columns."
  (let ((pad (- width 1 (string-width text))))
    (if (> pad 0)
        (concat text " " (make-string pad pi-annotate--rule-char))
      text)))

(defun pi-annotate--marker-line (style id kind span width)
  "Return the rule line that opens the block for ID, KIND and SPAN.
SPAN is a list of (START-LINE START-COL END-LINE END-COL); WIDTH is how
many columns the rule should fill."
  (cl-destructuring-bind (sl sc el ec) span
    (pi-annotate--rule
     (if (eq style 'org)
         (format "# annotate:#%d %d:%d-%d:%d %s" id sl sc el ec kind)
       (format "<!-- annotate:#%d %d:%d-%d:%d %s -->" id sl sc el ec kind))
     width)))

(defun pi-annotate--end-marker (style width)
  "Return the rule line that closes a STYLE block, WIDTH columns wide."
  (pi-annotate--rule (if (eq style 'org) "# /annotate" "<!-- /annotate -->")
                     width))

(defun pi-annotate--match-marker (line)
  "Return (ID SL SC EL EC KIND) when LINE is a marker line."
  (cl-some (lambda (regexp)
             (when (string-match regexp line)
               (list (string-to-number (match-string 1 line))
                     (string-to-number (match-string 2 line))
                     (string-to-number (match-string 3 line))
                     (string-to-number (match-string 4 line))
                     (string-to-number (match-string 5 line))
                     (match-string 6 line))))
           pi-annotate--marker-regexps))

(defun pi-annotate--match-end-marker (line)
  "Return non-nil when LINE is an end marker."
  (cl-some (lambda (regexp) (string-match-p regexp line))
           pi-annotate--end-marker-regexps))

(defun pi-annotate--match-source (line)
  "Return non-nil when LINE is a source header."
  (cl-some (lambda (regexp) (string-match-p regexp line))
           pi-annotate--source-regexps))

;;;; State

(cl-defstruct (pi-annotate--block (:constructor pi-annotate--block-create)
                                  (:copier nil))
  id kind
  marker-start marker-end        ; the marker line, excluding its newline
  body-start body-end            ; the note text
  end-start end-end)             ; the end marker line, excluding its newline

(cl-defstruct (pi-annotate--review (:constructor pi-annotate--review-create)
                                    (:copier nil))
  main-path sidecar-path result-file style
  main-buffer sidecar-buffer main-window sidecar-window
  blocks next-id active-id last-id
  anchors                        ; id -> (START-MARKER . END-MARKER) in main
  marks                          ; id -> highlight overlay in main
  decorations                    ; id -> rule/highlight overlays in the sidecar
  rows rows-key rows-total       ; display rows of the reviewed file's lines
  sync-main sync-side            ; window starts the two windows agree on
  sync-main-mark sync-side-mark  ; the same two starts, as markers
  sync-gen                       ; `layout-gen' when that pair was recorded
  layout-gen                     ; bumped whenever the rows may have moved
  window-config timer ended)

(defvar pi-annotate--review nil
  "The review currently open, or nil.")

(defvar pi-annotate--inhibit nil
  "Non-nil while pi-annotate edits buffers itself.
Its own after-change hooks must not treat those edits as user input.")

(defvar pi-annotate--syncing nil
  "Non-nil while the two windows are being scrolled together.")

(defun pi-annotate--active-p ()
  "Return non-nil when a review is open and both its buffers are live."
  (and pi-annotate--review
       (buffer-live-p (pi-annotate--review-main-buffer pi-annotate--review))
       (buffer-live-p (pi-annotate--review-sidecar-buffer pi-annotate--review))))

;;;; Positions and spans

(defun pi-annotate--line-start (buffer line)
  "Return the position at the start of 1-based LINE in BUFFER, or nil."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (zerop (forward-line (1- line)))
        (point)))))

(defun pi-annotate--line-length (buffer line)
  "Return the character length of 1-based LINE in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (let ((start (pi-annotate--line-start buffer line)))
        (when start
          (goto-char start)
          (- (line-end-position) (point)))))))

(defun pi-annotate--pos-of (buffer line col)
  "Return the position of 1-based LINE and COL (1-based, inclusive) in BUFFER."
  (with-current-buffer buffer
    (let ((start (pi-annotate--line-start buffer line)))
      (if (null start)
          (point-max)
        (goto-char start)
        (let ((offset (max 0 (1- col))))
          (min (point-max) (+ start (min offset (- (line-end-position) start)))))))))

(defun pi-annotate--span-of (review id)
  "Return the 1-based SPAN of annotation ID in REVIEW's reviewed file.
The end column is inclusive, matching the marker format."
  (let* ((anchor (cdr (assq id (pi-annotate--review-anchors review))))
         (main (pi-annotate--review-main-buffer review)))
    (when anchor
      (with-current-buffer main
        (save-excursion
          (let* ((start (max (point-min) (min (point-max) (car anchor))))
                 (end (max start (min (point-max) (cdr anchor))))
                 (sl (progn (goto-char start) (line-number-at-pos)))
                 (sc (1+ (- start (line-beginning-position))))
                 (el (progn (goto-char end) (line-number-at-pos)))
                 (ec (- end (line-beginning-position))))
            (when (and (> el sl) (zerop ec))
              ;; the span stops at the very start of a line: record it as
              ;; ending at the end of the previous line, which keeps quotes tidy
              (setq el (1- el))
              (goto-char (pi-annotate--line-start main el))
              (setq ec (- (line-end-position) (point))))
            (list sl sc el (max ec sc))))))))

(defun pi-annotate--ensure-anchor (review id span)
  "Make sure REVIEW has an anchor for ID, creating one from SPAN if needed."
  (unless (assq id (pi-annotate--review-anchors review))
    (let* ((main (pi-annotate--review-main-buffer review))
           (start (pi-annotate--pos-of main (nth 0 span) (nth 1 span)))
           ;; SPAN's end column is inclusive, so the anchor ends one character
           ;; past it, the shape `pi-annotate--span-of' reads back
           (end (pi-annotate--pos-of main (nth 2 span) (1+ (nth 3 span)))))
      (with-current-buffer main
        ;; `copy-marker' marks the current buffer, and `parse' runs in the
        ;; sidecar
        (push (cons id (cons (copy-marker start nil) (copy-marker end t)))
              (pi-annotate--review-anchors review)))))
  (cdr (assq id (pi-annotate--review-anchors review))))

(defun pi-annotate--anchor-region (review id)
  "Return (START . END) buffer positions of annotation ID in REVIEW.
The anchors live in the reviewed file, so clamp them against *that* buffer:
called with the sidecar current, the sidecar's own limits would cut a span
short and the note would be laid out on the wrong row."
  (let ((anchor (cdr (assq id (pi-annotate--review-anchors review)))))
    (when anchor
      (with-current-buffer (pi-annotate--review-main-buffer review)
        (cons (max (point-min) (min (point-max) (car anchor)))
              (max (point-min) (min (point-max) (cdr anchor))))))))

(defun pi-annotate--span-from-region (begin end)
  "Return the inclusive SPAN of the region BEGIN..END in the current buffer."
  (let ((start (min begin end))
        (stop (max begin end)))
    (save-excursion
      (goto-char start)
      (let* ((sl (line-number-at-pos))
             (sc (1+ (- start (line-beginning-position))))
             (el (progn (goto-char stop) (line-number-at-pos)))
             (ec (- stop (line-beginning-position))))
        (when (and (> el sl) (zerop ec))
          (setq el (1- el))
          (goto-char (pi-annotate--line-start (current-buffer) el))
          (setq ec (- (line-end-position) (point))))
        (list sl sc el (max ec 0))))))

;;;; Display rows
;;
;; The sidecar is laid out on the *screen rows* of the reviewed window, not
;; on its line numbers.  A long line soft-wraps in the reviewed window -- Org
;; files are hard-wrapped to 100 columns when they are saved, but a 100-column
;; line still wraps in a narrower window, and a file not yet written since it
;; was wrapped has longer ones -- so that line covers several rows while a
;; line number only names one; counting lines would leave every note below it
;; that many rows too high.  Rows are what the eye compares, so rows are what
;; both the layout and the scroll sync use.  The sidecar itself never wraps
;; (truncation is on and it has no line numbers), so one sidecar line is
;; exactly one row, and sidecar line R+1 sits on row R of the reviewed file --
;; line 1 is the hidden source header.

(defun pi-annotate--rows (review)
  "Return REVIEW's row table, rebuilding it when the reviewed window changed.
Entry I is the display row (0-based) at which line I+1 starts in the main
window, so the table follows the wrapping of the current window width."
  (let* ((main (pi-annotate--review-main-buffer review))
         (window (pi-annotate--review-main-window review))
         (displayed (and (window-live-p window)
                         (eq (window-buffer window) main)))
         (key (and displayed
                   (list (buffer-chars-modified-tick main)
                         (window-body-width window)
                         (window-margins window)
                         (buffer-local-value 'truncate-lines main)
                         (buffer-local-value 'word-wrap main)))))
    (if (and key (equal key (pi-annotate--review-rows-key review)))
        (pi-annotate--review-rows review)
      (with-current-buffer main
        (save-excursion
          (goto-char (point-min))
          (let ((rows nil)
                (row 0))
            (while (not (eobp))
              (push row rows)
              ;; an empty line still takes a row, but `count-screen-lines'
              ;; returns 0 for it
              (setq row (+ row (if displayed
                                   (max 1 (count-screen-lines
                                           (line-beginning-position)
                                           (line-end-position)
                                           nil window))
                                 1)))
              (forward-line 1))
            (setf (pi-annotate--review-rows review) (vconcat (nreverse rows))
                  (pi-annotate--review-rows-total review)
                  (if displayed
                      (count-screen-lines (point-min) (point-max) nil window)
                    (length rows))
                  (pi-annotate--review-rows-key review) key)
            (pi-annotate--review-rows review)))))))

(defun pi-annotate--row-at (review position)
  "Return the display row (0-based) of POSITION in REVIEW's reviewed file."
  (let* ((main (pi-annotate--review-main-buffer review))
         (window (pi-annotate--review-main-window review))
         (rows (pi-annotate--rows review)))
    (with-current-buffer main
      (save-excursion
        (goto-char (min (max position (point-min)) (point-max)))
        (let ((line (min (line-number-at-pos) (length rows))))
          (+ (aref rows (1- line))
             (if (and (window-live-p window)
                      (eq (window-buffer window) main))
                 ;; partial rows: a wrapped line's second row is a position
                 ;; part way along the buffer line
                 (count-screen-lines (line-beginning-position) (point) nil window)
               0)))))))

(defun pi-annotate--line-at-row (review row)
  "Return the number of the line that display ROW (0-based) falls on.
A row in the middle of a soft-wrapped line belongs to that line: the sidecar
knows rows, the reviewed file knows lines, and this is where they meet."
  (let* ((rows (pi-annotate--rows review))
         (count (length rows))
         (row (max 0 row))
         (line 1))
    ;; last line whose first row is at or above ROW
    (let ((low 1)
          (high count))
      (while (<= low high)
        (let ((mid (/ (+ low high) 2)))
          (if (<= (aref rows (1- mid)) row)
              (setq line mid low (1+ mid))
            (setq high (1- mid))))))
    line))

(defun pi-annotate--position-at-row (review row)
  "Return the first position shown on display ROW of REVIEW's reviewed file.
The inverse of `pi-annotate--row-at'.  When ROW falls inside a soft-wrapped
line the answer is a position part way along that line, not the line start:
returning the line start would leave the reviewed window a row or two above the
sidecar row that asked for it, which reads as the text drifting while the
sidecar is the window being scrolled.  Redisplay keeps such a start: it only
overrules one to keep point visible, and a follower is by construction the
window without point in it."
  (let* ((main (pi-annotate--review-main-buffer review))
         (window (pi-annotate--review-main-window review))
         (line (pi-annotate--line-at-row review row))
         (start (pi-annotate--line-position main line)))
    (with-current-buffer main
      (save-excursion
        (goto-char start)
        (let ((want (- row (pi-annotate--row-at review start)))
              (low start)
              (high (line-end-position))
              (found start))
          (if (or (<= want 0)
                  (not (window-live-p window))
                  (not (eq (window-buffer window) main)))
              start
            ;; one row's characters are contiguous, so the first position whose
            ;; row offset reaches WANT is where that row begins
            (while (<= low high)
              (let ((mid (/ (+ low high) 2)))
                (if (>= (count-screen-lines start mid nil window) want)
                    (setq found mid high (1- mid))
                  (setq low (1+ mid)))))
            found))))))

(defun pi-annotate--block-start-row (review block)
  "Return the display row where BLOCK's span begins, or nil."
  (let ((region (pi-annotate--anchor-region review (pi-annotate--block-id block))))
    (and region (pi-annotate--row-at review (car region)))))

;;;; Parsing the sidecar

(defun pi-annotate--parse (review)
  "Re-read REVIEW's sidecar and rebuild its block list."
  (let* ((buffer (pi-annotate--review-sidecar-buffer review))
         (blocks nil)
         (max-id 0)
         (paragraphs nil))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (text (buffer-substring-no-properties line-start line-end))
                 (match (pi-annotate--match-marker text)))
            (if (null match)
                (forward-line 1)
              (cl-destructuring-bind (id sl sc el ec kind) match
                ;; the body runs until the block's end marker
                (let ((body-start (min (point-max) (1+ line-end)))
                      (end-start nil)
                      (end-end nil))
                  (save-excursion
                    (forward-line 1)
                    (catch 'found
                      (while (not (eobp))
                        (let ((candidate-start (line-beginning-position))
                              (candidate-end (line-end-position)))
                          (when (pi-annotate--match-end-marker
                                 (buffer-substring-no-properties candidate-start candidate-end))
                            (setq end-start candidate-start
                                  end-end candidate-end)
                            (throw 'found t)))
                        (forward-line 1))))
                  (let ((body-end (or end-start (point-max))))
                    (unless (= body-start body-end)
                      (setq body-end (1- body-end))) ; drop the last newline
                    (push (pi-annotate--block-create
                           :id id :kind kind
                           :marker-start (copy-marker line-start)
                           :marker-end (copy-marker line-end)
                           :body-start (copy-marker body-start)
                           :body-end (copy-marker body-end)
                           :end-start (and end-start (copy-marker end-start))
                           :end-end (and end-end (copy-marker end-end t)))
                          blocks)
                    (push (cons id (list sl sc el ec)) paragraphs)
                    (setq max-id (max max-id id))
                    ;; continue after the block
                    (goto-char (or (and end-end (min (point-max) (1+ end-end)))
                                   (point-max))))))))
          ))
      (setf (pi-annotate--review-blocks review) (nreverse blocks)
            (pi-annotate--review-next-id review) (1+ max-id))
      ;; anchors: keep the live markers of annotations we already track, and
      ;; create markers for the ones read back from disk
      (dolist (entry paragraphs)
        (pi-annotate--ensure-anchor review (car entry) (cdr entry)))
      (let ((known (mapcar #'car paragraphs)))
        (setf (pi-annotate--review-anchors review)
              (cl-remove-if-not (lambda (entry) (memq (car entry) known))
                                (pi-annotate--review-anchors review))))
      blocks)))

(defun pi-annotate--block-at-point (review)
  "Return the block of REVIEW that point belongs to, or nil."
  (let ((in-main (eq (current-buffer) (pi-annotate--review-main-buffer review)))
        (pos (point))
        found)
    (dolist (block (pi-annotate--review-blocks review))
      (let ((region (pi-annotate--anchor-region review (pi-annotate--block-id block))))
        (when (if in-main
                  (and region (>= pos (car region)) (< pos (cdr region)))
                (and (<= (marker-position (pi-annotate--block-marker-start block)) pos)
                     (<= pos (if (pi-annotate--block-end-end block)
                                 (marker-position (pi-annotate--block-end-end block))
                               (marker-position (pi-annotate--block-body-end block))))))
          (setq found block))))
    found))

(defun pi-annotate--body-lines (block)
  "Return the note text lines of BLOCK as a list of strings."
  (let ((start (marker-position (pi-annotate--block-body-start block)))
        (end (marker-position (pi-annotate--block-body-end block))))
    (when (and start end (<= start end))
      (let ((text (buffer-substring-no-properties start end)))
        (if (string-empty-p text)
            (list "")
          (split-string text "\n"))))))

;;;; Decoration

(defun pi-annotate--rule-width (review)
  "Return the width, in columns, of REVIEW's rule lines."
  (let ((win (pi-annotate--review-sidecar-window review)))
    (max 24 (1- (if (window-live-p win) (window-body-width win) 80)))))

(defun pi-annotate--text-width (review)
  "Return the width available for note text in REVIEW."
  (max 20 (- (pi-annotate--rule-width review) 2)))

(defun pi-annotate--rule-face (review id kind)
  "Return the rule face for annotation ID of KIND in REVIEW."
  (cond
   ((eql id (pi-annotate--review-active-id review)) 'pi-annotate-box-active)
   ((string= kind "delete") 'pi-annotate-box-delete)
   (t 'pi-annotate-box-comment)))

(defun pi-annotate--clear-decorations (review)
  "Remove every decoration overlay REVIEW owns."
  (dolist (entry (pi-annotate--review-decorations review))
    (dolist (overlay (cdr entry))
      (when (overlayp overlay) (delete-overlay overlay))))
  (setf (pi-annotate--review-decorations review) nil))

(defun pi-annotate--deco-same (a b)
  "Return non-nil when decorations A and B draw the same thing."
  (and (= (length a) (length b))
       (cl-every #'equal a b)))

(defun pi-annotate--deco-overlays (previous specs)
  "Return overlays for SPECS, reusing those in PREVIOUS that still match.
SPECS is a list of (START END DISPLAY FACE BEFORE-STRING).  Touching an
overlay makes redisplay repaint its window from scratch, so an overlay whose
position and decorations are unchanged is kept as is."
  (let ((out nil))
    (dolist (spec specs)
      (cl-destructuring-bind (start end display face before) spec
        (let* ((wanted (list display face before))
               (hit (cl-find-if (lambda (overlay)
                                  (and (overlayp overlay)
                                       (eql (overlay-start overlay) start)
                                       (eql (overlay-end overlay) end)
                                       (pi-annotate--deco-same
                                        (overlay-get overlay 'pi-annotate-deco)
                                        wanted)))
                                previous)))
          (if hit
              (progn
                ;; one overlay per spec: a second identical spec must make its
                ;; own overlay rather than share this one
                (setq previous (delq hit previous))
                (push hit out))
            (let ((overlay (make-overlay start end)))
              (overlay-put overlay 'pi-annotate-deco wanted)
              (when display (overlay-put overlay 'display display))
              (when face (overlay-put overlay 'face face))
              (when before (overlay-put overlay 'before-string before))
              (push overlay out))))))
    (nreverse out)))

(defun pi-annotate--deco-specs (review block)
  "Return the overlay specs that dress BLOCK of REVIEW.
They colour the rule lines that open and close the note; the note text
itself is left as plain text, so the cursor in it is an ordinary cursor.
Each spec is (START END DISPLAY FACE BEFORE-STRING), the form
`pi-annotate--deco-overlays' reads."
  (let ((face (pi-annotate--rule-face review (pi-annotate--block-id block)
                                        (pi-annotate--block-kind block)))
        (specs nil))
    (push (list (marker-position (pi-annotate--block-marker-start block))
                (marker-position (pi-annotate--block-marker-end block))
                nil face nil)
          specs)
    (when (pi-annotate--block-end-start block)
      (push (list (marker-position (pi-annotate--block-end-start block))
                  (marker-position (pi-annotate--block-end-end block))
                  nil face nil)
            specs))
    (nreverse specs)))

(defun pi-annotate--render-decorations (review)
  "Hide the sidecar's plumbing and colour every annotation's rules.
Overlays that already match are left alone; only a rule whose position or
face actually changed is rebuilt."
  (let* ((buffer (pi-annotate--review-sidecar-buffer review))
         (old (pi-annotate--review-decorations review))
         (groups nil)                   ; (KEY . SPECS) for this redraw
         (overlays nil)
         (kept nil))
    (with-current-buffer buffer
      (save-excursion
        ;; the source header is plumbing: keep it out of sight
        (goto-char (point-min))
        (when (pi-annotate--match-source
               (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position)))
          (push (cons 'source
                      (list (list (line-beginning-position) (line-end-position)
                                  "" nil nil)))
                groups)))
      (dolist (block (pi-annotate--review-blocks review))
        (push (cons (pi-annotate--block-id block)
                    (pi-annotate--deco-specs review block))
              groups)))
    (with-current-buffer buffer
      (dolist (group groups)
        (let ((made (pi-annotate--deco-overlays (cdr (assq (car group) old))
                                                (cdr group))))
          (setq kept (nconc made kept))
          (push (cons (car group) made) overlays))))
    ;; drop overlays no block claims any more
    (dolist (entry old)
      (dolist (overlay (cdr entry))
        (when (and (overlayp overlay) (not (memq overlay kept)))
          (delete-overlay overlay))))
    (setf (pi-annotate--review-decorations review) (nreverse overlays))))

(defun pi-annotate--render (review)
  "Redraw REVIEW: re-read the sidecar, then highlight and dress every block."
  (when (pi-annotate--active-p)
    (pi-annotate--parse review)
    (let ((main (pi-annotate--review-main-buffer review))
          (marks nil))
      (with-current-buffer main
        (dolist (block (pi-annotate--review-blocks review))
          (let* ((id (pi-annotate--block-id block))
                 (kind (pi-annotate--block-kind block))
                 (active (eql id (pi-annotate--review-active-id review)))
                 (face (cond ((string= kind "delete")
                              (if active 'pi-annotate-delete-active 'pi-annotate-delete))
                             (active 'pi-annotate-comment-active)
                             (t 'pi-annotate-comment)))
                 (region (pi-annotate--anchor-region review id)))
            (when (and region (< (car region) (cdr region)))
              (let ((overlay (cdr (assq id (pi-annotate--review-marks review)))))
                (if (overlayp overlay)
                    (progn
                      (move-overlay overlay (car region) (cdr region))
                      (overlay-put overlay 'face face))
                  (setq overlay (make-overlay (car region) (cdr region) nil nil t))
                  (overlay-put overlay 'face face)
                  (overlay-put overlay 'pi-annotate-id id))
                (push (cons id overlay) marks)))))
        ;; drop highlights for annotations that are gone
        (let ((kept (mapcar #'car marks)))
          (dolist (entry (pi-annotate--review-marks review))
            (unless (memq (car entry) kept)
              (when (overlayp (cdr entry)) (delete-overlay (cdr entry))))))
        (setf (pi-annotate--review-marks review) marks)))
    (pi-annotate--render-decorations review)))

;;;; Wrapping

(defun pi-annotate--wrap (text width prefix)
  "Wrap TEXT to WIDTH columns, starting each line with PREFIX.
Greedy, by display width, and PREFIX goes on every line — the same shape as
annotate.nvim's wrapping, which is why a single unbreakable token longer than
the line can still cross it."
  (let ((words (split-string (or text "") "[ \t\n]+" t))
        (prefix (or prefix ""))
        (lines nil)
        (current ""))
    (dolist (word words)
      (cond
       ((string-empty-p current) (setq current word))
       ((<= (+ (string-width prefix) (string-width current) 1 (string-width word)) width)
        (setq current (concat current " " word)))
       (t
        (push (concat prefix current) lines)
        (setq current word))))
    (push (concat prefix current) lines)
    (nreverse lines)))

(defun pi-annotate--reflow-block (review block)
  "Re-wrap BLOCK of REVIEW when its text is wider than the sidecar.
Return non-nil when the buffer text changed."
  (let* ((width (pi-annotate--text-width review))
         (lines (pi-annotate--body-lines block))
         (kind (pi-annotate--block-kind block))
         (wrapped nil)
         (changed nil))
    (cond
     ((string= kind "delete")
      ;; a deletion quotes one source line per buffer line: wrap each quote on
      ;; its own, keeping the "> " that marks it
      (dolist (line lines)
        (if (string-match "\\`\\(>[ \t]?\\)\\(.*\\)\\'" line)
            (dolist (piece (pi-annotate--wrap (match-string 2 line) width (match-string 1 line)))
              (push piece wrapped))
          (push line wrapped)))
      ;; the pushes above run newest-first
      (setq wrapped (nreverse wrapped)))
     ;; notes that already fit keep the breaks their author chose, and blocks
     ;; that look like markdown/org structure are left alone
     ((and (cl-some (lambda (line) (> (string-width line) width)) lines)
           (not (cl-some (lambda (line)
                           (string-match-p "\\`[ \t]*\\(?:>\\|#\\|[-*+] \\|[0-9]+[.)] \\||\\)"
                                           line))
                         lines))
           (not (string-empty-p (string-trim (mapconcat #'identity lines " ")))))
      (let* ((text (mapconcat #'identity lines " "))
             ;; a space typed at the end of the note is not padding: dropping
             ;; it would glue the next word typed to the last one
             (trailing (and (string-match "[ \t\n]+\\'" text) (match-string 0 text))))
        (setq wrapped (pi-annotate--wrap
                       (string-trim (replace-regexp-in-string "[ \t\n]+" " " text))
                       width nil))
        (when (and trailing wrapped)
          (setcar (last wrapped) (concat (car (last wrapped)) trailing)))))
     (t (setq wrapped lines)))
    (when (and wrapped (not (equal wrapped lines)))
      (let ((start (marker-position (pi-annotate--block-body-start block)))
            (end (marker-position (pi-annotate--block-body-end block))))
        (when (and start end)
          (let* ((pi-annotate--inhibit t)
                 (buffer (current-buffer))
                 (window (pi-annotate--review-sidecar-window review))
                 (displayed (and (window-live-p window)
                                 (eq (window-buffer window) buffer)))
                 (here (if displayed (window-point window) (point)))
                 (inside (and (<= start here) (<= here end)))
                 ;; wrapping only moves whitespace around, so the number of
                 ;; non-space characters before point survives the rewrite
                 (consumed (and inside
                                (length (replace-regexp-in-string
                                         "[ \t\n]+" ""
                                         (buffer-substring-no-properties start here)))))
                 (total (and inside
                             (length (replace-regexp-in-string
                                      "[ \t\n]+" ""
                                      (buffer-substring-no-properties start end)))))
                 (stop nil))
            (save-excursion
              (goto-char start)
              (delete-region start end)
              (insert (mapconcat #'identity wrapped "\n"))
              (setq stop (point)))
            ;; rewriting the note must not move the cursor: the next typed
            ;; character would land somewhere else than where it is shown
            (when inside
              (if (>= consumed total)
                  ;; point was at the end of the note (or inside the
                  ;; whitespace after its last word): keep it at the end
                  (goto-char stop)
                (goto-char start)
                (let ((seen 0))
                  (while (and (< (point) stop) (< seen consumed))
                    (unless (memq (char-after) '(?\s ?\t ?\n))
                      (setq seen (1+ seen)))
                    (forward-char 1))))
              (when displayed
                (set-window-point window (point))))
            (setq changed t)))))
    changed))

(defun pi-annotate--reflow (review)
  "Re-wrap every overlong note in REVIEW.  Return non-nil when text changed."
  (let ((changed nil))
    (with-current-buffer (pi-annotate--review-sidecar-buffer review)
      (setq-local fill-column (pi-annotate--text-width review))
      (when pi-annotate-autoformat
        ;; later blocks first, so rewrapping one does not invalidate the
        ;; positions of the blocks before it
        (dolist (block (reverse (copy-sequence (pi-annotate--review-blocks review))))
          (when (pi-annotate--reflow-block review block)
            (setq changed t)))))
    changed))

;;;; Layout

(defun pi-annotate--desired-lines (review)
  "Return REVIEW's sidecar as a vector of lines, padded for alignment.
Each note opens on the display row of the text it annotates, so notes stay
beside their text even where the reviewed file soft-wraps.  The tail is padded
to the height of the reviewed file and to a screenful past the last note, so
the windows keep scrolling together while there is still a note to show."
  (pi-annotate--rows review)
  (let* ((style (pi-annotate--review-style review))
         (width (pi-annotate--rule-width review))
         (header (pi-annotate--header-line
                  style
                  (file-name-nondirectory (pi-annotate--review-main-path review))))
         (end-marker (pi-annotate--end-marker style width))
         (blocks (seq-sort
                  (lambda (a b)
                    (< (or (pi-annotate--block-start-row review a) 0)
                       (or (pi-annotate--block-start-row review b) 0)))
                  (copy-sequence (pi-annotate--review-blocks review))))
         ;; line 1 holds the (hidden) source header, so sidecar line N sits on
         ;; row N-1 of the reviewed file and the notes start counting at 1
         (lines (list header))
         (count 1))
    ;; `pi-annotate--body-lines' reads the sidecar's markers, so it must run
    ;; with the sidecar current
    (with-current-buffer (pi-annotate--review-sidecar-buffer review)
      (dolist (block blocks)
        (let ((id (pi-annotate--block-id block))
              (span (pi-annotate--span-of review (pi-annotate--block-id block)))
              (target (pi-annotate--block-start-row review block)))
          (when (and span target)
            (while (< count target)
              (push "" lines)
              (setq count (1+ count)))
            (push (pi-annotate--marker-line style id (pi-annotate--block-kind block)
                                            span width)
                  lines)
            (setq count (1+ count))
            (dolist (line (pi-annotate--body-lines block))
              (push line lines)
              (setq count (1+ count)))
            (push end-marker lines)
            (setq count (1+ count))
            (push "" lines)
            (setq count (1+ count))))))
    ;; Keep the sidecar as tall as the reviewed file, but no taller than a
    ;; screen past the last note: a file reviewed only at its top would
    ;; otherwise pad the sidecar with thousands of lines that no note will
    ;; ever sit beside, and every edit would rewrite them.
    (let* ((window (pi-annotate--review-sidecar-window review))
           (height (if (window-live-p window) (window-body-height window) 40))
           (limit (max 1 (min (pi-annotate--review-rows-total review)
                              (+ count height)))))
      (while (< count limit)
        (push "" lines)
        (setq count (1+ count))))
    (vconcat (nreverse lines))))

(defun pi-annotate--buffer-lines (buffer)
  "Return BUFFER's contents as a vector of lines."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((lines nil))
        (while (not (eobp))
          (push (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                lines)
          (forward-line 1))
        (vconcat (nreverse lines))))))

(defun pi-annotate--line-position (buffer line)
  "Return the start position of 1-based LINE in BUFFER."
  (or (pi-annotate--line-start buffer line)
      (with-current-buffer buffer (point-max))))

(defun pi-annotate--line-count (buffer)
  "Return the number of lines BUFFER can put a window start on."
  (with-current-buffer buffer
    (line-number-at-pos (point-max))))

(defun pi-annotate--apply-lines (review desired)
  "Make REVIEW's sidecar equal DESIRED, touching only what differs.
DESIRED is a vector of lines; return non-nil when the buffer changed."
  (let* ((buffer (pi-annotate--review-sidecar-buffer review))
         (current (pi-annotate--buffer-lines buffer))
         (n (length current))
         (m (length desired))
         (prefix 0)
         (suffix 0))
    (while (and (< prefix n) (< prefix m)
                (string= (aref current prefix) (aref desired prefix)))
      (setq prefix (1+ prefix)))
    (while (and (< suffix (- n prefix)) (< suffix (- m prefix))
                (string= (aref current (- n 1 suffix))
                         (aref desired (- m 1 suffix))))
      (setq suffix (1+ suffix)))
    (if (and (= prefix n) (= prefix m))
        nil
      (let* ((replacement (cl-subseq desired prefix (- m suffix)))
             (text (if replacement
                       (concat (mapconcat #'identity replacement "\n") "\n")
                     "")))
        (with-current-buffer buffer
          (let* (;; where the cursor is, so the rewrite below can put it back:
                 ;; deleting the region it sits in would otherwise park point
                 ;; at the region's start, i.e. at the top of the sidecar
                 (window (pi-annotate--review-sidecar-window review))
                 (displayed (and (window-live-p window)
                                 (eq (window-buffer window) buffer)))
                 (here (if displayed (window-point window) (point)))
                 (start (or (pi-annotate--line-start buffer (1+ prefix))
                            (point-max)))
                 (last-line (- n suffix))
                 (inside (and (<= start here)
                              (<= here (or (pi-annotate--line-start
                                            buffer (1+ last-line))
                                           (point-max)))))
                 (line-offset (and inside (save-excursion
                                            (goto-char here)
                                            (- (line-number-at-pos) prefix))))
                 (column (and inside (save-excursion
                                       (goto-char here)
                                       (current-column))))
                 (pi-annotate--inhibit t))
            (save-excursion
              ;; the common head and tail are left alone, so point and the undo
              ;; history survive a redraw that only moves padding around
              (when (> last-line prefix)
                (goto-char (pi-annotate--line-start buffer last-line))
                (delete-region start (min (point-max) (1+ (line-end-position)))))
              (goto-char start)
              (insert text))
            (when inside
              (goto-char start)
              (forward-line (max 0 (min (1- (max 1 (length replacement)))
                                        (1- line-offset))))
              (move-to-column column)
              (when displayed
                (set-window-point window (point))))))
        (pi-annotate--parse review)
        t))))

(defun pi-annotate--sync-markers (review)
  "Refresh REVIEW's sidecar marker lines from the live anchors.
Return non-nil when any marker line changed."
  (let ((changed nil)
        (style (pi-annotate--review-style review))
        (width (pi-annotate--rule-width review)))
    (with-current-buffer (pi-annotate--review-sidecar-buffer review)
      (dolist (block (pi-annotate--review-blocks review))
        (let* ((id (pi-annotate--block-id block))
               (span (pi-annotate--span-of review id))
               (want (and span (pi-annotate--marker-line
                                style
                                id (pi-annotate--block-kind block) span width)))
               (start (marker-position (pi-annotate--block-marker-start block)))
               (end (marker-position (pi-annotate--block-marker-end block))))
          (when (and want start end)
            (let ((have (buffer-substring-no-properties start end)))
              (unless (string= want have)
                (let ((pi-annotate--inhibit t))
                  (save-excursion
                    (goto-char start)
                    (delete-region start end)
                    (insert want)))
                (setq changed t)))))))
    changed))

(defun pi-annotate--relayout (review)
  "Re-pad REVIEW's sidecar so each annotation sits on its text's line."
  (when (and (pi-annotate--active-p)
             (not (pi-annotate--review-ended review)))
    (pi-annotate--parse review)
    (pi-annotate--sync-markers review)
    (let ((desired (pi-annotate--desired-lines review)))
      (pi-annotate--apply-lines review desired))))

;;;; Saving

(defun pi-annotate--written-end (buffer)
  "Return the position just past BUFFER's last non-blank line.
The blank lines after it only pad the buffer out to the height of the
reviewed file, so the two windows scroll together; they are not part of the
sidecar on disk."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (let ((end nil))
        (while (and (null end) (> (point) (point-min)))
          (forward-line -1)
          (unless (looking-at-p "[ \t]*$")
            (setq end (min (point-max) (1+ (line-end-position))))))
        (or end (point-min))))))

(defun pi-annotate--save (review)
  "Write REVIEW's sidecar (and its reviewed file when it is modified)."
  (let ((sidecar (pi-annotate--review-sidecar-buffer review))
        (main (pi-annotate--review-main-buffer review)))
    (when (buffer-live-p sidecar)
      (with-current-buffer sidecar
        (when (buffer-modified-p)
          (write-region (point-min) (pi-annotate--written-end sidecar)
                        (pi-annotate--review-sidecar-path review) nil 'silent)
          ;; the buffer keeps its padding while the file does not, so Emacs
          ;; must be told the file it visits is the one just written
          (set-visited-file-modtime)
          (set-buffer-modified-p nil))))
    (when (and (buffer-live-p main)
               (buffer-modified-p (get-buffer main)))
      (with-current-buffer main
        (when buffer-file-name
          (condition-case err
              (save-buffer)
            (error (message "pi-annotate: could not save %s: %s"
                            buffer-file-name (error-message-string err)))))))))

;;;; Window layout

(defun pi-annotate--sidecar-width ()
  "Return the width the sidecar window should have."
  (let* ((frame-width (frame-width))
         (wanted (round (* frame-width pi-annotate-split-ratio))))
    (max pi-annotate-min-width (min pi-annotate-max-width wanted))))

(defun pi-annotate--setup-windows (review)
  "Give the frame to REVIEW: reviewed file left, sidecar right.
The previous window layout is remembered and put back when the review ends."
  (let* ((main (pi-annotate--review-main-buffer review))
         (sidecar (pi-annotate--review-sidecar-buffer review))
         (main-window (selected-window))
         (sidecar-window nil))
    (setf (pi-annotate--review-window-config review) (current-window-configuration))
    (delete-other-windows main-window)
    (set-window-buffer main-window main)
    ;; a negative SIZE gives the *new* (right-hand) window that many columns
    (setq sidecar-window (split-window-right (- (pi-annotate--sidecar-width))))
    (set-window-buffer sidecar-window sidecar)
    (set-window-parameter main-window 'pi-annotate t)
    (set-window-parameter sidecar-window 'pi-annotate t)
    (setf (pi-annotate--review-main-window review) main-window
          (pi-annotate--review-sidecar-window review) sidecar-window)
    (set-window-margins sidecar-window 0 0)
    (with-current-buffer sidecar
      (setq-local truncate-lines t)
      (setq-local word-wrap nil)
      (setq-local display-line-numbers nil)
      (display-line-numbers-mode -1)
      ;; The sidecar inherits its file's major mode -- a `.comments.org'
      ;; sidecar opens in Org -- so it inherits whatever that mode does to
      ;; prose as well.  A wrapping or centring mode fights the fixed line
      ;; layout below: with `visual-line-mode' or `visual-fill-column-mode' on,
      ;; one note takes several screen rows and the rules beside it no longer
      ;; line up with the text they annotate.  Nothing in this configuration
      ;; turns either on any more (Org is hard-wrapped when it is saved, not
      ;; wrapped on screen), so these two calls are a guard against a global
      ;; mode or a hook added later rather than a fix for a known setting.
      (when (fboundp 'visual-line-mode) (visual-line-mode -1))
      (when (fboundp 'visual-fill-column-mode) (visual-fill-column-mode -1))
      ;; The sidecar is laid out one note per screen row, so each note must
      ;; stay one screen line: overlong notes are wrapped into real lines by
      ;; `pi-annotate--reflow' once typing stops, not soft-wrapped by
      ;; redisplay.  No `auto-fill-mode' either: it would insert fill breaks
      ;; on its own schedule, in the middle of a note.  The Org save hook
      ;; cannot refill the notes either: the sidecar is written with
      ;; `write-region' and never passes through `save-buffer', and the width
      ;; kept here is `pi-annotate--text-width'.
      (setq-local fill-column (pi-annotate--text-width review)))
    (with-current-buffer main
      (setq-local truncate-lines nil))
    (select-window main-window)
    (set-window-start main-window (point-min) t)
    (set-window-start sidecar-window (point-min) t)
    (set-window-hscroll sidecar-window 0)
    (set-window-hscroll main-window 0)
    (pi-annotate--record-sync review)
    (add-hook 'window-size-change-functions #'pi-annotate--window-size-changed)))

(defun pi-annotate--line-number (buffer position)
  "Return the 1-based line number of POSITION in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (min (max position (point-min)) (point-max)))
      (line-number-at-pos))))

(defun pi-annotate--record-sync (review)
  "Remember the window starts REVIEW's two windows currently show.
The pair is kept twice: as the plain positions `pi-annotate--scroll-all'
compares, and as markers.  The markers are what the next sync measures the
scroll step against -- they follow an edit, so a note typed into one buffer
does not leave the pair pointing at text that has moved."
  (let ((main (pi-annotate--review-main-window review))
        (sidecar (pi-annotate--review-sidecar-window review)))
    (when (and (window-live-p main) (window-live-p sidecar))
      (let ((main-start (window-start main))
            (side-start (window-start sidecar)))
        (setf (pi-annotate--review-sync-main review) main-start
              (pi-annotate--review-sync-side review) side-start
              (pi-annotate--review-sync-main-mark review)
              (with-current-buffer (window-buffer main)
                (copy-marker main-start))
              (pi-annotate--review-sync-side-mark review)
              (with-current-buffer (window-buffer sidecar)
                (copy-marker side-start))
              (pi-annotate--review-sync-gen review)
              (pi-annotate--review-layout-gen review))))))

(defun pi-annotate--row-delta (review from to)
  "Return the display rows between positions FROM and TO in REVIEW.
Negative when TO is above FROM.  Only the span between the two positions is
measured, so a scroll step costs what the step spans: `count-screen-lines'
over a whole 1600-line buffer costs tens of milliseconds in a live window, and
that is the cost the row table used to charge on every scroll."
  (let ((main (pi-annotate--review-main-buffer review))
        (window (pi-annotate--review-main-window review)))
    (cond
     ((or (not (number-or-marker-p from))
          (not (number-or-marker-p to))
          (= from to))
      0)
     (t
      (with-current-buffer main
        (let ((window (and (window-live-p window) window))
              (forward (< from to)))
          (* (if forward 1 -1)
             (count-screen-lines (if forward from to)
                                 (if forward to from)
                                 nil window))))))))

(defun pi-annotate--position-after-rows (review position rows)
  "Return the position ROWS display rows away from POSITION in REVIEW.
Negative ROWS moves up.  The reviewed window's own geometry is what counts, so
the answer stays right where the reviewed file soft-wraps."
  (let ((main (pi-annotate--review-main-buffer review))
        (window (pi-annotate--review-main-window review)))
    (with-current-buffer main
      (save-excursion
        (goto-char (min (max position (point-min)) (point-max)))
        (vertical-motion rows (and (window-live-p window) window))
        (point)))))

(defun pi-annotate--row-gap (review window from to)
  "Return the display rows between FROM and TO as WINDOW of REVIEW shows them.
The sidecar is one row per line, so nothing has to be measured there; the
reviewed file is measured only across the span between the two positions,
which for a scroll is one screenful."
  (if (eq (window-buffer window) (pi-annotate--review-main-buffer review))
      (pi-annotate--row-delta review from to)
    (- (pi-annotate--line-number (window-buffer window) to)
       (pi-annotate--line-number (window-buffer window) from))))

(defun pi-annotate--may-scroll-p (review window target)
  "Return non-nil when REVIEW's WINDOW can be scrolled to TARGET safely.
The window the user is working in is only moved while its point would stay on
screen: scrolling it away from the cursor would take the note being typed out
of view, and the next keystroke would land on a line the user cannot see.
Point and target are both measured from the window's own start, so the test
needs no absolute rows and no row table."
  (or (not (eq window (selected-window)))
      (let* ((height (window-body-height window))
             (start (window-start window))
             (point-gap (pi-annotate--row-gap review window start
                                            (window-point window)))
             (target-gap (pi-annotate--row-gap review window start target)))
        (and (<= target-gap point-gap)
             (< point-gap (+ target-gap height))))))

(defun pi-annotate--set-start (window position)
  "Make WINDOW start at POSITION.
NOFORCE stays nil so redisplay honours POSITION: passed non-nil, it lets
redisplay disregard it and pick a start that keeps point visible, and the
follower windows would then never move.  A point that ends up outside the new
range is moved into view by redisplay itself."
  (unless (eql (window-start window) position)
    (set-window-start window position)))

(defun pi-annotate--sync-current-p (review)
  "Return non-nil when the recorded sync pair can still be measured from.
The pair is a position in each buffer plus the layout generation it was taken
in.  An edit or a resize moves the rows under those positions, and a stale
pair would then apply a delta measured in rows that no longer exist."
  (and (markerp (pi-annotate--review-sync-main-mark review))
       (marker-position (pi-annotate--review-sync-main-mark review))
       (markerp (pi-annotate--review-sync-side-mark review))
       (marker-position (pi-annotate--review-sync-side-mark review))
       (eql (pi-annotate--review-sync-gen review)
            (pi-annotate--review-layout-gen review))))

(defun pi-annotate--follow-main (review)
  "Put REVIEW's sidecar window on the row its reviewed window starts on.
The step is taken as a delta from the pair the last sync agreed on rather than
as an absolute row: one sidecar line is one row of the reviewed file, so the
sidecar moves by the rows the reviewed window moved by.  A delta is also the
honest answer while the padding is stale, which is the whole interval between
a change and the redraw that re-pads the sidecar."
  (let ((main (pi-annotate--review-main-window review))
        (sidecar (pi-annotate--review-sidecar-window review)))
    (let* ((buffer (window-buffer sidecar))
           (recorded-main (pi-annotate--review-sync-main-mark review))
           (recorded-side (pi-annotate--review-sync-side-mark review))
           ;; the sidecar is padded to the height of the reviewed file, but a
           ;; stale one may still be shorter: stop at its last line
           (line (max 1 (min (if (pi-annotate--sync-current-p review)
                                 (+ (pi-annotate--line-number
                                     buffer (marker-position recorded-side))
                                    (pi-annotate--row-delta
                                     review recorded-main (window-start main)))
                               ;; no pair to measure from: the first sync of a
                               ;; review, or the first one after an edit or a
                               ;; resize, where the row table is stale anyway
                               ;; and has to be rebuilt sooner or later
                               (1+ (pi-annotate--row-at
                                   review (window-start main))))
                           (pi-annotate--line-count buffer))))
           (target (pi-annotate--line-position buffer line)))
      (when (pi-annotate--may-scroll-p review sidecar target)
        (pi-annotate--set-start sidecar target)
        t))))

(defun pi-annotate--follow-sidecar (review)
  "Put REVIEW's reviewed window on the row its sidecar window starts on.
The mirror of `pi-annotate--follow-main': the reviewed window moves by as many
display rows as the sidecar moved lines, measured with `vertical-motion' from
the position the last sync agreed on."
  (let ((main (pi-annotate--review-main-window review))
        (sidecar (pi-annotate--review-sidecar-window review)))
    (let* ((buffer (window-buffer sidecar))
           (line (pi-annotate--line-number buffer (window-start sidecar)))
           (recorded-main (pi-annotate--review-sync-main-mark review))
           (recorded-side (pi-annotate--review-sync-side-mark review))
           (target
            (if (pi-annotate--sync-current-p review)
                (pi-annotate--position-after-rows
                 review recorded-main
                 (- line (pi-annotate--line-number
                          buffer (marker-position recorded-side))))
              ;; the sidecar shows one row per line, so its line names the row;
              ;; that row may begin part way along a soft-wrapped line
              (pi-annotate--position-at-row review (max 0 (1- line))))))
      (when (pi-annotate--may-scroll-p review main target)
        (pi-annotate--set-start main target)
        t))))

(defun pi-annotate--pre-redisplay (&optional _window)
  "Make the follower right before the window about to be painted is laid out.
`window-scroll-functions' does the same job one window earlier, while the
window that scrolled is being laid out, and a start given to the *other*
window at that point takes effect only on the next redisplay cycle -- which
arrives with the next event, so the sidecar sits on the wrong rows until the
hand moves again.  This hook runs in each review buffer just before one of its
windows is laid out, and the reviewed window is the left-hand one, so by the
time the sidecar is reached the reviewed window has already scrolled."
  (pi-annotate--scroll-all))

(defun pi-annotate--scroll-all (&optional _window _new-start)
  "Keep REVIEW's two windows on the same screen row.
WINDOW and NEW-START are ignored: the follower is chosen by comparing the two
windows' starts with the pair recorded by the last sync, which is also what
makes this safe to call from any of its three callers -- `post-command-hook',
`window-scroll-functions' and `pre-redisplay-functions'.

Comparing the actual starts rather than a remembered line means a scroll
*inside* one soft-wrapped line still moves the sidecar; a line number does not
change there, so a line-keyed sync froze and then jumped when the line finally
did change.

The three callers cover three different moments, and all three are needed.
A command that scrolls the reviewed window outright -- `C-v', a wheel event, a
jump -- has already moved its start when `post-command-hook' runs, so that is
the earliest point to follow it.  A scroll Emacs performs *while displaying*,
which is what the arrow keys do at the window edge, grows no start until
redisplay lays the window out, so it is caught by `window-scroll-functions'
-- and that call is also what marks the sidecar for repaint.  Finally, a start
handed to the other window during redisplay is honoured on the *next* cycle,
which may only arrive with the next event; `pre-redisplay-functions' runs in
the sidecar's own buffer just before that window is laid out, so whatever the
other two hooks left stale is corrected for the frame being painted now."
  (when (and pi-annotate-scroll-sync
             (pi-annotate--active-p)
             (not pi-annotate--syncing))
    (let* ((review pi-annotate--review)
           (main (pi-annotate--review-main-window review))
           (sidecar (pi-annotate--review-sidecar-window review)))
      (when (and (window-live-p main) (window-live-p sidecar))
        (let ((main-start (window-start main))
              (side-start (window-start sidecar))
              (was-main (pi-annotate--review-sync-main review))
              (was-side (pi-annotate--review-sync-side review))
              (pi-annotate--syncing t))
          ;; A window that could not be followed -- the guard in
          ;; `pi-annotate--may-scroll-p' keeps the cursor of a note on screen
          ;; -- leaves the record alone, so the pair stays known to be out of
          ;; step and the next command tries again.  Recording it anyway would
          ;; make the two windows agree on the wrong rows for good.
          (when
              (cond
               ;; the reviewed window moved -- keyboard or mouse scrolling, a
               ;; `recenter', a jump -- so the sidecar follows it
               ((and (not (eql main-start was-main)) (eql side-start was-side))
                (pi-annotate--follow-main review))
               ;; the sidecar moved -- usually the note being typed grew past
               ;; the window -- so the reviewed text follows it
               ((and (not (eql side-start was-side)) (eql main-start was-main))
                (pi-annotate--follow-sidecar review))
               ;; both moved (the first sync, a resize, a jump in both
               ;; windows): the window the user is in leads, the reviewed file
               ;; otherwise
               ((or (not (eql main-start was-main)) (not (eql side-start was-side)))
                (if (eq (selected-window) sidecar)
                    (pi-annotate--follow-sidecar review)
                  (pi-annotate--follow-main review))))
            (pi-annotate--record-sync review)))))))

;;;; Active annotation tracking

(defun pi-annotate--update-active ()
  "Highlight the annotation the cursor is on, in either window."
  (when (pi-annotate--active-p)
    (let* ((review pi-annotate--review)
           (block (and (or (eq (current-buffer) (pi-annotate--review-main-buffer review))
                           (eq (current-buffer) (pi-annotate--review-sidecar-buffer review)))
                       (pi-annotate--block-at-point review)))
           (id (and block (pi-annotate--block-id block))))
      (unless (eql id (pi-annotate--review-active-id review))
        (setf (pi-annotate--review-active-id review) id)
        (pi-annotate--render review)))))

(defun pi-annotate--after-command ()
  "Post-command hook for REVIEW's buffers."
  (when (pi-annotate--active-p)
    (pi-annotate--update-active)
    ;; syncing here, before redisplay, is what keeps the follower window from
    ;; showing the old position for a frame after a scroll
    (pi-annotate--scroll-all)))

;;;; Change hooks and redraw scheduling

(defun pi-annotate--schedule ()
  "Arrange a redraw once the user stops typing."
  (when (and (pi-annotate--active-p)
             (not pi-annotate--inhibit)
             (not (pi-annotate--review-ended pi-annotate--review)))
    (let ((review pi-annotate--review))
      ;; Restart the countdown on every change.  Keeping the timer armed from
      ;; the first edit instead would let it fire in the middle of a note,
      ;; where the redraw -- parse, relayout, re-dress and autosave -- lands
      ;; between two keystrokes and makes typing feel laggy.
      (when (timerp (pi-annotate--review-timer review))
        (cancel-timer (pi-annotate--review-timer review)))
      (setf (pi-annotate--review-timer review)
            (run-with-idle-timer
             pi-annotate-redraw-idle nil
             (lambda ()
               (when (eq review pi-annotate--review)
                 (setf (pi-annotate--review-timer review) nil)
                 (pi-annotate--flush))))))))

(defun pi-annotate--flush ()
  "Bring the sidecar, the highlights and the rules back in step."
  (when (and (pi-annotate--active-p)
             (not (pi-annotate--review-ended pi-annotate--review)))
    (let ((review pi-annotate--review))
      (pi-annotate--parse review)
      (pi-annotate--sync-markers review)
      (pi-annotate--relayout review)
      ;; wrapping a long note moves the notes below it, so the layout has to
      ;; be rebuilt around the new line count
      (when (pi-annotate--reflow review)
        (pi-annotate--relayout review))
      (pi-annotate--render review)
      (when pi-annotate-autosave
        (pi-annotate--save-sidecar-only review)))))

(defun pi-annotate--save-sidecar-only (review)
  "Write REVIEW's sidecar if it changed."
  (let ((sidecar (pi-annotate--review-sidecar-buffer review)))
    (when (buffer-live-p sidecar)
      (with-current-buffer sidecar
        (when (buffer-modified-p)
          (condition-case err
              (progn
                (write-region (point-min) (pi-annotate--written-end sidecar)
                              (pi-annotate--review-sidecar-path review) nil 'silent)
                (set-visited-file-modtime)
                (set-buffer-modified-p nil))
            (error (message "pi-annotate: could not save the sidecar: %s"
                            (error-message-string err)))))))))

;;;; Annotating

(defun pi-annotate--block-for-span (review span)
  "Return the block of REVIEW whose anchor equals SPAN."
  (cl-find-if (lambda (block)
                (equal (pi-annotate--span-of review (pi-annotate--block-id block)) span))
              (pi-annotate--review-blocks review)))

(defun pi-annotate--goto-note (review id)
  "Move the cursor into annotation ID's note in REVIEW."
  (let ((block (cl-find id (pi-annotate--review-blocks review)
                        :key #'pi-annotate--block-id)))
    (when block
      (setf (pi-annotate--review-last-id review) id
            (pi-annotate--review-active-id review) id)
      (let ((window (pi-annotate--review-sidecar-window review)))
        (when (window-live-p window)
          (select-window window)
          (goto-char (marker-position (pi-annotate--block-body-start block)))
          ;; recentering moves this window on its own, and the post-command
          ;; sync then brings the other window to the row it landed on
          (recenter)
          (pi-annotate--render review))))))

(defun pi-annotate--goto-text (review id)
  "Move the cursor to annotation ID's span in REVIEW's reviewed file."
  (let ((region (pi-annotate--anchor-region review id))
        (window (pi-annotate--review-main-window review)))
    (when (and region (window-live-p window))
      (setf (pi-annotate--review-active-id review) id)
      (select-window window)
      (goto-char (car region))
      ;; the post-command sync brings the sidecar to the row this landed on
      (recenter)
      (pi-annotate--render review))))

(defun pi-annotate--insert-block (review kind begin end)
  "Record a KIND annotation of BEGIN..END in REVIEW and return its id.
The recorded span is normalised first (a selection that ends at the start of
a line stops at the end of the previous one), so the quoted text of a
deletion matches the range the marker names."
  (let* ((id (pi-annotate--review-next-id review))
         (span (pi-annotate--span-from-region begin end))
         (main (pi-annotate--review-main-buffer review))
         (style (pi-annotate--review-style review))
         (width (pi-annotate--rule-width review))
         (end-marker (pi-annotate--end-marker style width))
         ;; `pi-annotate--pos-of' already clamps to the reviewed file's end
         (start-pos (pi-annotate--pos-of main (nth 0 span) (nth 1 span)))
         (end-pos (pi-annotate--pos-of main (nth 2 span) (1+ (nth 3 span))))
         (quote-lines nil))
    (when (string= kind "delete")
      (with-current-buffer main
        (setq quote-lines
              (mapcar (lambda (line)
                        (if (string-empty-p line) ">" (concat "> " line)))
                      (split-string
                       (buffer-substring-no-properties
                        (min start-pos end-pos) (max start-pos end-pos))
                       "\n")))))
    (setf (pi-annotate--review-next-id review) (1+ id))
    ;; anchor first: relayout reads the anchor back to place the note
    (push (cons id (cons (copy-marker start-pos nil) (copy-marker end-pos t)))
          (pi-annotate--review-anchors review))
    (let ((lines (append (list (pi-annotate--marker-line style id kind span width))
                         (or quote-lines (list ""))
                         (list end-marker))))
      (with-current-buffer (pi-annotate--review-sidecar-buffer review)
        (let ((pi-annotate--inhibit t))
          (save-excursion
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (mapconcat #'identity lines "\n") "\n")))))
    (pi-annotate--parse review)
    id))

(defun pi-annotate--delete-block (review block)
  "Remove BLOCK from REVIEW's sidecar and its highlight from the reviewed file."
  (with-current-buffer (pi-annotate--review-sidecar-buffer review)
    ;; the markers live in the sidecar, so its own end is what bounds them
    (let ((start (marker-position (pi-annotate--block-marker-start block)))
          (end (or (and (pi-annotate--block-end-end block)
                        (min (point-max) (1+ (marker-position (pi-annotate--block-end-end block)))))
                   (and (pi-annotate--block-body-end block)
                        (min (point-max) (1+ (marker-position (pi-annotate--block-body-end block))))))))
      (when (and start end)
        (let ((pi-annotate--inhibit t))
          (save-excursion
            (goto-char start)
            (delete-region start end)))))
    (setf (pi-annotate--review-anchors review)
          (assq-delete-all (pi-annotate--block-id block)
                           (pi-annotate--review-anchors review)))
    (when (eql (pi-annotate--review-active-id review) (pi-annotate--block-id block))
      (setf (pi-annotate--review-active-id review) nil))))

(defun pi-annotate--region-or-error ()
  "Return (BEGIN . END) of the active region, or signal a user error."
  (unless (use-region-p)
    (user-error "Select some text first"))
  (cons (region-beginning) (region-end)))

(defun pi-annotate-comment ()
  "Record a comment on the active region, then type the note."
  (interactive)
  (let* ((review pi-annotate--review)
         (region (pi-annotate--region-or-error))
         (span (pi-annotate--span-from-region (car region) (cdr region)))
         (existing (pi-annotate--block-for-span review span)))
    (if existing
        (pi-annotate--goto-note review (pi-annotate--block-id existing))
      (let ((id (pi-annotate--insert-block review "comment" (car region) (cdr region))))
        (pi-annotate--relayout review)
        (pi-annotate--render review)
        (pi-annotate--save-sidecar-only review)
        (pi-annotate--goto-note review id)))))

(defun pi-annotate-delete ()
  "Mark the active region for deletion, or unmark an identical range."
  (interactive)
  (let* ((review pi-annotate--review)
         (region (pi-annotate--region-or-error))
         (span (pi-annotate--span-from-region (car region) (cdr region)))
         (existing (pi-annotate--block-for-span review span)))
    (cond
     ((and existing (string= (pi-annotate--block-kind existing) "delete"))
      (pi-annotate--delete-block review existing)
      (pi-annotate--relayout review)
      (pi-annotate--render review)
      (pi-annotate--save-sidecar-only review)
      (message "pi-annotate: removed deletion mark at %d:%d \u2192 %d:%d"
               (nth 0 span) (nth 1 span) (nth 2 span) (nth 3 span)))
     (existing
      (pi-annotate--goto-note review (pi-annotate--block-id existing)))
     (t
      (let ((id (pi-annotate--insert-block review "delete" (car region) (cdr region))))
        (pi-annotate--relayout review)
        (pi-annotate--render review)
        (pi-annotate--save-sidecar-only review)
        (pi-annotate--goto-text review id)
        (message "pi-annotate: marked for deletion: %d:%d \u2192 %d:%d"
                 (nth 0 span) (nth 1 span) (nth 2 span) (nth 3 span)))))))

(defun pi-annotate-jump ()
  "Jump between an annotation and the text it belongs to."
  (interactive)
  (let* ((review pi-annotate--review)
         (block (pi-annotate--block-at-point review)))
    (cond
     ((null block)
      (if (pi-annotate--review-last-id review)
          (if (eq (current-buffer) (pi-annotate--review-sidecar-buffer review))
              (pi-annotate--goto-text review (pi-annotate--review-last-id review))
            (pi-annotate--goto-note review (pi-annotate--review-last-id review)))
        (user-error "No annotation here yet")))
     ((eq (current-buffer) (pi-annotate--review-sidecar-buffer review))
      (pi-annotate--goto-text review (pi-annotate--block-id block)))
     (t
      (pi-annotate--goto-note review (pi-annotate--block-id block))))))

(defun pi-annotate-clear ()
  "Remove the annotation at the cursor."
  (interactive)
  (let* ((review pi-annotate--review)
         (block (pi-annotate--block-at-point review)))
    (unless block
      (user-error "No annotation at the cursor"))
    (pi-annotate--delete-block review block)
    (pi-annotate--relayout review)
    (pi-annotate--render review)
    (pi-annotate--save-sidecar-only review)
    (message "pi-annotate: annotation removed")))

(defun pi-annotate-redraw ()
  "Re-read the anchors, redraw the review and line the windows up again.
Re-padding moves every note under the sidecar window, so the pair the scroll
sync last agreed on no longer describes what is on screen.  Re-aligning here,
against the row table the re-pad just built, keeps the next scroll on the
cheap delta path instead of making it pay for an absolute mapping."
  (interactive)
  (let ((review pi-annotate--review))
    (pi-annotate--relayout review)
    (pi-annotate--render review)
    (when (and (pi-annotate--active-p)
               (pi-annotate--follow-main review))
      (pi-annotate--record-sync review))))

;;;; Ending the review

(defun pi-annotate--write-result (review status)
  "Tell pi how REVIEW ended: STATUS is \"finished\" or \"aborted\"."
  (let ((file (pi-annotate--review-result-file review)))
    (when (and file (not (pi-annotate--review-ended review)))
      (condition-case err
          (progn
            (with-temp-buffer
              (insert status "\n")
              (write-region (point-min) (point-max) file nil 'silent))
            (setf (pi-annotate--review-ended review) status))
        (error (message "pi-annotate: could not report %s to pi: %s"
                        status (error-message-string err)))))))

(defun pi-annotate--teardown (review)
  "Drop REVIEW's hooks and decorations and restore the window layout."
  (dolist (buffer (list (pi-annotate--review-main-buffer review)
                        (pi-annotate--review-sidecar-buffer review)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-hook 'after-change-functions #'pi-annotate--after-change t)
        (remove-hook 'post-command-hook #'pi-annotate--after-command t)
        (remove-hook 'window-scroll-functions #'pi-annotate--scroll-all t)
        (when (bound-and-true-p pi-annotate-mode)
          (pi-annotate-mode -1)))))
  (remove-hook 'window-size-change-functions #'pi-annotate--window-size-changed)
  (let ((sidecar (pi-annotate--review-sidecar-buffer review))
        (window (pi-annotate--review-sidecar-window review))
        (config (pi-annotate--review-window-config review)))
    (pi-annotate--clear-decorations review)
    (when (window-live-p window)
      (condition-case nil (delete-window window) (error nil)))
    ;; never kill the buffer whose own kill hook brought us here
    (when (and (buffer-live-p sidecar)
               (not (eq sidecar (current-buffer))))
      (with-current-buffer sidecar
        (set-buffer-modified-p nil))
      (kill-buffer sidecar))
    (when (and config (window-configuration-p config))
      (condition-case nil
          (set-window-configuration config)
        (error nil)))))

(defun pi-annotate--finish (status)
  "End the current review, reporting STATUS to pi."
  (let ((review pi-annotate--review))
    (when (and review (not (pi-annotate--review-ended review)))
      (when (pi-annotate--active-p)
        (when (string= status "finished")
          ;; last chance to make the sidecar agree with the reviewed file
          (condition-case nil
              (progn
                (pi-annotate--parse review)
                (pi-annotate--sync-markers review)
                (pi-annotate--relayout review))
            (error nil)))
        (pi-annotate--save review))
      (pi-annotate--write-result review status)
      (pi-annotate--teardown review)
      (setq pi-annotate--review nil)
      (message "%s"
               (if (string= status "aborted")
                   "pi-annotate: review aborted \u2014 the notes are kept, nothing is applied"
                 "pi-annotate: review finished \u2014 the annotations were handed to pi")))))

(defun pi-annotate-finish ()
  "Save the annotations and end the review."
  (interactive)
  (pi-annotate--finish "finished"))

(defun pi-annotate-abort ()
  "End the review without handing the annotations to pi."
  (interactive)
  (pi-annotate--finish "aborted"))

(defun pi-annotate--after-change (&rest _)
  "Note that a review buffer changed and schedule a redraw.
A change moves rows: text typed above a note pushes it down, and the padding
\=\`pi-annotate--relayout' inserts does the same.  The scroll sync's delta is
measured against the pair the last sync agreed on, so it is only valid while
those rows stand still; this is what retires it, and the next sync then maps
the row the absolute way once, from a row table that a change invalidates
anyway."
  (when (pi-annotate--active-p)
    (cl-incf (pi-annotate--review-layout-gen pi-annotate--review)))
  (pi-annotate--schedule))

(defun pi-annotate--buffer-killed ()
  "Report the review as finished when one of its buffers is killed."
  (when (and pi-annotate--review
             (not (pi-annotate--review-ended pi-annotate--review)))
    (let ((review pi-annotate--review))
      ;; the buffer being killed is still readable here, so save what is left
      (ignore-errors (pi-annotate--save review))
      (pi-annotate--write-result review "finished")
      (pi-annotate--teardown review)
      (setq pi-annotate--review nil)
      (message "pi-annotate: review closed \u2014 the annotations were handed to pi"))))

;;;; Minor mode

(defvar pi-annotate-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `pi-annotate-mode'.")

(defun pi-annotate--install-keys ()
  "Build `pi-annotate-mode-map' from `pi-annotate-prefix-key'."
  ;; keep the keymap object: `define-minor-mode' captured it by identity
  (setcdr pi-annotate-mode-map nil)
  (let ((prefix (kbd pi-annotate-prefix-key)))
    (define-key pi-annotate-mode-map (vconcat prefix (vector ?c)) #'pi-annotate-comment)
    (define-key pi-annotate-mode-map (vconcat prefix (vector ?d)) #'pi-annotate-delete)
    (define-key pi-annotate-mode-map (vconcat prefix (vector ?j)) #'pi-annotate-jump)
    (define-key pi-annotate-mode-map (vconcat prefix (vector ?k)) #'pi-annotate-clear)
    (define-key pi-annotate-mode-map (vconcat prefix (vector ?q)) #'pi-annotate-finish)
    (define-key pi-annotate-mode-map (vconcat prefix (vector ?x)) #'pi-annotate-abort)
    (define-key pi-annotate-mode-map (vconcat prefix (vector ?g)) #'pi-annotate-redraw)))

;;;###autoload
(define-minor-mode pi-annotate-mode
  "Review-annotation mode for a file and its pi sidecar.

\\{pi-annotate-mode-map}"
  :lighter " annotate"
  :keymap pi-annotate-mode-map
  :group 'pi-annotate)

;;;; Sidecar paths

(defun pi-annotate--sidecar-path (path)
  "Return the sidecar path for PATH."
  (if (string-match-p "\\.comments\\.\\(?:md\\|org\\)\\'" path)
      path
    (concat (file-name-sans-extension path)
            (if (pi-annotate--org-sidecar-p path) ".comments.org" ".comments.md"))))

(defun pi-annotate--main-path-for (path)
  "Return the reviewed file behind sidecar PATH."
  (if (string-match "\\`\\(.+\\)\\.comments\\.\\(md\\|org\\)\\'" path)
      (concat (match-string 1 path) "." (match-string 2 path))
    path))

;;;; Entry point

(defun pi-annotate--open-buffer (path)
  "Visit PATH without disturbing the user's windows."
  (find-file-noselect (expand-file-name path) 'nowarn))

(defun pi-annotate--install-hooks (review)
  "Attach REVIEW's change, cursor and kill hooks to its two buffers."
  (dolist (buffer (list (pi-annotate--review-main-buffer review)
                        (pi-annotate--review-sidecar-buffer review)))
    (with-current-buffer buffer
      (add-hook 'after-change-functions #'pi-annotate--after-change nil t)
      (add-hook 'post-command-hook #'pi-annotate--after-command t)
      (add-hook 'window-scroll-functions #'pi-annotate--scroll-all nil t)
      (add-hook 'pre-redisplay-functions #'pi-annotate--pre-redisplay nil t)
      (add-hook 'kill-buffer-hook #'pi-annotate--buffer-killed nil t)
      (pi-annotate-mode 1))))

(defun pi-annotate--window-size-changed (&rest _)
  "Arrange a redraw after the review windows change size.
The rules in the sidecar are drawn to the window width, so a resize leaves
them too long or too short until the next redraw, and it changes how many rows
the reviewed file takes, so the row delta of the last sync no longer holds."
  (when (pi-annotate--active-p)
    (cl-incf (pi-annotate--review-layout-gen pi-annotate--review))
    (pi-annotate--schedule)))

;;;###autoload
(defun pi-annotate-open (path result-file)
  "Review PATH in this Emacs, reporting the outcome to RESULT-FILE.
Called by pi's /annotate command through `emacsclient --eval'."
  (unless (and (stringp path) (stringp result-file))
    (user-error "pi-annotate-open needs a file and a result file"))
  (when (and pi-annotate--review
             (not (pi-annotate--review-ended pi-annotate--review)))
    (error "pi-annotate: %s is still open for review"
           (pi-annotate--review-main-path pi-annotate--review)))
  (let* ((main-path (expand-file-name (pi-annotate--main-path-for path)))
         (sidecar-path (expand-file-name (pi-annotate--sidecar-path main-path)))
         (style (if (pi-annotate--org-sidecar-p main-path) 'org 'md))
         (review (pi-annotate--review-create
                  :main-path main-path :sidecar-path sidecar-path
                  :result-file (expand-file-name result-file)
                  :style style
                  :blocks nil :anchors nil :marks nil :decorations nil
                  :sync-main nil :sync-side nil
                  :sync-main-mark nil :sync-side-mark nil
                  :sync-gen 0 :layout-gen 0
                  :rows nil :rows-key nil :rows-total nil
                  :next-id 1 :active-id nil :last-id nil :ended nil)))
    (unless (file-readable-p main-path)
      (error "pi-annotate: cannot read %s" main-path))
    (setq pi-annotate--review review)
    (unwind-protect
        (progn
          (setf (pi-annotate--review-main-buffer review)
                (pi-annotate--open-buffer main-path))
          ;; the sidecar is regenerable: keep any unsaved text of a previous
          ;; visit, then start from a clean buffer so no stale overlays survive
          (let ((existing (find-buffer-visiting sidecar-path)))
            (when (buffer-live-p existing)
              (with-current-buffer existing
                (when (buffer-modified-p)
                  (write-region (point-min) (point-max) sidecar-path nil 'silent))
                (set-buffer-modified-p nil))
              (kill-buffer existing)))
          (unless (file-exists-p sidecar-path)
            (with-temp-buffer
              (insert (pi-annotate--header-line
                       style (file-name-nondirectory main-path))
                      "\n\n")
              (write-region (point-min) (point-max) sidecar-path nil 'silent)))
          (setf (pi-annotate--review-sidecar-buffer review)
                (pi-annotate--open-buffer sidecar-path))
          (pi-annotate--install-keys)
          (pi-annotate--install-hooks review)
          (pi-annotate--setup-windows review)
          ;; draw before claiming the review is ready, so a broken sidecar
          ;; fails here (and pi reports it) instead of after a long wait
          (pi-annotate--parse review)
          (pi-annotate--relayout review)
          (pi-annotate--render review)
          (pi-annotate--save-sidecar-only review)
          (message "pi-annotate: reviewing %s \u2014 %s c comment \u00b7 %s d delete \u00b7 %s j jump \u00b7 %s q finish \u00b7 %s x abort"
                   (file-name-nondirectory main-path)
                   pi-annotate-prefix-key pi-annotate-prefix-key
                   pi-annotate-prefix-key pi-annotate-prefix-key
                   pi-annotate-prefix-key)
          (buffer-name (pi-annotate--review-main-buffer review)))
      ;; on failure leave nothing half-open
      (when (and (not (pi-annotate--review-ended review))
                 (not (pi-annotate--active-p)))
        (pi-annotate--teardown review)
        (setq pi-annotate--review nil)))))

;;;###autoload
(defun pi-annotate-status ()
  "Describe the review currently open, if any."
  (interactive)
  (if (and pi-annotate--review (pi-annotate--active-p))
      (message "pi-annotate: reviewing %s (%d annotation%s)"
               (pi-annotate--review-main-path pi-annotate--review)
               (length (pi-annotate--review-blocks pi-annotate--review))
               (if (= 1 (length (pi-annotate--review-blocks pi-annotate--review))) "" "s"))
    (message "pi-annotate: no review open")))

(provide 'pi-annotate)
;;; pi-annotate.el ends here
