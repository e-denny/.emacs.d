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
;;     cursor moves into the new box so the note can be typed at once;
;;   * `C-c C d' marks the region for deletion (press it again on the same
;;     range to remove the mark);
;;   * `C-c C j' jumps between a box and the text it belongs to, `C-c C k'
;;     removes the annotation at the cursor;
;;   * `C-c C q' finishes the review, `C-c C x' aborts it.
;;
;; Commented spans get a yellow background in the reviewed file, deleted spans
;; get strikethrough, and every annotation is drawn as a rounded box in the
;; sidecar, aligned with the text it refers to.  The sidecar stays a plain
;; Markdown/Org file on disk: the markers, borders and highlighting are
;; overlays, not text.
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
The sidecar is laid out on the same line numbers as the reviewed file, so
keeping the two window starts on the same line keeps every box beside the
text it annotates."
  :type 'boolean
  :group 'pi-annotate)

(defcustom pi-annotate-autosave t
  "Whether the sidecar is written as you annotate."
  :type 'boolean
  :group 'pi-annotate)

(defcustom pi-annotate-autoformat t
  "Whether notes are wrapped to the box width as you type.
Longer lines that arrive from elsewhere are re-wrapped when the review
redraws; the wrapping is undone by setting this to nil."
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
;; deletions, and a blue/red/yellow box border that brightens for the
;; annotation under the cursor.

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
  "Face for the box borders in the sidecar."
  :group 'pi-annotate)

(defface pi-annotate-box-comment
  '((((background dark)) :foreground "#7aa2f7" :weight bold)
    (((background light)) :foreground "#1e66f5" :weight bold)
    (t :inherit default))
  "Face for the border of a comment box."
  :group 'pi-annotate)

(defface pi-annotate-box-delete
  '((((background dark)) :foreground "#f7768e" :weight bold)
    (((background light)) :foreground "#d20f39" :weight bold)
    (t :inherit default))
  "Face for the border of a deletion box."
  :group 'pi-annotate)

(defface pi-annotate-box-active
  '((((background dark)) :foreground "#ffd75f" :weight bold)
    (((background light)) :foreground "#b8860b" :weight bold)
    (t :inherit default))
  "Face for the border of the box at the cursor."
  :group 'pi-annotate)

;;;; Source formats
;;
;; Markdown sidecars hold their markers in HTML comments (invisible in
;; rendered Markdown); Org sidecars use `# annotate:' comment lines.  Parsing
;; accepts both styles whatever the file extension, so a sidecar written by an
;; earlier run is still picked up.

(defconst pi-annotate--marker-regexps
  (list
   (concat "^<!--[ \t]*annotate:#\\([0-9]+\\)[ \t]+"
           "\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\)[ \t]+"
           "\\([a-zA-Z_]+\\)[ \t]*-->[ \t]*$")
   (concat "^#[ \t]*annotate:#\\([0-9]+\\)[ \t]+"
           "\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\)[ \t]+"
           "\\([a-zA-Z_]+\\)[ \t]*$"))
  "Regexps matching an annotation marker line, either comment style.")

(defconst pi-annotate--end-marker-regexps
  (list "^<!--[ \t]*/annotate[ \t]*-->[ \t]*$"
        "^#[ \t]*/annotate[ \t]*$")
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

(defun pi-annotate--marker-line (style id kind span)
  "Return the marker line for STYLE recording ID, KIND and SPAN.
SPAN is a list of (START-LINE START-COL END-LINE END-COL)."
  (cl-destructuring-bind (sl sc el ec) span
    (if (eq style 'org)
        (format "# annotate:#%d %d:%d-%d:%d %s" id sl sc el ec kind)
      (format "<!-- annotate:#%d %d:%d-%d:%d %s -->" id sl sc el ec kind))))

(defun pi-annotate--end-marker (style)
  "Return the end marker line for STYLE."
  (if (eq style 'org) "# /annotate" "<!-- /annotate -->"))

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
  boxes                          ; list of decoration overlays in the sidecar
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
           (end (pi-annotate--pos-of main (nth 2 span) (nth 3 span))))
      (push (cons id (cons (copy-marker start nil) (copy-marker end t)))
            (pi-annotate--review-anchors review))))
  (cdr (assq id (pi-annotate--review-anchors review))))

(defun pi-annotate--anchor-region (review id)
  "Return (START . END) buffer positions of annotation ID in REVIEW."
  (let ((anchor (cdr (assq id (pi-annotate--review-anchors review)))))
    (when anchor
      (cons (max (point-min) (min (point-max) (car anchor)))
            (max (point-min) (min (point-max) (cdr anchor)))))))

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

(defun pi-annotate--box-width (review)
  "Return the box width, in columns, for REVIEW's sidecar window."
  (let ((win (pi-annotate--review-sidecar-window review)))
    (max 24 (1- (if (window-live-p win) (window-body-width win) 61)))))

(defun pi-annotate--text-width (review)
  "Return the width available for note text in REVIEW."
  (max 8 (- (pi-annotate--box-width review) 4)))

(defun pi-annotate--label (review id kind)
  "Return the box label for annotation ID of KIND in REVIEW."
  (let ((span (pi-annotate--span-of review id)))
    (if span
        (format "%d:%d \u2192 %d:%d  %s"
                (nth 0 span) (nth 1 span) (nth 2 span) (nth 3 span) kind)
      (format "?  %s" kind))))

(defun pi-annotate--top-border (width label)
  "Return a box top border of WIDTH labelled LABEL."
  (let* ((label-width (string-width label))
         (width (if (< width (+ 5 label-width)) (+ 5 label-width) width))
         (pad (- width 5 label-width)))
    (concat "\u256d\u2500 " label " " (make-string (max 0 pad) ?\u2500) "\u256e")))

(defun pi-annotate--bottom-border (width)
  "Return a box bottom border of WIDTH."
  (concat "\u2570" (make-string (max 0 (- width 2)) ?\u2500) "\u256f"))

(defun pi-annotate--border-face (review id kind)
  "Return the border face for annotation ID of KIND in REVIEW."
  (cond
   ((eql id (pi-annotate--review-active-id review)) 'pi-annotate-box-active)
   ((string= kind "delete") 'pi-annotate-box-delete)
   (t 'pi-annotate-box-comment)))

(defun pi-annotate--clear-decorations (review)
  "Remove every decoration overlay REVIEW owns."
  (dolist (overlay (pi-annotate--review-boxes review))
    (when (overlayp overlay) (delete-overlay overlay)))
  (setf (pi-annotate--review-boxes review) nil))

(defun pi-annotate--render-boxes (review)
  "Draw every annotation of REVIEW as a box in the sidecar."
  (let* ((buffer (pi-annotate--review-sidecar-buffer review))
         (width (pi-annotate--box-width review))
         (overlays nil))
    (with-current-buffer buffer
      (save-excursion
        ;; the source header is plumbing: keep it out of sight
        (goto-char (point-min))
        (when (pi-annotate--match-source
               (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put overlay 'display "")
            (push overlay overlays)))
        (dolist (block (pi-annotate--review-blocks review))
          (let* ((id (pi-annotate--block-id block))
                 (kind (pi-annotate--block-kind block))
                 (face (pi-annotate--border-face review id kind))
                 (top (pi-annotate--top-border width (pi-annotate--label review id kind)))
                 (bottom (pi-annotate--bottom-border width)))
            ;; the marker line becomes the top border
            (let ((overlay (make-overlay (marker-position (pi-annotate--block-marker-start block))
                                         (marker-position (pi-annotate--block-marker-end block)))))
              (overlay-put overlay 'display top)
              (overlay-put overlay 'face face)
              (push overlay overlays))
            ;; each note line gets a left and a right border
            (let ((start (marker-position (pi-annotate--block-body-start block)))
                  (end (marker-position (pi-annotate--block-body-end block))))
              (save-excursion
                (goto-char start)
                (while (< (point) (max (1+ start) end))
                  (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
                    ;; `before-string' (rather than `line-prefix') because the
                    ;; left border must be a displayed string; the window
                    ;; truncates rather than wraps, so every note line is one
                    ;; screen line and the right border lands on the box edge
                    (overlay-put overlay 'before-string
                                 (propertize "\u2502 " 'face face))
                    (overlay-put overlay 'after-string
                                 (concat (propertize " " 'display `(space :align-to ,(1- width)))
                                         (propertize "\u2502" 'face face)))
                    (push overlay overlays))
                  (forward-line 1))))
            ;; the end marker becomes the bottom border
            (when (pi-annotate--block-end-start block)
              (let ((overlay (make-overlay (marker-position (pi-annotate--block-end-start block))
                                           (marker-position (pi-annotate--block-end-end block)))))
                (overlay-put overlay 'display bottom)
                (overlay-put overlay 'face face)
                (push overlay overlays))))))
      (setf (pi-annotate--review-boxes review) (nreverse overlays)))))

(defun pi-annotate--render (review)
  "Redraw REVIEW: re-read the sidecar, then highlight and box every block."
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
    (pi-annotate--clear-decorations review)
    (pi-annotate--render-boxes review)))

;;;; Wrapping

(defun pi-annotate--wrap (text width prefix)
  "Wrap TEXT to WIDTH columns, starting each line with PREFIX.
Greedy, by display width, and PREFIX goes on every line — the same shape as
annotate.nvim's wrapping, which is why a single unbreakable token longer than
the box can still cross its border."
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
  "Re-wrap BLOCK of REVIEW when its text is wider than the box.
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
      (setq wrapped (pi-annotate--wrap
                     (string-trim (replace-regexp-in-string
                                   "[ \t\n]+" " " (mapconcat #'identity lines " ")))
                     width nil)))
     (t (setq wrapped lines)))
    (when (and wrapped (not (equal wrapped lines)))
      (let ((start (marker-position (pi-annotate--block-body-start block)))
            (end (marker-position (pi-annotate--block-body-end block))))
        (when (and start end)
          (let ((pi-annotate--inhibit t))
            (save-excursion
              (goto-char start)
              (delete-region start end)
              (insert (mapconcat #'identity wrapped "\n"))))
          (setq changed t))))
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
  "Return the sidecar of REVIEW as a list of lines, padded for alignment."
  (let* ((header (pi-annotate--header-line
                  (pi-annotate--review-style review)
                  (file-name-nondirectory (pi-annotate--review-main-path review))))
         (end-marker (pi-annotate--end-marker (pi-annotate--review-style review)))
         (blocks (seq-sort
                  (lambda (a b)
                    (let ((sa (pi-annotate--span-of review (pi-annotate--block-id a)))
                          (sb (pi-annotate--span-of review (pi-annotate--block-id b))))
                      (if (/= (nth 0 sa) (nth 0 sb))
                          (< (nth 0 sa) (nth 0 sb))
                        (< (nth 1 sa) (nth 1 sb)))))
                  (copy-sequence (pi-annotate--review-blocks review))))
         (lines (list header)))
    (with-current-buffer (pi-annotate--review-sidecar-buffer review)
      (dolist (block blocks)
        (let* ((id (pi-annotate--block-id block))
               (span (pi-annotate--span-of review id))
               (target (nth 0 span)))
          (while (< (length lines) (max 1 (1- target)))
            (push "" lines))
          (push (pi-annotate--marker-line (pi-annotate--review-style review)
                                          id (pi-annotate--block-kind block) span)
                lines)
          (dolist (line (pi-annotate--body-lines block))
            (push line lines))
          (push end-marker lines)
          (push "" lines))))
    (nreverse lines)))

(defun pi-annotate--buffer-lines (buffer)
  "Return BUFFER's contents as a list of lines."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((lines nil))
        (while (not (eobp))
          (push (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                lines)
          (forward-line 1))
        (nreverse lines)))))

(defun pi-annotate--line-position (buffer line)
  "Return the start position of 1-based LINE in BUFFER."
  (or (pi-annotate--line-start buffer line) (point-max)))

(defun pi-annotate--apply-lines (review desired)
  "Make REVIEW's sidecar equal DESIRED, touching only what differs.
Return non-nil when the buffer changed."
  (let* ((buffer (pi-annotate--review-sidecar-buffer review))
         (current (pi-annotate--buffer-lines buffer))
         (n (length current))
         (m (length desired))
         (prefix 0)
         (suffix 0))
    (while (and (< prefix n) (< prefix m)
                (string= (nth prefix current) (nth prefix desired)))
      (setq prefix (1+ prefix)))
    (while (and (< suffix (- n prefix)) (< suffix (- m prefix))
                (string= (nth (- n 1 suffix) current) (nth (- m 1 suffix) desired)))
      (setq suffix (1+ suffix)))
    (if (and (= prefix n) (= prefix m))
        nil
      (let* ((replacement (cl-subseq desired prefix (- m suffix)))
             (text (if replacement
                       (concat (mapconcat #'identity replacement "\n") "\n")
                     "")))
        (with-current-buffer buffer
          (let ((pi-annotate--inhibit t))
            (save-excursion
              ;; the common head and tail are left alone, so point and the undo
              ;; history survive a redraw that only moves padding around
              (let ((start (or (pi-annotate--line-start buffer (1+ prefix))
                               (point-max)))
                    (last-line (- n suffix)))
                (when (> last-line prefix)
                  (goto-char (pi-annotate--line-start buffer last-line))
                  (delete-region start (min (point-max) (1+ (line-end-position)))))
                (goto-char start)
                (insert text)))))
        (pi-annotate--parse review)
        t))))

(defun pi-annotate--sync-markers (review)
  "Refresh REVIEW's sidecar marker lines from the live anchors.
Return non-nil when any marker line changed."
  (let ((changed nil))
    (with-current-buffer (pi-annotate--review-sidecar-buffer review)
      (dolist (block (pi-annotate--review-blocks review))
        (let* ((id (pi-annotate--block-id block))
               (span (pi-annotate--span-of review id))
               (want (and span (pi-annotate--marker-line
                                (pi-annotate--review-style review)
                                id (pi-annotate--block-kind block) span)))
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
  "Re-pad REVIEW's sidecar so each box sits on its text's line."
  (when (and (pi-annotate--active-p)
             (not (pi-annotate--review-ended review)))
    (pi-annotate--parse review)
    (pi-annotate--sync-markers review)
    (let ((desired (pi-annotate--desired-lines review)))
      (pi-annotate--apply-lines review desired))))

;;;; Saving

(defun pi-annotate--save (review)
  "Write REVIEW's sidecar (and its reviewed file when it is modified)."
  (let ((sidecar (pi-annotate--review-sidecar-buffer review))
        (main (pi-annotate--review-main-buffer review)))
    (when (buffer-live-p sidecar)
      (with-current-buffer sidecar
        (when (buffer-modified-p)
          (write-region (point-min) (point-max) (pi-annotate--review-sidecar-path review) nil 'silent)
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
    (setq sidecar-window (split-window-right (pi-annotate--sidecar-width)))
    (set-window-buffer sidecar-window sidecar)
    (set-window-parameter main-window 'pi-annotate t)
    (set-window-parameter sidecar-window 'pi-annotate t)
    (with-current-buffer sidecar
      (setq-local truncate-lines t)
      (setq-local word-wrap nil)
      (setq-local display-line-numbers nil)
      (display-line-numbers-mode -1)
      (setq-local fill-column (pi-annotate--text-width review))
      (when pi-annotate-autoformat
        (auto-fill-mode 1)))
    (with-current-buffer main
      (setq-local truncate-lines nil))
    (setf (pi-annotate--review-main-window review) main-window
          (pi-annotate--review-sidecar-window review) sidecar-window)
    (select-window main-window)
    (set-window-start main-window (point-min) t)
    (set-window-start sidecar-window (point-min) t)))

(defun pi-annotate--line-number (buffer position)
  "Return the 1-based line number of POSITION in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (min (max position (point-min)) (point-max)))
      (line-number-at-pos))))

(defun pi-annotate--scroll-all (window new-start)
  "Keep REVIEW's two windows on the same line when WINDOW scrolls to NEW-START."
  (when (and pi-annotate-scroll-sync
             (not pi-annotate--syncing)
             pi-annotate--review
             (window-parameter window 'pi-annotate)
             (window-live-p window))
    (let* ((buffer (window-buffer window))
           (other (if (eq buffer (pi-annotate--review-main-buffer pi-annotate--review))
                      (pi-annotate--review-sidecar-window pi-annotate--review)
                    (pi-annotate--review-main-window pi-annotate--review))))
      (when (and (window-live-p other)
                 (not (eq other window))
                 (window-parameter other 'pi-annotate))
        (let* ((line (pi-annotate--line-number buffer new-start))
               (target (pi-annotate--line-position (window-buffer other) line)))
          (unless (eq (window-start other) target)
            (let ((pi-annotate--syncing t))
              (set-window-start other target))))))))

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
    (pi-annotate--update-active)))

;;;; Change hooks and redraw scheduling

(defun pi-annotate--schedule ()
  "Arrange a redraw once the user stops typing."
  (when (and (pi-annotate--active-p)
             (not pi-annotate--inhibit)
             (not (pi-annotate--review-ended pi-annotate--review)))
    (let ((review pi-annotate--review))
      (unless (timerp (pi-annotate--review-timer review))
        (setf (pi-annotate--review-timer review)
              (run-with-idle-timer
               0.25 nil
               (lambda ()
                 (when (eq review pi-annotate--review)
                   (setf (pi-annotate--review-timer review) nil)
                   (pi-annotate--flush)))))))))

(defun pi-annotate--flush ()
  "Bring the sidecar, the highlights and the boxes back in step."
  (when (and (pi-annotate--active-p)
             (not (pi-annotate--review-ended pi-annotate--review)))
    (let ((review pi-annotate--review))
      (pi-annotate--parse review)
      (pi-annotate--sync-markers review)
      (pi-annotate--relayout review)
      (when (pi-annotate--reflow review)
        (pi-annotate--parse review))
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
                (write-region (point-min) (point-max)
                              (pi-annotate--review-sidecar-path review) nil 'silent)
                (set-buffer-modified-p nil))
            (error (message "pi-annotate: could not save the sidecar: %s"
                            (error-message-string err)))))))))

;;;; Annotating

(defun pi-annotate--block-for-span (review span)
  "Return the block of REVIEW whose anchor equals SPAN."
  (cl-find-if (lambda (block)
                (equal (pi-annotate--span-of review (pi-annotate--block-id block)) span))
              (pi-annotate--review-blocks review)))

(defun pi-annotate--goto-box (review id)
  "Move the cursor into annotation ID's box in REVIEW."
  (let ((block (cl-find id (pi-annotate--review-blocks review)
                        :key #'pi-annotate--block-id)))
    (when block
      (setf (pi-annotate--review-last-id review) id
            (pi-annotate--review-active-id review) id)
      (let ((window (pi-annotate--review-sidecar-window review)))
        (when (window-live-p window)
          (select-window window)
          (goto-char (marker-position (pi-annotate--block-body-start block)))
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
         (end-marker (pi-annotate--end-marker style))
         (start-pos (pi-annotate--pos-of main (nth 0 span) (nth 1 span)))
         (end-pos (min (point-max)
                       (pi-annotate--pos-of main (nth 2 span) (1+ (nth 3 span)))))
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
    ;; anchor first: relayout reads the anchor back to place the box
    (push (cons id (cons (copy-marker start-pos nil) (copy-marker end-pos t)))
          (pi-annotate--review-anchors review))
    (let ((lines (append (list (pi-annotate--marker-line style id kind span))
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
  (let ((start (marker-position (pi-annotate--block-marker-start block)))
        (end (or (and (pi-annotate--block-end-end block)
                      (min (point-max) (1+ (marker-position (pi-annotate--block-end-end block)))))
                 (and (pi-annotate--block-body-end block)
                      (min (point-max) (1+ (marker-position (pi-annotate--block-body-end block))))))))
    (when (and start end)
      (with-current-buffer (pi-annotate--review-sidecar-buffer review)
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
        (pi-annotate--goto-box review (pi-annotate--block-id existing))
      (let ((id (pi-annotate--insert-block review "comment" (car region) (cdr region))))
        (pi-annotate--relayout review)
        (pi-annotate--render review)
        (pi-annotate--save-sidecar-only review)
        (pi-annotate--goto-box review id)))))

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
      (pi-annotate--goto-box review (pi-annotate--block-id existing)))
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
            (pi-annotate--goto-box review (pi-annotate--review-last-id review)))
        (user-error "No annotation here yet")))
     ((eq (current-buffer) (pi-annotate--review-sidecar-buffer review))
      (pi-annotate--goto-text review (pi-annotate--block-id block)))
     (t
      (pi-annotate--goto-box review (pi-annotate--block-id block))))))

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
  "Re-read the anchors and redraw the review."
  (interactive)
  (let ((review pi-annotate--review))
    (pi-annotate--relayout review)
    (pi-annotate--render review)))

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
  "Note that a review buffer changed and schedule a redraw."
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
      (add-hook 'kill-buffer-hook #'pi-annotate--buffer-killed nil t)
      (pi-annotate-mode 1))))

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
                  :blocks nil :anchors nil :marks nil :boxes nil
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
