  ;;; modal-setup.el --- modal keybinding minor mode. -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Modal Command Key Bindings

;;; Code:

;; (defun my-beginning-of-line-or-block ()
;;   "Move cursor to beginning of line or previous paragraph.

;; • When called first time, move cursor to beginning of char in current line.
;;   (if already, move to beginning of line.)
;; • When called again, move cursor backward by jumping over any sequence of
;;   whitespaces containing 2 blank lines."
;;   (interactive)
;;   (let (($p (point)))
;;     (if (or (equal (point) (line-beginning-position))
;;             (eq last-command this-command))
;;         (if (re-search-backward "\n[\t\n ]*\n+" nil "move")
;;             (progn
;;               (skip-chars-backward "\n\t ")
;;               ;; (forward-char )
;;               )
;;           (goto-char (point-min)))
;;       (progn
;;         (back-to-indentation)
;;         (when (eq $p (point))
;;           (beginning-of-line))))))

;; (defun my-end-of-line-or-block ()
;;   "Move cursor to end of line or next paragraph.

;; • When called first time, move cursor to end of line.
;; • When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines."
;;   (interactive)
;;   (if (or (equal (point) (line-end-position))
;;           (eq last-command this-command))
;;       (progn
;;         (re-search-forward "\n[\t\n ]*\n+" nil "move" ))
;;     (end-of-line)))

(setq debug-on-error t)

(defun modal--define-keys (keymap-name key-cmd-alist)
  "Map `define-key' for `KEYMAP-NAME' over a alist `KEY-CMD-ALIST'."
  (interactive)
  (dolist (item key-cmd-alist)
    (define-key keymap-name (kbd (car item)) (cdr item))))


(defhydra hydra-region (:color pink :hint nil)
  "
_s_ set-mark-command   _w_ mark-word          _x_ exchange-point-and-mark
_p_ mark-paragraph     _h_ mark-whole-buffer  _q_ quit
_e_ mark-sexp          _d_ mark-defun
"
  ("s" set-mark-command)
  ("p" mark-paragraph)
  ("e" mark-sexp)
  ("w" mark-word)
  ("h" mark-whole-buffer)
  ("d" mark-defun)
  ("x" exchange-point-and-mark)
  ("q" nil)
  )

(define-key global-map "C-c ." 'hydra-region/body)

(defvar modal-m-keymap-alist
  '(
    ("" . set-mark-command)
    ("" . mark-paragraph)
    ("" . mark-sexp)
    ("" . mark-word)
    ("" . mark-whole-buffer)
    ("" . mark-defun)
    ("" . exchange-point-and-mark)
    ))

(defvar modal-r-keymap-alist
  '(
    ("" . kill-region)
    ("" . comment-region)
    ("" . kill-ring-save)
    ("" . downcase-region)
    ("" . upcase-region)
    ("" . eval-region)
    ("" . capitalize-region)
    ("" . underline-region)
    ))


(defvar modal-c-keymap-alist
  '(
    ("w" . forward-word)
    ("q" . backward-word)
    ("s" . forward-sentence)
    ("a" . backward-sentence)
    ("x" . forward-paragraph)
    ("z" . backward-paragraph)
    ("d" . scroll-up-command)
    ("e" . scroll-down-command)
    ("f" . end-of-buffer)
    ("r" . beginning-of-buffer)
    ("0" . forward-sexp)
    ("9" . backward-sexp)
    ("," . back-to-indentation)
    ("o" . move-beginning-of-line)
    ("p" . move-end-of-line)
    ("-" . exchange-point-and-mark)
    ("g" . goto-line)
    ("m" . counsel-imenu)
    ("1" . bookmark-set)
    ("2" . bookmark-jump)
    ("3" . bookmark-bmenu-list)
    ("[" . beginning-of-defun)
    ("]" . end-of-defun)
    ;; scroll-other-window
    ))

(defvar modal-w-keymap-alist
  '(
    ("d" . delete-window)
    ("x" . delete-other-windows)
    ("=" . balance-windows)
    ("[" . shrink-window) ; make modal
    ("]" . enlarge-window) ; make modal
    ("," . shrink-window-horizontally) ; make modal
    ("." . enlarge-window-horizontally) ; make modal
    ("r" . split-window-right)
    ("b" . split-window-below)
    ("o" . other-window)
    ("q" . quit-window)
    ("u" . winner-undo)
    ("r" . winner-redo)
    ("j" . windmove-left)
    ("k" . windmove-down)
    ("l" . windmove-right)
    ("i" . windmove-up)
    ;; ("1" . (exwm-workspace-switch-create 0))
    ;; ("2" . (exwm-workspace-switch-create 1))
    ;; ("3" . (exwm-workspace-switch-create 2))
    ;; ("4" . (exwm-workspace-switch-create 3))
    ))

(defvar modal-b-keymap-alist
  '(
    ("[" . previous-buffer)
    ("]" . next-buffer)
    ("m" . helm-buffers-list)
    ("s" . save-buffer)
    ("a" . save-some-buffers)
    ("k" . kill-buffer)
    ("z" . kill-some-buffers)
    ("x" . kill-matching-buffers)
    ("r" . rename-buffer)
    ("k" . buf-move-down)
    ("i" . buf-move-up)
    ("j" . buf-move-left)
    ("l" . buf-move-right)
    ))

(defvar modal-h-keymap-alist
  '(
    (";" . Info-goto-emacs-command-node)
    ("a" . apropos-command)
    ("b" . describe-bindings)
    ("c" . describe-char)
    ("d" . apropos-documentation)
    ("e" . view-echo-area-messages)
    ("f" . describe-face)
    ("g" . info-lookup-symbol)
    ("h" . describe-function)
    ("i" . info)
    ("j" . man)
    ("k" . describe-key)
    ("K" . Info-goto-emacs-key-command-node)
    ("l" . view-lossage)
    ("n" . describe-variable)
    ("o" . describe-language-environment)
    ("p" . finder-by-keyword)
    ("r" . apropos-variable)
    ("s" . describe-syntax)
    ("u" . elisp-index-search)
    ("v" . apropos-value)
    ("z" . describe-coding-system)
    ))

(defvar modal-leader-keymap-alist
  '(
    ;; ("\\" . toggle-input-method)
    ("0" . delete-window)
    ("4" . split-window-right)
    ("5" . balance-windows)
    ("9" . ispell-word)
    ("a" . mark-whole-buffer)
    ("b" . end-of-buffer)
    ("d" . beginning-of-buffer)
    ("g" . isearch-forward)
    ("h" . modal-command-h-prefix)
    ("i" . kill-line)
    ("l" . recenter-top-bottom) ; move to m
    ("o" . exchange-point-and-mark) ; move-to-m
    ("p" . query-replace)
    ("s" . save-buffer)
    ("u" . switch-to-buffer)
    ))

(defvar modal-command-keymap-alist
  '(
    ("SPC" . modal-command-leader-prefix)
    ("h" . modal-command-h-prefix)
    ("m" . modal-command-c-prefix)
    ("b" . modal-command-b-prefix)
    ("w" . modal-command-w-prefix)
    ("j" . backward-char)
    ("l" . forward-char)
    ("k" . next-line)
    ("i" . previous-line)
    ("u" . modal-mode-toggle)
    ))

(defvar modal-command-key-map (make-sparse-keymap) "Keybinding for `modal-command-mode' minor mode.")

(defvar modal-command-cursor-type nil "Cursor type to use in modal-command-mode.")

(setq modal-command-cursor-type 'box)

(define-minor-mode modal-command-mode
  "A modal command keybinding set."
  nil
  "modal-command"
  modal-command-key-map
  (modal-command-mode-activate)
  ;; (setq-local cursor-type
  ;;             (if modal-command-mode
  ;;                 modal-command-cursor-type
  ;;               (default-value 'cursor-type)))
  )


(defun modal-command-mode-activate ()
  "Activate command mode.
Runs `modal-command-mode-activate'"
  (interactive)
  (unless (minibufferp)
    (modal--define-keys (define-prefix-command 'modal-command-leader-prefix)
                        modal-leader-keymap-alist)
    (modal--define-keys (define-prefix-command 'modal-command-h-prefix)
                        modal-h-keymap-alist)
    (modal--define-keys (define-prefix-command 'modal-command-c-prefix)
                        modal-c-keymap-alist)
    (modal--define-keys (define-prefix-command 'modal-command-b-prefix)
                        modal-b-keymap-alist)
    (modal--define-keys (define-prefix-command 'modal-command-w-prefix)
                        modal-w-keymap-alist)
    (modal--define-keys modal-command-key-map modal-command-keymap-alist))
  )

(defun modal-command-mode-inactivate ()
  "Turn off modal-keys minor mode."
  (interactive)
  (modal-command-mode 0))

(defun modal-mode-toggle ()
  "Switch between =command= and =Emacs= modes."
  (interactive)
  (if modal-command-mode
      (modal-command-mode-inactivate)
    (modal-command-mode)))

(global-set-key (kbd "C-.") 'modal-mode-toggle)

(provide 'modal-setup)

;;; modal-setup.el ends here
