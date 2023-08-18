;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; multiple cursors / iedit
;; ----------------------------------------------------------------------

(use-package multiple-cursors
  :general
  (my-leader-key
    "ie" 'mc/edit-lines
    "ia" 'mc/mark-all-like-this
    "in" 'mc/mark-next-like-this
    "ip" 'mc/mark-previous-like-this
    "iN" 'mc/unmark-next-like-this
    "iP" 'mc/unmark-previous-like-this
    "is" 'mc/skip-to-next-like-this
    "iS" 'mc/skip-to-previous-like-this
    "ih" 'mc-hide-unmatched-lines-mode))

;; Iedit is interactive edit, where if you are on a word and you enter iedit-mode,
;; you're basically editing every instance of that word/variable in the buffer.
(use-package iedit
  :general
  (my-leader-key
    "i;" 'iedit-mode))

(provide 'init-mc-iedit)
;;; init-iedit.el ends here
