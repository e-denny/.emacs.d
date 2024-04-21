;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; multiple cursors / iedit
;; ----------------------------------------------------------------------

(use-package multiple-cursors
  :bind
  (("s-i e" . mc/edit-lines)
   ("s-i a" . mc/mark-all-like-this)
   ("s-i n" . mc/mark-next-like-this)
   ("s-i p" . mc/mark-previous-like-this)
   ("s-i N" . mc/unmark-next-like-this)
   ("s-i P" . mc/unmark-previous-like-this)
   ("s-i s" . mc/skip-to-next-like-this)
   ("s-i S" . mc/skip-to-previous-like-this)
   ("s-i h" . mc-hide-unmatched-lines-mode)))

;; Iedit is interactive edit, where if you are on a word and you enter iedit-mode,
;; you're basically editing every instance of that word/variable in the buffer.
(use-package iedit
  :bind
  (("s-i ;" . iedit-mode)))

(provide 'init-mc-iedit)
;;; init-iedit.el ends here
