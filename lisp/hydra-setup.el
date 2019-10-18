;;; hydra-setup.el --- Hydra Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Hydra Setup

;;; Code:

(use-package use-package-hydra
  :ensure t)

(with-eval-after-load 'hydra
  (defhydra my/hydra-rectangle (:body-pre (rectangle-mark-mode 1) :post (deactivate-mark))
    "
    -----          |   ^_p_^   |    _d_elete    _k_ill
Rectangle Mode     | _b_   _f_ |    _s_tring    _y_ank
    -----          |   ^_n_^   |    _c_opy      _r_eset"
    ("p" rectangle-previous-line nil)
    ("n" rectangle-next-line nil)
    ("b" rectangle-backward-char nil)
    ("f" rectangle-forward-char nil)
    ("d" delete-rectangle nil)
    ("s" string-rectangle nil :exit t)
    ("k" kill-rectangle nil)
    ("y" yank-rectangle nil :exit t)
    ("c" copy-rectangle-as-kill nil)
    ("r" (progn (if (region-active-p)
                    (deactivate-mark))
                (rectangle-mark-mode 1)) nil)))

(use-package multiple-cursors
  :defer t)

(defhydra my/multiple-cursors-hydra (:hint nil)
  "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))


(provide 'hydra-setup)

;;; hydra-setup.el ends here
