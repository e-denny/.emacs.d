;;; flycheck-setup.el --- Flycheck Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Flycheck Setup

;;; Code:

(use-package flycheck
  :ensure t
  :diminish "✓"
  :commands global-flycheck-mode
  :init (add-hook 'prog-mode-hook 'global-flycheck-mode)
  :config
  (progn
    (setq-default flycheck-emacs-lisp-initialize-packages t
                  flycheck-highlighting-mode 'lines
                  flycheck-display-errors-delay 0.3
                  flycheck-check-syntax-automatically '(save)
                  flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
    ;; Define fringe indicator / warning levels
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'infoq
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-info)
    (custom-set-variables
     '(flycheck-python-flake8-executable "python3")
     '(flycheck-python-pycompile-executable "python3")
     '(flycheck-python-pylint-executable "python3"))))

(defhydra hydra-flycheck (:color pink)
  "
^
^Flycheck^          ^Errors^            ^Checker^
^────────^──────────^──────^────────────^───────^───────────
_q_ quit            _<_ previous        _?_ describe
_m_ manual          _>_ next            _d_ disable
_v_ verify setup    _f_ check           _s_ select
^^                  _l_ list            ^^
^^                  ^^                  ^^
"
  ("q" nil)
  ("<" flycheck-previous-error)
  (">" flycheck-next-error)
  ("?" flycheck-describe-checker :color blue)
  ("d" flycheck-disable-checker :color blue)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors :color blue)
  ("m" flycheck-manual :color blue)
  ("s" flycheck-select-checker :color blue)
  ("v" flycheck-verify-setup :color blue))

(provide 'flycheck-setup)

;;; flycheck-setup.el ends here
