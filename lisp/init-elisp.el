;;; .emacs  --- user-init-file   -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Package system

;; ----------------------------------------------------------------------
;; elisp
;; ----------------------------------------------------------------------

(defvar +emacs-lisp--face nil)

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and variables.")

;; from doom emacs
(defun +emacs-lisp-highlight-vars-and-faces (end)
  "Match defined variables and functions to END.
Functions are differentiated into special forms, built-in functions and
  library/userland functions"
  (catch 'matcher
    (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
      (let ((ppss (save-excursion (syntax-ppss))))
        (cond ((nth 3 ppss)  ; strings
               (search-forward "\"" end t))
              ((nth 4 ppss)  ; comments
               (forward-line +1))
              ((let ((symbol (intern-soft (match-string-no-properties 0))))
                 (and (cond ((null symbol) nil)
                            ((eq symbol t) nil)
                            ((keywordp symbol) nil)
                            ((special-variable-p symbol)
                             (setq +emacs-lisp--face 'font-lock-variable-name-face))
                            ((and (fboundp symbol)
                                  (eq (char-before (match-beginning 0)) ?\()
                                  (not (memq (char-before (1- (match-beginning 0)))
                                             (list ?\' ?\`))))
                             (let ((unaliased (indirect-function symbol)))
                               (unless (or (macrop unaliased)
                                           (special-form-p unaliased))
                                 (let (unadvised)
                                   (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                   (setq unaliased (indirect-function unadvised)))))
                                   unaliased)
                                 (setq +emacs-lisp--face
                                       (if (subrp unaliased)
                                           'font-lock-constant-face
                                         'font-lock-function-name-face))))))
                      (throw 'matcher t)))))))
    nil))

;; We byte-compile to ensure they run as fast as possible:
(dolist (fn '(+emacs-lisp-highlight-vars-and-faces))
  (unless (byte-code-function-p (symbol-function fn))
    (with-no-warnings (byte-compile fn))))

(use-package highlight-quoted)

(use-package elisp-mode
  :ensure nil
  :custom
  (font-lock-maximum-decoration t)
  (debugger-stack-frame-as-list t)
  ;; Enhance elisp syntax highlighting, by highlighting defined symbols.
  :config
  (defun my-enhanced-elisp-fontification ()
    (font-lock-add-keywords
     'emacs-lisp-mode
     (append
      ;; highlight defined, special variables & functions
      (when +emacs-lisp-enable-extra-fontification
        `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face))))))

  (defun my/debugger-pp-frame ()
    (interactive)
    (let ((inhibit-read-only t)
          (frame (backtrace-frame (debugger-frame-number))))
      (set-buffer (pop-to-buffer "*BT: Frame*"))
      (destructuring-bind (special fn &rest args) frame
                          (erase-buffer)
                          (progn
                            (insert "(" (pp-to-string fn))
                            (dolist (arg args)
                              (insert "\n" (pp-to-string arg)))
                            (insert ")"))
                          (goto-char (point-min))
                          (indent-pp-sexp))))
  :hook ((emacs-lisp-mode . highlight-quoted-mode)
         (emacs-lisp-mode . my-enhanced-elisp-fontification)))

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init
  (setq eldoc-idle-delay 0.1)
  :hook ((emacs-lisp-mode . eldoc-mode)
         (lisp-interaction-mode . eldoc-mode)
         (lisp-mode . eldoc-mode)
         (ielm-mode . eldoc-mode)))

;; M-. navigate to symbol
;; M-, pop back to prevous marks
(use-package elisp-slime-nav
  :diminish
  ;; FIXME: this does not work
  :bind (("s-c e v" . elisp-slime-nav-describe-elisp-thing-at-point))
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-slime-nav-mode))

(use-package macrostep
  :bind
  (("s-c e e" . macrostep-expand)
   ("s-c e n" . macrostep-next-macro)
   ("s-c e p" . macrostep-prev-macro)
   ("s-c e c" . macrostep-collapse)
   ("s-c e q" . macrostep-collapse-all)))

(use-package eros
  :init (eros-mode t))

(use-package ielm
  :bind
  (("s-c i" . ielm)))

(use-package elisp-mode
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
        ("s-c e s" . eval-last-sexp)
        ("s-c r" . eval-region)
        ("s-c b" . eval-buffer)
        ("s-c d" . eval-defun)))

(use-package edebug
  :bind
  (("s-c e f" . edebug-defun)))

;; ----------------------------------------------------------------------
;; esup
;; ----------------------------------------------------------------------

(use-package esup
  :commands esup
  :bind ("s-a e s" . esup))


(use-package benchmark-init
  :bind
  (("s-a b t" . benchmark-init/show-durations-tabulated)
   ("s-a b r" . benchmark-init/show-durations-tree))
  :config
  ;; To disable collection of benchmarks data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(provide 'init-elisp)
;;; init.el ends here
