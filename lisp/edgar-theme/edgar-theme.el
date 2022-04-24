;;; Edgar-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2021 Edgar Denny

;; Author: Edgar Denny
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(deftheme edgar "The Edgar color theme.")

(defgroup edgar-theme nil
  "Edgar theme."
  :group 'faces
  :prefix "edgar-"
  :tag "Edgar theme")

;;;###autoload
(defcustom edgar-override-colors-alist '()
  "Place to override default theme colors.
You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'edgar-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defvar edgar-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar edgar-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar edgar-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom edgar-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'edgar-theme
  :package-version '(edgar . "2.6"))

(defcustom edgar-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'edgar-theme
  :package-version '(edgar . "2.6"))

(defcustom edgar-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'edgar-theme
  :package-version '(edgar . "2.6"))

(defcustom edgar-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'edgar-theme
  :package-version '(edgar . "2.6"))

(defcustom edgar-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'edgar-theme
  :package-version '(edgar . "2.6"))

;;; Color Palette

(defvar edgar-default-colors-alist
  '(("default-fg-color" .           "gainsboro")
    ("default-bg-color" .           "gray15")
    ("background-highlight-color" . "#383838")
    ("comment-delimiter-fg-color" . "gray60")
    ("comment-fg-color" .           "grey60")
    ("region-bg-color" .            "grey30")
    ("annotation-fg-color" .        "grey50")
    ("line-number-fg-color" .       "#6F6F6F")
    ("error-fg-color" .             "red2")
    ("paren-mismatch-fg-color" .    "firebrick2")
    ("builtin-fg-color" .           "tomato2")
    ("diff-a-color" .               "indian red")
    ("search-bg-color" .            "indian red")
    ("type-fg-color" .              "light coral")
    ("constant-fg-color" .          "LightSalmon1")
    ("diff-changed-fg-color" .      "dark goldenrod")
    ("string-face-fg-color"      .  "peru")
    ("compilation-info-face" .      "orange1")
    ("variable-name-fg-color" .     "dark orange")
    ("keyword-face-fg-color" .      "SandyBrown")
    ("search-fg-color" .            "sandy brown")
    ("function-fg-color" .          "#F0DFAF")
    ("doc-fg-color"  .              "burlywood")
    ("preprocessor-fg-color".       "RosyBrown")
    ("diff-removed-fg-color" .      "#AC7373")
    ("modeline-fg-color" .          "gainsboro")
    ("popup-fg-color" .             "#000000")
    ("cursor-bg-color" .            "#FFFFEF")
    ("org-block-bg-color" .         "#494949")
    ("tooltip-bg-color" .           "#4F4F4F")
    ("diff-change-bg-color" .       "#6CA0A3")
    ("diff-c-bg-color" .            "#5C888B")
    ("selection-bg-color" .         "#4C7073")
    ("current-diff-c-bg-color" .    "#366060")
    ("org-done-fg-color"  .         "#AFD8AF")
    ("modified-fg-color"  .         "#DC8CC3"))
  "List of Edgar colors.
Each element has the form (NAME . HEX).
`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro edgar-with-color-variables (&rest body)
  "`let' bind all colors defined in `edgar-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append edgar-default-colors-alist
                           edgar-override-colors-alist))
         (z-variable-pitch (if edgar-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(edgar-with-color-variables
  (custom-theme-set-faces
   'edgar
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,keyword-face-fg-color :underline t :weight bold))))
   `(link-visited ((t (:foreground ,search-fg-color :underline t :weight normal))))
   `(default ((t (:foreground ,default-fg-color :background ,default-bg-color))))
   `(cursor ((t (:foreground ,default-fg-color :background ,cursor-bg-color))))
   `(widget-field ((t (:foreground ,default-fg-color :background ,line-number-fg-color))))
   `(escape-glyph ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(fringe ((t (:foreground ,default-fg-color :background ,default-bg-color))))
   `(header-line ((t (:foreground ,keyword-face-fg-color
                                  :background ,region-bg-color
                                  :box (:line-width -1 :style released-button)
                                  :extend t))))
   `(highlight ((t (:background ,background-highlight-color))))
   `(success ((t (:foreground ,constant-fg-color :weight bold))))
   `(warning ((t (:foreground ,variable-name-fg-color :weight bold))))
   `(tooltip ((t (:foreground ,default-fg-color :background ,tooltip-bg-color))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,keyword-face-fg-color))))
   `(compilation-enter-directory-face ((t (:foreground ,constant-fg-color))))
   `(compilation-error-face ((t (:foreground ,error-fg-color :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,default-fg-color))))
   `(compilation-info-face ((t (:foreground ,compilation-info-face))))
   `(compilation-info ((t (:foreground ,constant-fg-color :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,constant-fg-color))))
   `(compilation-line-face ((t (:foreground ,keyword-face-fg-color))))
   `(compilation-line-number ((t (:foreground ,keyword-face-fg-color))))
   `(compilation-message-face ((t (:foreground ,compilation-info-face))))
   `(compilation-warning-face ((t (:foreground ,variable-name-fg-color :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,doc-fg-color :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,string-face-fg-color :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,keyword-face-fg-color :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,annotation-fg-color))))
   `(completions-common-part ((t (:foreground ,compilation-info-face))))
   `(completions-first-difference ((t (:foreground ,cursor-bg-color))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,compilation-info-face :weight bold))))
   `(custom-group-tag ((t (:foreground ,compilation-info-face :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,constant-fg-color))))
;;;;; display-fill-column-indicator
   `(fill-column-indicator ((,class :foreground ,background-highlight-color :weight semilight)))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,default-fg-color))))
   `(grep-error-face ((t (:foreground ,error-fg-color :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,compilation-info-face))))
   `(grep-match-face ((t (:foreground ,variable-name-fg-color :weight bold))))
   `(match ((t (:background ,region-bg-color :foreground ,variable-name-fg-color :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,function-fg-color    :foreground ,region-bg-color))))
   `(hi-green   ((t (:background ,constant-fg-color :foreground ,region-bg-color))))
   `(hi-pink    ((t (:background ,modified-fg-color :foreground ,region-bg-color))))
   `(hi-yellow  ((t (:background ,keyword-face-fg-color  :foreground ,region-bg-color))))
   `(hi-blue-b  ((t (:foreground ,compilation-info-face    :weight     bold))))
   `(hi-green-b ((t (:foreground ,doc-fg-color :weight     bold))))
   `(hi-red-b   ((t (:foreground ,string-face-fg-color     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,search-fg-color :weight bold :background ,search-bg-color))))
   `(isearch-fail ((t (:foreground ,default-fg-color :background ,diff-a-color))))
   `(lazy-highlight ((t (:foreground ,search-fg-color :weight bold :background ,background-highlight-color))))

   `(menu ((t (:foreground ,default-fg-color :background ,default-bg-color))))
   `(minibuffer-prompt ((t (:foreground ,keyword-face-fg-color))))
   `(region ((,class (:background ,region-bg-color :extend t))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,search-bg-color))))
   `(trailing-whitespace ((t (:background ,string-face-fg-color))))
   `(vertical-border ((t (:foreground ,default-fg-color))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,builtin-fg-color :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,comment-fg-color))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment-delimiter-fg-color))))
   `(font-lock-constant-face ((t (:foreground ,constant-fg-color))))
   `(font-lock-doc-face ((t (:foreground ,doc-fg-color))))
   `(font-lock-function-name-face ((t (:foreground ,function-fg-color))))
   `(font-lock-keyword-face ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,preprocessor-fg-color))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,constant-fg-color :weight bold))))
   `(font-lock-string-face ((t (:foreground ,string-face-fg-color))))
   `(font-lock-type-face ((t (:foreground ,type-fg-color))))
   `(font-lock-variable-name-face ((t (:foreground ,variable-name-fg-color))))
   `(font-lock-warning-face ((t (:foreground ,search-fg-color :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,line-number-fg-color :background ,background-highlight-color))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,search-fg-color))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;;; mode-line
   `(mode-line ((,class (:foreground ,modeline-fg-color
                                     :background ,region-bg-color))))
   `(mode-line-buffer-id ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(mode-line-inactive ((t (:foreground ,comment-delimiter-fg-color
                                         :background ,default-bg-color))))
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,annotation-fg-color :background ,default-bg-color :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,doc-fg-color :background ,default-bg-color :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,annotation-fg-color :background ,default-bg-color :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; vertico
   `(vertico-current ((t (:background ,background-highlight-color :foreground ,default-fg-color :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,string-face-fg-color :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,keyword-face-fg-color))))
   `(font-latex-italic-face ((t (:foreground ,function-fg-color :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,variable-name-fg-color))))
   `(font-latex-script-char-face ((t (:foreground ,variable-name-fg-color))))

;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,line-number-fg-color :foreground ,popup-fg-color))))
   `(ac-selection-face ((t (:background ,selection-bg-color :foreground ,default-fg-color))))
   `(popup-tip-face ((t (:background ,search-fg-color :foreground ,popup-fg-color))))
   `(popup-menu-mouse-face ((t (:background ,search-fg-color :foreground ,popup-fg-color))))
   `(popup-summary-face ((t (:background ,line-number-fg-color :foreground ,popup-fg-color))))
   `(popup-scroll-bar-foreground-face ((t (:background ,current-diff-c-bg-color))))
   `(popup-scroll-bar-background-face ((t (:background ,region-bg-color))))
   `(popup-isearch-match ((t (:background ,default-bg-color :foreground ,default-fg-color))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,annotation-fg-color :background ,default-bg-color :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,org-done-fg-color :background ,default-bg-color :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,keyword-face-fg-color :background ,default-bg-color :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,paren-mismatch-fg-color :background ,default-bg-color :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,function-fg-color :background ,default-bg-color :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,default-fg-color :background ,tooltip-bg-color))))
   `(company-tooltip-annotation ((t (:foreground ,variable-name-fg-color :background ,tooltip-bg-color))))
   `(company-tooltip-annotation-selection ((t (:foreground ,variable-name-fg-color :background ,region-bg-color))))
   `(company-tooltip-selection ((t (:foreground ,default-fg-color :background ,background-highlight-color))))
   `(company-tooltip-mouse ((t (:background ,region-bg-color))))
   `(company-tooltip-common ((t (:foreground ,doc-fg-color))))
   `(company-tooltip-common-selection ((t (:foreground ,doc-fg-color))))
   `(company-scrollbar-fg ((t (:background ,region-bg-color))))
   `(company-scrollbar-bg ((t (:background ,search-bg-color))))
   `(company-preview ((t (:background ,doc-fg-color))))
   `(company-preview-common ((t (:foreground ,doc-fg-color :background ,region-bg-color))))
;;;;; bm
   `(bm-face ((t (:background ,diff-changed-fg-color :foreground ,default-bg-color))))
   `(bm-fringe-face ((t (:background ,diff-changed-fg-color :foreground ,default-bg-color))))
   `(bm-fringe-persistent-face ((t (:background ,comment-delimiter-fg-color :foreground ,default-bg-color))))
   `(bm-persistent-face ((t (:background ,comment-delimiter-fg-color :foreground ,default-bg-color))))

;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,default-fg-color)))
   `(context-coloring-level-1-face ((t :foreground ,function-fg-color)))
   `(context-coloring-level-2-face ((t :foreground ,constant-fg-color)))
   `(context-coloring-level-3-face ((t :foreground ,keyword-face-fg-color)))
   `(context-coloring-level-4-face ((t :foreground ,variable-name-fg-color)))
   `(context-coloring-level-5-face ((t :foreground ,modified-fg-color)))
   `(context-coloring-level-6-face ((t :foreground ,preprocessor-fg-color)))
   `(context-coloring-level-7-face ((t :foreground ,doc-fg-color)))
   `(context-coloring-level-8-face ((t :foreground ,search-fg-color)))
   `(context-coloring-level-9-face ((t :foreground ,paren-mismatch-fg-color)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,compilation-info-face :foreground ,default-bg-color))))
   `(ctbl:face-continue-bar ((t (:background ,background-highlight-color :foreground ,default-bg-color))))
   `(ctbl:face-row-select ((t (:background ,function-fg-color :foreground ,default-bg-color))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,annotation-fg-color))))
   `(debbugs-gnu-handled ((t (:foreground ,constant-fg-color))))
   `(debbugs-gnu-new ((t (:foreground ,string-face-fg-color))))
   `(debbugs-gnu-pending ((t (:foreground ,compilation-info-face))))
   `(debbugs-gnu-stale ((t (:foreground ,variable-name-fg-color))))
   `(debbugs-gnu-tagged ((t (:foreground ,string-face-fg-color))))
;;;;; diff
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(diff-added          ((t (:background "#335533" :foreground ,constant-fg-color))))
   `(diff-changed        ((t (:background "#555511" :foreground ,diff-changed-fg-color))))
   `(diff-removed        ((t (:background "#553333" :foreground ,diff-removed-fg-color))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,constant-fg-color))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,keyword-face-fg-color))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,string-face-fg-color))))
   `(diff-header ((,class (:background ,search-bg-color))
                  (t (:background ,default-fg-color :foreground ,default-bg-color))))
   `(diff-file-header
     ((,class (:background ,search-bg-color :foreground ,default-fg-color :weight bold))
      (t (:background ,default-fg-color :foreground ,default-bg-color :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,compilation-info-face :background ,diff-change-bg-color))))
   `(diff-hl-delete ((,class (:foreground ,paren-mismatch-fg-color :background ,error-fg-color))))
   `(diff-hl-insert ((,class (:foreground ,modeline-fg-color :background ,comment-delimiter-fg-color))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,tooltip-bg-color)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,compilation-info-face))))
   `(diredp-compressed-file-suffix ((t (:foreground ,variable-name-fg-color))))
   `(diredp-date-time ((t (:foreground ,modified-fg-color))))
   `(diredp-deletion ((t (:foreground ,keyword-face-fg-color))))
   `(diredp-deletion-file-name ((t (:foreground ,string-face-fg-color))))
   `(diredp-dir-heading ((t (:foreground ,compilation-info-face :background ,region-bg-color))))
   `(diredp-dir-priv ((t (:foreground ,function-fg-color))))
   `(diredp-exec-priv ((t (:foreground ,string-face-fg-color))))
   `(diredp-executable-tag ((t (:foreground ,modeline-fg-color))))
   `(diredp-file-name ((t (:foreground ,compilation-info-face))))
   `(diredp-file-suffix ((t (:foreground ,constant-fg-color))))
   `(diredp-flag-mark ((t (:foreground ,keyword-face-fg-color))))
   `(diredp-flag-mark-line ((t (:foreground ,variable-name-fg-color))))
   `(diredp-ignored-file-name ((t (:foreground ,string-face-fg-color))))
   `(diredp-link-priv ((t (:foreground ,keyword-face-fg-color))))
   `(diredp-mode-line-flagged ((t (:foreground ,keyword-face-fg-color))))
   `(diredp-mode-line-marked ((t (:foreground ,variable-name-fg-color))))
   `(diredp-no-priv ((t (:foreground ,default-fg-color))))
   `(diredp-number ((t (:foreground ,modeline-fg-color))))
   `(diredp-other-priv ((t (:foreground ,diff-changed-fg-color))))
   `(diredp-rare-priv ((t (:foreground ,error-fg-color))))
   `(diredp-read-priv ((t (:foreground ,comment-delimiter-fg-color))))
   `(diredp-symlink ((t (:foreground ,keyword-face-fg-color))))
   `(diredp-write-priv ((t (:foreground ,modified-fg-color))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,string-face-fg-color :weight bold))))
   `(dired-async-message ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,keyword-face-fg-color))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,variable-name-fg-color))))
   `(diredfl-date-time ((t (:foreground ,modified-fg-color))))
   `(diredfl-deletion ((t (:foreground ,keyword-face-fg-color))))
   `(diredfl-deletion-file-name ((t (:foreground ,string-face-fg-color))))
   `(diredfl-dir-heading ((t (:foreground ,compilation-info-face :background ,region-bg-color))))
   `(diredfl-dir-priv ((t (:foreground ,function-fg-color))))
   `(diredfl-exec-priv ((t (:foreground ,string-face-fg-color))))
   `(diredfl-executable-tag ((t (:foreground ,modeline-fg-color))))
   `(diredfl-file-name ((t (:foreground ,compilation-info-face))))
   `(diredfl-file-suffix ((t (:foreground ,constant-fg-color))))
   `(diredfl-flag-mark ((t (:foreground ,keyword-face-fg-color))))
   `(diredfl-flag-mark-line ((t (:foreground ,variable-name-fg-color))))
   `(diredfl-ignored-file-name ((t (:foreground ,string-face-fg-color))))
   `(diredfl-link-priv ((t (:foreground ,keyword-face-fg-color))))
   `(diredfl-no-priv ((t (:foreground ,default-fg-color))))
   `(diredfl-number ((t (:foreground ,modeline-fg-color))))
   `(diredfl-other-priv ((t (:foreground ,diff-changed-fg-color))))
   `(diredfl-rare-priv ((t (:foreground ,error-fg-color))))
   `(diredfl-read-priv ((t (:foreground ,comment-fg-color))))
   `(diredfl-symlink ((t (:foreground ,keyword-face-fg-color))))
   `(diredfl-write-priv ((t (:foreground ,modified-fg-color))))

;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,default-fg-color :background ,diff-a-color))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,default-fg-color :background ,diff-a-color))))
   `(ediff-current-diff-B ((t (:foreground ,default-fg-color :background ,comment-delimiter-fg-color))))
   `(ediff-current-diff-C ((t (:foreground ,default-fg-color :background ,current-diff-c-bg-color))))
   `(ediff-even-diff-A ((t (:background ,tooltip-bg-color))))
   `(ediff-even-diff-Ancestor ((t (:background ,tooltip-bg-color))))
   `(ediff-even-diff-B ((t (:background ,tooltip-bg-color))))
   `(ediff-even-diff-C ((t (:background ,tooltip-bg-color))))
   `(ediff-fine-diff-A ((t (:foreground ,default-fg-color :background ,diff-removed-fg-color :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,default-fg-color :background ,diff-removed-fg-color weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,default-fg-color :background ,constant-fg-color :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,default-fg-color :background ,diff-c-bg-color :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,search-bg-color))))
   `(ediff-odd-diff-Ancestor ((t (:background ,search-bg-color))))
   `(ediff-odd-diff-B ((t (:background ,search-bg-color))))
   `(ediff-odd-diff-C ((t (:background ,search-bg-color))))

;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,keyword-face-fg-color :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,search-fg-color
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,error-fg-color :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,keyword-face-fg-color
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,doc-fg-color :background ,default-bg-color))))
   `(w3m-lnum-match ((t (:background ,region-bg-color
                                     :foreground ,variable-name-fg-color
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,keyword-face-fg-color))))

;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,constant-fg-color :background ,default-bg-color))))
   `(ert-test-result-unexpected ((t (:foreground ,string-face-fg-color :background ,default-bg-color))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,error-fg-color :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,preprocessor-fg-color :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,paren-mismatch-fg-color :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,default-fg-color))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,function-fg-color :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,doc-fg-color :weight bold))))

;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,string-face-fg-color)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,error-fg-color :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,variable-name-fg-color)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,variable-name-fg-color :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,constant-fg-color)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,comment-delimiter-fg-color :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,variable-name-fg-color) :inherit unspecified))
      (t (:foreground ,variable-name-fg-color :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,string-face-fg-color) :inherit unspecified))
      (t (:foreground ,error-fg-color :weight bold :underline t))))

;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,modeline-fg-color :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,preprocessor-fg-color  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,preprocessor-fg-color  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,constant-fg-color  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,keyword-face-fg-color  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,constant-fg-color :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,string-face-fg-color :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,modified-fg-color :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,default-fg-color :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,constant-fg-color  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,string-face-fg-color :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,modified-fg-color :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, variable-name-fg-color))))

;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,compilation-info-face))))
   `(guide-key/key-face ((t (:foreground ,constant-fg-color))))
   `(guide-key/prefix-command-face ((t (:foreground ,modeline-fg-color))))

;;;;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,compilation-info-face))))
;;;;; highlight-symbol
   `(highlight-symbol-face ((t (:background ,search-bg-color))))
;;;;; highlight-thing
   `(highlight-thing ((t (:background ,search-bg-color))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,background-highlight-color))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,background-highlight-color :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,tooltip-bg-color))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,error-fg-color :background ,default-bg-color))))
   `(hydra-face-amaranth ((t (:foreground ,string-face-fg-color :background ,default-bg-color))))
   `(hydra-face-blue ((t (:foreground ,compilation-info-face :background ,default-bg-color))))
   `(hydra-face-pink ((t (:foreground ,modified-fg-color :background ,default-bg-color))))
   `(hydra-face-teal ((t (:foreground ,function-fg-color :background ,default-bg-color))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,region-bg-color :foreground ,variable-name-fg-color))))
   `(info-constant-ref-item ((t (:background ,region-bg-color :foreground ,modified-fg-color))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,region-bg-color :foreground ,keyword-face-fg-color))))
   `(info-function-ref-item ((t (:background ,region-bg-color :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,region-bg-color :foreground ,keyword-face-fg-color))))
   `(info-menu ((t (:foreground ,keyword-face-fg-color))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,region-bg-color))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,region-bg-color :foreground ,keyword-face-fg-color))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,region-bg-color :foreground ,preprocessor-fg-color))))
   `(info-user-option-ref-item ((t (:background ,region-bg-color :foreground ,string-face-fg-color))))
   `(info-variable-ref-item ((t (:background ,region-bg-color :foreground ,variable-name-fg-color))))

;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,search-bg-color :weight bold))))

;;;;; js2-mode
   `(js2-warning ((t (:underline ,variable-name-fg-color))))
   `(js2-error ((t (:foreground ,string-face-fg-color :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,comment-delimiter-fg-color))))
   `(js2-jsdoc-type ((t (:foreground ,doc-fg-color))))
   `(js2-jsdoc-value ((t (:foreground ,org-done-fg-color))))
   `(js2-function-param ((t (:foreground, variable-name-fg-color))))
   `(js2-external-variable ((t (:foreground ,variable-name-fg-color))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,comment-delimiter-fg-color))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,variable-name-fg-color))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,error-fg-color))))
   `(js2-object-property ((t (:foreground ,preprocessor-fg-color))))
   `(js2-magic-paren ((t (:foreground ,current-diff-c-bg-color))))
   `(js2-private-function-call ((t (:foreground ,function-fg-color))))
   `(js2-function-call ((t (:foreground ,function-fg-color))))
   `(js2-private-member ((t (:foreground ,type-fg-color))))
   `(js2-keywords ((t (:foreground ,modified-fg-color))))

;;;;; linum-mode
   `(linum ((t (:foreground ,doc-fg-color :background ,default-bg-color))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,background-highlight-color :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,default-bg-color :background ,default-fg-color))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,keyword-face-fg-color))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,default-fg-color))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,keyword-face-fg-color))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,keyword-face-fg-color :box t))))
   `(ruler-mode-default ((t (:foreground ,doc-fg-color :background ,default-bg-color))))


;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,doc-fg-color :background ,region-bg-color))))
   `(macrostep-gensym-2
     ((t (:foreground ,paren-mismatch-fg-color :background ,region-bg-color))))
   `(macrostep-gensym-3
     ((t (:foreground ,preprocessor-fg-color :background ,region-bg-color))))
   `(macrostep-gensym-4
     ((t (:foreground ,modified-fg-color :background ,region-bg-color))))
   `(macrostep-gensym-5
     ((t (:foreground ,keyword-face-fg-color :background ,region-bg-color))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(magit-section-highlight           ((t (:background ,org-block-bg-color))))
   `(magit-section-heading             ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,variable-name-fg-color :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,org-block-bg-color :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,org-block-bg-color :weight bold
                                                        :foreground ,variable-name-fg-color))))
   `(magit-diff-added                  ((t (:background ,comment-delimiter-fg-color))))
   `(magit-diff-added-highlight        ((t (:background ,constant-fg-color))))
   `(magit-diff-removed                ((t (:background ,diff-a-color))))
   `(magit-diff-removed-highlight      ((t (:background ,string-face-fg-color))))
   `(magit-diff-hunk-heading           ((t (:background ,tooltip-bg-color))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,search-bg-color))))
   `(magit-diff-hunk-heading-selection ((t (:background ,search-bg-color
                                                        :foreground ,variable-name-fg-color))))
   `(magit-diff-lines-heading          ((t (:background ,variable-name-fg-color
                                                        :foreground ,search-bg-color))))
   `(magit-diff-context-highlight      ((t (:background ,org-block-bg-color
                                                        :foreground ,comment-fg-color))))
   `(magit-diffstat-added              ((t (:foreground ,constant-fg-color))))
   `(magit-diffstat-removed            ((t (:foreground ,string-face-fg-color))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,keyword-face-fg-color  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,comment-delimiter-fg-color :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,constant-fg-color   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,annotation-fg-color    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,diff-change-bg-color  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,constant-fg-color  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,string-face-fg-color    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,variable-name-fg-color))))
   `(magit-log-date      ((t (:foreground ,annotation-fg-color))))
   `(magit-log-graph     ((t (:foreground ,cursor-bg-color))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,search-fg-color))))
   `(magit-sequence-stop ((t (:foreground ,constant-fg-color))))
   `(magit-sequence-part ((t (:foreground ,keyword-face-fg-color))))
   `(magit-sequence-head ((t (:foreground ,compilation-info-face))))
   `(magit-sequence-drop ((t (:foreground ,string-face-fg-color))))
   `(magit-sequence-done ((t (:foreground ,annotation-fg-color))))
   `(magit-sequence-onto ((t (:foreground ,annotation-fg-color))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,constant-fg-color))))
   `(magit-bisect-skip ((t (:foreground ,keyword-face-fg-color))))
   `(magit-bisect-bad  ((t (:foreground ,string-face-fg-color))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,region-bg-color :foreground ,diff-change-bg-color))))
   `(magit-blame-hash    ((t (:background ,region-bg-color :foreground ,diff-change-bg-color))))
   `(magit-blame-name    ((t (:background ,region-bg-color :foreground ,variable-name-fg-color))))
   `(magit-blame-date    ((t (:background ,region-bg-color :foreground ,variable-name-fg-color))))
   `(magit-blame-summary ((t (:background ,region-bg-color :foreground ,diff-change-bg-color
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,line-number-fg-color))))
   `(magit-hash           ((t (:foreground ,line-number-fg-color))))
   `(magit-tag            ((t (:foreground ,variable-name-fg-color :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,constant-fg-color  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,compilation-info-face   :weight bold))))
   `(magit-branch-current ((t (:foreground ,compilation-info-face   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,compilation-info-face   :weight bold))))
   `(magit-refname        ((t (:background ,search-bg-color :foreground ,default-fg-color :weight bold))))
   `(magit-refname-stash  ((t (:background ,search-bg-color :foreground ,default-fg-color :weight bold))))
   `(magit-refname-wip    ((t (:background ,search-bg-color :foreground ,default-fg-color :weight bold))))
   `(magit-signature-good      ((t (:foreground ,constant-fg-color))))
   `(magit-signature-bad       ((t (:foreground ,string-face-fg-color))))
   `(magit-signature-untrusted ((t (:foreground ,keyword-face-fg-color))))
   `(magit-signature-expired   ((t (:foreground ,variable-name-fg-color))))
   `(magit-signature-revoked   ((t (:foreground ,modified-fg-color))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,function-fg-color))))
   `(magit-cherry-equivalent   ((t (:foreground ,modified-fg-color))))
   `(magit-reflog-commit       ((t (:foreground ,constant-fg-color))))
   `(magit-reflog-amend        ((t (:foreground ,modified-fg-color))))
   `(magit-reflog-merge        ((t (:foreground ,constant-fg-color))))
   `(magit-reflog-checkout     ((t (:foreground ,compilation-info-face))))
   `(magit-reflog-reset        ((t (:foreground ,string-face-fg-color))))
   `(magit-reflog-rebase       ((t (:foreground ,modified-fg-color))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,constant-fg-color))))
   `(magit-reflog-remote       ((t (:foreground ,function-fg-color))))
   `(magit-reflog-other        ((t (:foreground ,function-fg-color))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,preprocessor-fg-color))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,keyword-face-fg-color))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,search-fg-color :underline t))))
   `(markup-list-face ((t (:foreground ,cursor-bg-color))))
   `(markup-meta-face ((t (:foreground ,keyword-face-fg-color))))
   `(markup-meta-hide-face ((t (:foreground ,keyword-face-fg-color))))
   `(markup-secondary-text-face ((t (:foreground ,diff-changed-fg-color))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,keyword-face-fg-color))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,modeline-fg-color))))
   `(message-header-other ((t (:foreground ,constant-fg-color))))
   `(message-header-to ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(message-header-cc ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(message-header-subject ((t (:foreground ,variable-name-fg-color :weight bold))))
   `(message-header-xheader ((t (:foreground ,constant-fg-color))))
   `(message-mml ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))

;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,function-fg-color :background ,default-bg-color :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,default-bg-color :background ,modified-fg-color :weight bold))))
   `(paren-face-no-match ((t (:foreground ,default-bg-color :background ,string-face-fg-color :weight bold))))

;;;;; nav
   `(nav-face-heading ((t (:foreground ,keyword-face-fg-color))))
   `(nav-face-button-num ((t (:foreground ,function-fg-color))))
   `(nav-face-dir ((t (:foreground ,constant-fg-color))))
   `(nav-face-hdir ((t (:foreground ,string-face-fg-color))))
   `(nav-face-file ((t (:foreground ,default-fg-color))))
   `(nav-face-hfile ((t (:foreground ,diff-a-color))))

;;;;; orderless
   `(orderless-match-face-0 ((t (:foreground ,constant-fg-color))))
   `(orderless-match-face-1 ((t (:foreground ,modified-fg-color))))
   `(orderless-match-face-2 ((t (:foreground ,compilation-info-face))))
   `(orderless-match-face-3 ((t (:foreground ,variable-name-fg-color))))

;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,cursor-bg-color :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,default-fg-color :weight bold))))
   `(org-block ((t (:background ,org-block-bg-color :extend t))))
   `(org-checkbox ((t (:background ,search-bg-color :foreground ,cursor-bg-color
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,compilation-info-face :underline t))))
   `(org-deadline-announce ((t (:foreground ,error-fg-color))))
   `(org-done ((t (:weight bold :weight bold :foreground ,org-done-fg-color))))
   `(org-formula ((t (:foreground ,search-fg-color))))
   `(org-headline-done ((t (:foreground ,org-done-fg-color))))
   `(org-hide ((t (:foreground ,default-bg-color))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,variable-name-fg-color
                               ,@(when edgar-scale-org-headlines
                                   (list :height edgar-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,constant-fg-color
                               ,@(when edgar-scale-org-headlines
                                   (list :height edgar-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,type-fg-color
                               ,@(when edgar-scale-org-headlines
                                   (list :height edgar-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,search-fg-color
                               ,@(when edgar-scale-org-headlines
                                   (list :height edgar-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,function-fg-color))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,doc-fg-color))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,diff-a-color))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,selection-bg-color))))
   `(org-link ((t (:foreground ,search-fg-color :underline t))))
   `(org-quote ((t (:background ,org-block-bg-color :extend t))))
   `(org-scheduled ((t (:foreground ,constant-fg-color))))
   `(org-scheduled-previously ((t (:foreground ,string-face-fg-color))))
   `(org-scheduled-today ((t (:foreground ,preprocessor-fg-color))))
   `(org-sexp-date ((t (:foreground ,preprocessor-fg-color :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,doc-fg-color))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,variable-name-fg-color))))
   `(org-todo ((t (:weight bold :foreground ,string-face-fg-color :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,string-face-fg-color :weight bold :underline nil))))
   `(org-column ((t (:background ,region-bg-color))))
   `(org-column-title ((t (:background ,region-bg-color :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,default-fg-color :background ,region-bg-color))))
   `(org-mode-line-clock-overrun ((t (:foreground ,default-bg-color :background ,error-fg-color))))
   `(org-ellipsis ((t (:foreground ,diff-changed-fg-color :underline t))))
   `(org-footnote ((t (:foreground ,function-fg-color :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,compilation-info-face
                                      :weight bold
                                      ,@(when edgar-scale-org-headlines
                                          (list :height edgar-height-plus-4))))))
   `(org-document-info ((t (:foreground ,compilation-info-face))))
   `(org-habit-ready-face ((t :background ,constant-fg-color)))
   `(org-habit-alert-face ((t :background ,diff-changed-fg-color :foreground ,default-bg-color)))
   `(org-habit-clear-face ((t :background ,diff-c-bg-color)))
   `(org-habit-overdue-face ((t :background ,string-face-fg-color)))
   `(org-habit-clear-future-face ((t :background ,selection-bg-color)))
   `(org-habit-ready-future-face ((t :background ,comment-delimiter-fg-color)))
   `(org-habit-alert-future-face ((t :background ,search-fg-color :foreground ,default-bg-color)))
   `(org-habit-overdue-future-face ((t :background ,diff-a-color)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,variable-name-fg-color
                             ,@(when edgar-scale-outline-headlines
                                 (list :height edgar-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,constant-fg-color
                             ,@(when edgar-scale-outline-headlines
                                 (list :height edgar-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,type-fg-color
                             ,@(when edgar-scale-outline-headlines
                                 (list :height edgar-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,search-fg-color
                             ,@(when edgar-scale-outline-headlines
                                 (list :height edgar-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,function-fg-color))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,doc-fg-color))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,diff-a-color))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,selection-bg-color))))

;;;;; paren-face
   `(parenthesis ((t (:foreground ,annotation-fg-color))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,search-fg-color))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,default-fg-color))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,constant-fg-color))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,search-fg-color))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,function-fg-color))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,doc-fg-color))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,preprocessor-fg-color))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,diff-changed-fg-color))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,modeline-fg-color))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,diff-change-bg-color))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,variable-name-fg-color))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,constant-fg-color))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,current-diff-c-bg-color))))

;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,default-bg-color :background ,modified-fg-color))))
   `(reb-match-1 ((t (:foreground ,default-bg-color :background ,compilation-info-face))))
   `(reb-match-2 ((t (:foreground ,default-bg-color :background ,variable-name-fg-color))))
   `(reb-match-3 ((t (:foreground ,default-bg-color :background ,string-face-fg-color))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,constant-fg-color))))
   `(realgud-overlay-arrow2 ((t (:foreground ,keyword-face-fg-color))))
   `(realgud-overlay-arrow3 ((t (:foreground ,variable-name-fg-color))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,string-face-fg-color :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color ,comment-fg-color :style nil)))))
   `(realgud-line-number ((t (:foreground ,keyword-face-fg-color))))
   `(realgud-backtrace-number ((t (:foreground ,keyword-face-fg-color, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,selection-bg-color :weight bold))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,variable-name-fg-color))))
   `(rst-level-2-face ((t (:foreground ,modeline-fg-color))))
   `(rst-level-3-face ((t (:foreground ,type-fg-color))))
   `(rst-level-4-face ((t (:foreground ,search-fg-color))))
   `(rst-level-5-face ((t (:foreground ,function-fg-color))))
   `(rst-level-6-face ((t (:foreground ,comment-delimiter-fg-color))))
;;;;; selectrum
   `(selectrum-current-candidate ((t (:foreground ,keyword-face-fg-color :weight bold :underline t))))
   `(selectrum-primary-highlight ((t (:background ,comment-delimiter-fg-color))))
   `(selectrum-secondary-highlight ((t (:background ,constant-fg-color))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,keyword-face-fg-color :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,string-face-fg-color))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,paren-mismatch-fg-color :background ,line-number-fg-color :weight bold))))
   `(show-paren-match ((t (:foreground ,default-fg-color :background ,line-number-fg-color :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,paren-mismatch-fg-color :background ,line-number-fg-color :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,line-number-fg-color :weight bold))))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,string-face-fg-color))))
   `(slime-repl-inputed-output-face ((t (:foreground ,constant-fg-color))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,string-face-fg-color)))
      (t
       (:underline ,string-face-fg-color))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,variable-name-fg-color)))
      (t
       (:underline ,variable-name-fg-color))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,keyword-face-fg-color)))
      (t
       (:underline ,keyword-face-fg-color))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,constant-fg-color)))
      (t
       (:underline ,constant-fg-color))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; swiper
   `(swiper-line-face ((t (:underline t))))
;;;;; sx
   `(sx-custom-button
     ((t (:background ,default-fg-color :foreground ,region-bg-color
                      :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
     ((t (:foreground ,org-done-fg-color
                      :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((t (:foreground ,org-done-fg-color
                      :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((t (:box (:color ,region-bg-color :line-width 3 :style released-button)
               :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,default-fg-color
                                    :background ,default-bg-color))))
   `(tabbar-selected ((t (:foreground ,default-fg-color
                                      :background ,default-bg-color
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,default-fg-color
                                        :background ,tooltip-bg-color
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,default-bg-color
                                       :background ,region-bg-color))))
   `(term-color-red ((t (:foreground ,diff-removed-fg-color
                                     :background ,diff-a-color))))
   `(term-color-green ((t (:foreground ,constant-fg-color
                                       :background ,doc-fg-color))))
   `(term-color-yellow ((t (:foreground ,variable-name-fg-color
                                        :background ,keyword-face-fg-color))))
   `(term-color-blue ((t (:foreground ,type-fg-color
                                      :background ,selection-bg-color))))
   `(term-color-magenta ((t (:foreground ,modified-fg-color
                                         :background ,string-face-fg-color))))
   `(term-color-cyan ((t (:foreground ,function-fg-color
                                      :background ,compilation-info-face))))
   `(term-color-white ((t (:foreground ,default-fg-color
                                       :background ,annotation-fg-color))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,cursor-bg-color :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,error-fg-color :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,default-fg-color))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,keyword-face-fg-color))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,function-fg-color))))
;;;;; vertico
   `(vertico-current ((t (:foreground ,keyword-face-fg-color :weight bold :underline t))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,default-bg-color :background ,constant-fg-color :weight bold))))
   `(vr/group-1 ((t (:foreground ,default-bg-color :background ,variable-name-fg-color :weight bold))))
   `(vr/group-2 ((t (:foreground ,default-bg-color :background ,compilation-info-face :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,search-fg-color :background ,region-bg-color :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,string-face-fg-color :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,background-highlight-color))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,variable-name-fg-color ))))
   `(web-mode-css-prop-face ((t (:foreground ,variable-name-fg-color))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,org-done-fg-color :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,compilation-info-face))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,compilation-info-face))))
   `(web-mode-html-attr-name-face ((t (:foreground ,variable-name-fg-color))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,function-fg-color))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,default-bg-color))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,string-face-fg-color))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,tooltip-bg-color :foreground ,tooltip-bg-color))))
   `(whitespace-hspace ((t (:background ,tooltip-bg-color :foreground ,tooltip-bg-color))))
   `(whitespace-tab ((t (:background ,error-fg-color))))
   `(whitespace-newline ((t (:foreground ,tooltip-bg-color))))
   `(whitespace-trailing ((t (:background ,string-face-fg-color))))
   `(whitespace-line ((t (:background ,default-bg-color :foreground ,modified-fg-color))))
   `(whitespace-space-before-tab ((t (:background ,variable-name-fg-color :foreground ,variable-name-fg-color))))
   `(whitespace-indentation ((t (:background ,keyword-face-fg-color :foreground ,string-face-fg-color))))
   `(whitespace-empty ((t (:background ,keyword-face-fg-color))))
   `(whitespace-space-after-tab ((t (:background ,keyword-face-fg-color :foreground ,string-face-fg-color))))
   ))

;;; Theme Variables
(edgar-with-color-variables
  (custom-theme-set-variables
   'edgar
;;;;; ansi-color
   `(ansi-color-names-vector [,default-bg-color ,string-face-fg-color ,constant-fg-color ,keyword-face-fg-color
                               ,compilation-info-face ,modified-fg-color ,function-fg-color ,default-fg-color])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,tooltip-bg-color)
   `(company-quickhelp-color-foreground ,default-fg-color)
;;;;; fill-column-indicator
   `(fci-rule-color ,background-highlight-color)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,string-face-fg-color ,variable-name-fg-color ,keyword-face-fg-color ,constant-fg-color ,constant-fg-color
                             ,function-fg-color ,preprocessor-fg-color ,modified-fg-color))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,default-fg-color . ,background-highlight-color))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,error-fg-color)
       ( 40. . ,string-face-fg-color)
       ( 60. . ,variable-name-fg-color)
       ( 80. . ,search-fg-color)
       (100. . ,diff-changed-fg-color)
       (120. . ,keyword-face-fg-color)
       (140. . ,comment-delimiter-fg-color)
       (160. . ,constant-fg-color)
       (180. . ,modeline-fg-color)
       (200. . ,doc-fg-color)
       (220. . ,org-done-fg-color)
       (240. . ,constant-fg-color)
       (260. . ,function-fg-color)
       (280. . ,diff-change-bg-color)
       (300. . ,type-fg-color)
       (320. . ,compilation-info-face)
       (340. . ,preprocessor-fg-color)
       (360. . ,modified-fg-color)))
   `(vc-annotate-very-old-color ,modified-fg-color)
   `(vc-annotate-background ,region-bg-color)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defcustom edgar-add-font-lock-keywords nil
  "Whether to add font-lock keywords for edgar color names.
In buffers visiting library `edgar-theme.el' the edgar
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
defined).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
edgar-specific font-lock keywords to be used.
In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'."
  :type 'boolean
  :group 'edgar-theme)

(defvar edgar-colors-font-lock-keywords nil)

(defun edgar--rainbow-turn-on ()
  "Maybe also add font-lock keywords for edgar colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or edgar-add-font-lock-keywords
                 (and (buffer-file-name)
                      (equal (file-name-nondirectory (buffer-file-name))
                             "edgar-theme.el"))))
    (unless edgar-colors-font-lock-keywords
      (setq edgar-colors-font-lock-keywords
            `((,(regexp-opt (mapcar 'car edgar-default-colors-alist) 'words)
               (0 (rainbow-colorize-by-assoc edgar-default-colors-alist))))))
    (font-lock-add-keywords nil edgar-colors-font-lock-keywords 'end)))

(defun edgar--rainbow-turn-off ()
  "Also remove font-lock keywords for edgar colors."
  (font-lock-remove-keywords nil edgar-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'edgar--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'edgar--rainbow-turn-off))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'edgar)
;;; edgar-theme.el ends here
