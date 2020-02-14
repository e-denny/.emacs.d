(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(counsel-find-file-ignore-regexp
   "\\(\\`\\.[^.]\\|\\(?:\\.\\(?:aux\\|b\\(?:bl\\|in\\|lg\\|zr/\\)\\|c\\(?:lass\\|ps?\\)\\|d\\(?:\\(?:64fs\\|fs\\|x\\(?:\\(?:32\\|64\\)fs\\)?\\)l\\)\\|elc\\|f\\(?:asl?\\|mt\\|ns?\\|\\(?:x\\(?:\\(?:32\\|64\\)f\\)\\)?sl\\)\\|g\\(?:it/\\|[lm]o\\)\\|hg/\\|idx\\|kys?\\|l\\(?:bin\\|ib\\|o[ft]\\|x\\(?:\\(?:32\\|64\\)fsl\\)\\|[ano]\\)\\|m\\(?:em\\|o\\)\\|p\\(?:64fsl\\|fsl\\|gs?\\|y[co]\\)\\|s\\(?:o\\|parcf\\|vn/\\|x\\(?:\\(?:32\\|64\\)fsl\\)\\)\\|t\\(?:fm\\|oc\\|ps?\\)\\|ufsl\\|vrs?\\|wx\\(?:\\(?:32\\|64\\)fsl\\)\\|x86f\\|[ao]\\)\\|CVS/\\|_\\(?:\\(?:MTN\\|darcs\\)/\\)\\|~\\)\\'\\)")
 '(custom-safe-themes
   (quote
    ("585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" default)))
 '(dashboard-center-content t)
 '(dashboard-items (quote ((recents . 15) (projects . 5) (bookmarks . 5))))
 '(fci-rule-color "#383838")
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign ">")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-custom-commands
   (quote
    (("c" "Super Agenda" agenda
      (org-super-agenda-mode)
      ((org-super-agenda-groups
        (quote
         ((:name "Today" :time-grid t :scheduled today)
          (:name "Due Today" :deadline today)
          (:name "Important" :priority "A")
          (:name "Overdue" :deadline past)
          (:name "Due soon" :deadline future)
          (:name "Waiting" :todo "WAITING")))))
      (org-agenda nil "a")))))
 '(org-agenda-files
   (quote
    ("/home/edgar/Notes/BAC_ZeroPII.org" "/home/edgar/Notes/Complete.org" "/home/edgar/Notes/Inbox.org" "/home/edgar/Notes/Mesobase.org" "/home/edgar/Notes/NBA.org" "/home/edgar/Notes/NatGeo.org" "/home/edgar/Notes/Other_BAC.org" "/home/edgar/Notes/SunTrust.org" "/home/edgar/Notes/Tickler.org" "/home/edgar/Notes/Unite.org" "/home/edgar/Notes/cal.org" "/home/edgar/Notes/epsilon.org" "/home/edgar/Notes/gcal.org" "/home/edgar/Notes/personal.org" "/home/edgar/Notes/project.org")))
 '(package-selected-packages
   (quote
    (fringe-mode fringe helm-ls-git org-sidebar org-ql org-gcal org-caldav swiper-helm helm-xref helm-swoop helm-projectile helm-rg helm-org-rifle helm-org helm-flycheck helm-exwm helm-descbinds helm-ag helm doom-modeline org-mu4e counsel-projectile moody vscode-icon dired-sidebar which-key-posframe ivy-posframe avy 0blayout 0x0 vimish-fold general treemacs-magit treemacs-projectile treemacs fish-completion macrostep fish-mode elisp-slime-nav highlight-indent-guides buffer-move system-packages pdf-tools multiple-cursors browse-kill-ring flyspell-correct-ivy leuven-theme doneburn-theme dired-narrow dired-subtree smex flx eshell-prompt-extras eshell-git-prompt dimmer beacon use-package-hydra gruvbox anzu dashboard ivy-hydra counsel-org-capture-string counsel all-the-icons-ivy mu4e elfeed-goodies elfeed org-bullets sx dap-mode company-lsp lsp-ui lsp-mode rainbow-mode popwin company free-keys winum git-gutter spaceline-all-the-icons diminish all-the-icons-dired highlight-parentheses which-key rainbow-delimiters paredit guide-key smart-mode-line xelb exwm gruvbox-dark-soft-theme gruvbox-theme projectile magit use-package neotree)))
 '(send-mail-function (quote smtpmail-send-it))
 '(use-package-compute-statistics t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#FFFFFF" :foreground "#333333" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 55 :width normal :foundry "CYEL" :family "Iosevka"))))
 '(mode-line ((t (:background "dim gray" :foreground "white smoke" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "white" :weight bold))))
 '(mode-line-emphasis ((t (:foreground "white" :weight bold))))
 '(org-document-title ((t (:foreground "#171717" :weight bold :height 1.5))))
 '(powerline-active0 ((t (:inherit mode-line :background "gray20" :foreground "white smoke"))))
 '(powerline-active1 ((t (:inherit mode-line :background "gray40" :foreground "white smoke"))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray60" :foreground "black")))))
