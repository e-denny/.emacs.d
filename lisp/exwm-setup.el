;;; exwm-setup.el --- Exwm Setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Exwm Setup

;;; Code:

(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (server-start)
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 4))
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda () (exwm-workspace-rename-buffer exwm-title)))
  (add-hook 'exwm-exit-hook
            (lambda ()
              (shell-command "killall xfce-session")))
  ;; Global keybindings.
  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(([?\s-r] . exwm-reset)
            ([?\s-w] . exwm-workspace-switch)
            ;; Launch application.
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ;; 's-N': Switch to certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9)))))
  ;; (exwm-input-set-key (kbd "s-<left>") #'windmove-left)
  ;; (exwm-input-set-key (kbd "s-<down>") #'windmove-down)
  ;; (exwm-input-set-key (kbd "s-<up>") #'windmove-up)
  ;; (exwm-input-set-key (kbd "s-<right>") #'windmove-right)
  ;; (exwm-input-set-key (kbd "s-t") #'exwm-floating-toggle-floating)
  ;; ;; Firefox: open everything in new windows
  ;; 1: about:config -> browser.tabs.opentabfor.middleclick -> false
  ;; 2: place the following in chrome/userChrome.css in your FF profile:
  ;;    #tabbrowser-tabs { visibility: collapse !important; }
  (unless (get 'exwm-input-simulation-keys 'saved-value)
    (setq exwm-input-simulation-keys
          '(
            ;; movement - firefox
            ([?\C-b] . [left])
            ([?\C-f] . [right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home]) ; top of page
            ([?\C-e] . [end]) ; bottom of page
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end delete])
            ([?\C-w] . [?\C-x])
            ([?\M-w] . [?\C-c])
            ([?\C-y] . [?\C-v])
            ([?\C-s] . [?\C-f]) ; search
            ([?\C-g] . [?\C-g]) ; search again
            ([?\C-r] . [?\C-\M-r]) ; reader mode
            ([?\C-j] . [?\C-d]) ; toggle search bar / page
            ([?\C-W] . [?\C-\S-w]) ; close window
            ([?\M-n] . [?\C-n]) ; new window
            ;; Firefox extension - (Saka Key) - link hints
            ;;  - always open in new window 'fn' (not tab)
            ;; Firefox: focus search bar: C-l
            )))
  (setq save-interprogram-paste-before-kill t)
  (exwm-enable)
  (exwm-config-misc)

  (require 'exwm-randr)
;  (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "DP-1"))
;  (add-hook 'exwm-randr-screen-change-hook 'my-change-screen-hook)
  (exwm-randr-enable))

(provide 'exwm-setup)

;;; exwm-setup.el ends here
