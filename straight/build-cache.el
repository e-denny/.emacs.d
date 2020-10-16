
:kak

"28.0.50"

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("org-elpa" ("2020-10-15 20:48:42" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2020-10-15 20:48:42" nil (:type git :host github :repo "melpa/melpa" :no-build t :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2020-10-15 20:48:42" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :no-build t :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "emacsmirror-mirror" ("2020-10-15 20:48:42" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :no-build t :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2020-10-15 20:48:42" ("emacs") (:type git :host github :repo "raxod502/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "evil" ("2020-10-15 20:48:42" ("emacs" "undo-tree" "goto-chg" "cl-lib") (:type git :flavor melpa :files (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el") "evil-pkg.el") :host github :repo "emacs-evil/evil" :package "evil" :local-repo "evil")) "undo-tree" ("2020-10-15 20:48:42" nil (:type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git")) :package "undo-tree" :local-repo "undo-tree")) "goto-chg" ("2020-10-15 20:48:42" ("undo-tree") (:type git :flavor melpa :host github :repo "emacs-evil/goto-chg" :package "goto-chg" :local-repo "goto-chg")) "evil-surround" ("2020-10-15 20:48:42" ("evil") (:type git :flavor melpa :host github :repo "emacs-evil/evil-surround" :package "evil-surround" :local-repo "evil-surround")) "evil-indent-textobject" ("2020-10-15 20:48:42" ("evil") (:type git :flavor melpa :host github :repo "cofi/evil-indent-textobject" :package "evil-indent-textobject" :local-repo "evil-indent-textobject")) "evil-org" ("2020-09-19 21:01:40" ("emacs" "evil") (:type git :flavor melpa :host github :repo "Somelauw/evil-org-mode" :package "evil-org" :local-repo "evil-org-mode")) "doom-themes" ("2020-10-15 20:48:44" ("emacs" "cl-lib") (:type git :flavor melpa :files (:defaults "themes/*.el" "doom-themes-pkg.el") :host github :repo "hlissner/emacs-doom-themes" :package "doom-themes" :local-repo "emacs-doom-themes")) "org-roam" ("2020-09-19 21:01:43" ("emacs" "dash" "f" "s" "org" "emacsql" "emacsql-sqlite") (:host github :repo "jethrokuan/org-roam" :branch "master" :package "org-roam" :type git :local-repo "org-roam")) "dash" ("2020-10-15 20:48:42" nil (:type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el" :package "dash" :local-repo "dash.el")) "f" ("2020-10-15 20:48:42" ("s" "dash") (:type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el" :package "f" :local-repo "f.el")) "s" ("2020-10-15 20:48:42" nil (:type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el" :package "s" :local-repo "s.el")) "org" ("2020-09-19 21:01:43" nil (:type git :repo "https://code.orgmode.org/bzg/org-mode.git" :local-repo "org" :package "org")) "emacsql" ("2020-09-19 21:01:43" ("emacs") (:type git :flavor melpa :files ("emacsql.el" "emacsql-compiler.el" "emacsql-system.el" "README.md" "emacsql-pkg.el") :host github :repo "skeeto/emacsql" :package "emacsql" :local-repo "emacsql")) "emacsql-sqlite" ("2020-09-19 21:01:43" ("emacs" "emacsql") (:flavor melpa :files ("emacsql-sqlite.el" "sqlite" "emacsql-sqlite-pkg.el") :package "emacsql-sqlite" :local-repo "emacsql" :type git :repo "skeeto/emacsql" :host github)) "use-package" ("2020-10-15 20:48:42" ("emacs" "bind-key") (:type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package" :package "use-package" :local-repo "use-package")) "bind-key" ("2020-10-15 20:48:42" nil (:flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :package "bind-key" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "diminish" ("2020-10-15 20:48:42" ("emacs") (:type git :flavor melpa :host github :repo "myrjola/diminish.el" :package "diminish" :local-repo "diminish.el")) "avy" ("2020-10-15 20:48:42" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "abo-abo/avy" :package "avy" :local-repo "avy")) "browse-kill-ring" ("2020-10-15 20:48:42" nil (:type git :flavor melpa :host github :repo "browse-kill-ring/browse-kill-ring" :package "browse-kill-ring" :local-repo "browse-kill-ring")) "company" ("2020-10-15 20:48:42" ("emacs") (:type git :flavor melpa :host github :repo "company-mode/company-mode" :package "company" :local-repo "company-mode")) "company-box" ("2020-10-15 20:48:42" ("emacs" "dash" "dash-functional" "company" "frame-local") (:type git :flavor melpa :files (:defaults "images" "company-box-pkg.el") :host github :repo "sebastiencs/company-box" :package "company-box" :local-repo "company-box")) "dash-functional" ("2020-10-15 20:48:42" ("dash" "emacs") (:flavor melpa :files ("dash-functional.el" "dash-functional-pkg.el") :package "dash-functional" :local-repo "dash.el" :type git :repo "magnars/dash.el" :host github)) "frame-local" ("2020-10-15 20:48:42" ("emacs") (:type git :flavor melpa :host github :repo "sebastiencs/frame-local" :package "frame-local" :local-repo "frame-local")) "dired-subtree" ("2020-10-15 20:48:42" ("dash" "dired-hacks-utils") (:type git :flavor melpa :files ("dired-subtree.el" "dired-subtree-pkg.el") :host github :repo "Fuco1/dired-hacks" :package "dired-subtree" :local-repo "dired-hacks")) "dired-hacks-utils" ("2020-10-15 20:48:42" ("dash") (:flavor melpa :files ("dired-hacks-utils.el" "dired-hacks-utils-pkg.el") :package "dired-hacks-utils" :local-repo "dired-hacks" :type git :repo "Fuco1/dired-hacks" :host github)) "dired-narrow" ("2020-10-15 20:48:42" ("dash" "dired-hacks-utils") (:flavor melpa :files ("dired-narrow.el" "dired-narrow-pkg.el") :package "dired-narrow" :local-repo "dired-hacks" :type git :repo "Fuco1/dired-hacks" :host github)) "vscode-icon" ("2020-10-15 20:48:42" ("emacs") (:type git :flavor melpa :files (:defaults "icons" "vscode-icon-pkg.el") :host github :repo "jojojames/vscode-icon-emacs" :package "vscode-icon" :local-repo "vscode-icon-emacs")) "dired-sidebar" ("2020-10-15 20:48:42" ("emacs" "dired-subtree") (:type git :flavor melpa :host github :repo "jojojames/dired-sidebar" :package "dired-sidebar" :local-repo "dired-sidebar")) "diredfl" ("2020-10-15 20:48:42" ("emacs") (:type git :flavor melpa :host github :repo "purcell/diredfl" :package "diredfl" :local-repo "diredfl")) "dired-git-info" ("2020-10-15 20:48:42" ("emacs") (:type git :host github :repo "emacs-straight/dired-git-info" :files ("*" (:exclude ".git")) :package "dired-git-info" :local-repo "dired-git-info")) "elisp-slime-nav" ("2020-10-15 20:48:42" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "purcell/elisp-slime-nav" :package "elisp-slime-nav" :local-repo "elisp-slime-nav")) "macrostep" ("2020-10-15 20:48:42" ("cl-lib") (:type git :flavor melpa :host github :repo "joddie/macrostep" :package "macrostep" :local-repo "macrostep")) "paredit" ("2020-10-15 20:48:42" nil (:type git :flavor melpa :files ("paredit.el" "paredit-pkg.el") :repo "https://mumble.net/~campbell/git/paredit.git" :package "paredit" :local-repo "paredit")) "highlight-parentheses" ("2020-10-15 20:48:42" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "tsdh/highlight-parentheses.el" :package "highlight-parentheses" :local-repo "highlight-parentheses.el")) "eshell-git-prompt" ("2020-10-15 20:48:42" ("emacs" "cl-lib" "dash") (:type git :flavor melpa :host github :repo "xuchunyang/eshell-git-prompt" :package "eshell-git-prompt" :local-repo "eshell-git-prompt")) "eyebrowse" ("2020-10-15 20:48:42" ("dash" "emacs") (:type git :flavor melpa :repo "https://depp.brause.cc/eyebrowse.git" :package "eyebrowse" :local-repo "eyebrowse")) "flycheck" ("2020-10-15 20:48:42" ("dash" "pkg-info" "let-alist" "seq" "emacs") (:type git :flavor melpa :host github :repo "flycheck/flycheck" :package "flycheck" :local-repo "flycheck")) "pkg-info" ("2020-10-15 20:48:42" ("epl") (:type git :flavor melpa :host github :repo "emacsorphanage/pkg-info" :package "pkg-info" :local-repo "pkg-info")) "epl" ("2020-10-15 20:48:42" ("cl-lib") (:type git :flavor melpa :host github :repo "cask/epl" :package "epl" :local-repo "epl")) "let-alist" ("2020-10-15 20:48:42" ("emacs") (:type git :host github :repo "emacsmirror/let-alist" :package "let-alist" :local-repo "let-alist")) "evil-leader" ("2020-10-15 20:48:42" ("evil") (:type git :flavor melpa :host github :repo "cofi/evil-leader" :package "evil-leader" :local-repo "evil-leader")) "helm" ("2020-10-15 20:48:42" ("emacs" "async" "popup" "helm-core") (:type git :flavor melpa :files ("*.el" "emacs-helm.sh" (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") "helm-pkg.el") :host github :repo "emacs-helm/helm" :package "helm" :local-repo "helm")) "async" ("2020-10-15 20:48:42" ("emacs") (:type git :flavor melpa :host github :repo "jwiegley/emacs-async" :package "async" :local-repo "emacs-async")) "popup" ("2020-10-15 20:48:42" ("cl-lib") (:type git :flavor melpa :files ("popup.el" "popup-pkg.el") :host github :repo "auto-complete/popup-el" :package "popup" :local-repo "popup-el")) "helm-core" ("2020-10-15 20:48:42" ("emacs" "async") (:flavor melpa :files ("helm-core-pkg.el" "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") :package "helm-core" :local-repo "helm" :type git :repo "emacs-helm/helm" :host github)) "helm-descbinds" ("2020-10-15 20:48:42" ("helm") (:type git :flavor melpa :host github :repo "emacs-helm/helm-descbinds" :package "helm-descbinds" :local-repo "helm-descbinds")) "helm-swoop" ("2020-10-15 20:48:42" ("emacs" "helm") (:type git :flavor melpa :host github :repo "emacsorphanage/helm-swoop" :package "helm-swoop" :local-repo "helm-swoop")) "helm-projectile" ("2020-10-15 20:48:42" ("helm" "projectile" "cl-lib") (:type git :flavor melpa :host github :repo "bbatsov/helm-projectile" :package "helm-projectile" :local-repo "helm-projectile")) "projectile" ("2020-10-15 20:48:42" ("emacs" "pkg-info") (:type git :flavor melpa :files ("projectile.el" "projectile-pkg.el") :host github :repo "bbatsov/projectile" :package "projectile" :local-repo "projectile")) "helm-rg" ("2020-10-15 20:48:42" ("emacs" "cl-lib" "dash" "helm") (:type git :flavor melpa :host github :repo "cosmicexplorer/helm-rg" :package "helm-rg" :local-repo "helm-rg")) "helm-xref" ("2020-10-15 20:48:42" ("emacs" "helm") (:type git :flavor melpa :host github :repo "brotzeit/helm-xref" :package "helm-xref" :local-repo "helm-xref")) "helm-ls-git" ("2020-10-15 20:48:42" ("helm") (:type git :flavor melpa :host github :repo "emacs-helm/helm-ls-git" :package "helm-ls-git" :local-repo "helm-ls-git")) "ivy" ("2020-10-15 20:48:42" ("emacs") (:type git :flavor melpa :files (:defaults (:exclude "swiper.el" "counsel.el" "ivy-hydra.el") "doc/ivy-help.org" "ivy-pkg.el") :host github :repo "abo-abo/swiper" :package "ivy" :local-repo "swiper")) "swiper" ("2020-10-15 20:48:42" ("emacs" "ivy") (:flavor melpa :files ("swiper.el" "swiper-pkg.el") :package "swiper" :local-repo "swiper" :type git :repo "abo-abo/swiper" :host github)) "counsel" ("2020-10-15 20:48:42" ("emacs" "swiper") (:flavor melpa :files ("counsel.el" "counsel-pkg.el") :package "counsel" :local-repo "swiper" :type git :repo "abo-abo/swiper" :host github)) "smex" ("2020-10-15 20:48:42" ("emacs") (:type git :flavor melpa :host github :repo "nonsequitur/smex" :package "smex" :local-repo "smex")) "hydra" ("2020-10-15 20:48:42" ("cl-lib" "lv") (:type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra" :package "hydra" :local-repo "hydra")) "lv" ("2020-10-15 20:48:42" nil (:flavor melpa :files ("lv.el" "lv-pkg.el") :package "lv" :local-repo "hydra" :type git :repo "abo-abo/hydra" :host github)) "lsp-mode" ("2020-10-15 20:48:42" ("emacs" "dash" "dash-functional" "f" "ht" "spinner" "markdown-mode" "lv") (:type git :flavor melpa :host github :repo "emacs-lsp/lsp-mode" :package "lsp-mode" :local-repo "lsp-mode")) "ht" ("2020-10-15 20:48:42" ("dash") (:type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el" :package "ht" :local-repo "ht.el")) "spinner" ("2020-10-15 20:48:42" nil (:type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git")) :package "spinner" :local-repo "spinner")) "markdown-mode" ("2020-10-15 20:48:42" ("emacs") (:type git :flavor melpa :host github :repo "jrblevin/markdown-mode" :package "markdown-mode" :local-repo "markdown-mode")) "ccls" ("2020-10-15 20:48:42" ("emacs" "lsp-mode" "dash") (:type git :flavor melpa :host github :repo "MaskRay/emacs-ccls" :package "ccls" :local-repo "emacs-ccls")) "lsp-ui" ("2020-10-15 20:48:43" ("emacs" "dash" "dash-functional" "lsp-mode" "markdown-mode") (:type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "lsp-ui-pkg.el") :host github :repo "emacs-lsp/lsp-ui" :package "lsp-ui" :local-repo "lsp-ui")) "company-lsp" ("2020-10-15 20:48:43" ("emacs" "lsp-mode" "company" "s" "dash") (:type git :flavor melpa :host github :repo "tigersoldier/company-lsp" :package "company-lsp" :local-repo "company-lsp")) "dap-mode" ("2020-10-15 20:48:43" ("emacs" "dash" "lsp-mode" "dash-functional" "bui" "f" "s" "lsp-treemacs" "posframe") (:type git :flavor melpa :files (:defaults "icons" "dap-mode-pkg.el") :host github :repo "emacs-lsp/dap-mode" :package "dap-mode" :local-repo "dap-mode")) "bui" ("2020-10-15 20:48:43" ("emacs" "dash") (:type git :flavor melpa :host github :repo "alezost/bui.el" :package "bui" :local-repo "bui.el")) "lsp-treemacs" ("2020-10-15 20:48:43" ("emacs" "dash" "dash-functional" "f" "ht" "treemacs" "lsp-mode") (:type git :flavor melpa :files (:defaults "icons" "lsp-treemacs-pkg.el") :host github :repo "emacs-lsp/lsp-treemacs" :package "lsp-treemacs" :local-repo "lsp-treemacs")) "treemacs" ("2020-10-15 20:48:43" ("emacs" "cl-lib" "dash" "s" "f" "ace-window" "pfuture" "hydra" "ht") (:type git :flavor melpa :files (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el" "src/scripts/treemacs*.py" (:exclude "src/extra/*") "treemacs-pkg.el") :host github :repo "Alexander-Miller/treemacs" :package "treemacs" :local-repo "treemacs")) "ace-window" ("2020-10-15 20:48:43" ("avy") (:type git :flavor melpa :host github :repo "abo-abo/ace-window" :package "ace-window" :local-repo "ace-window")) "pfuture" ("2020-10-15 20:48:43" ("emacs") (:type git :flavor melpa :host github :repo "Alexander-Miller/pfuture" :package "pfuture" :local-repo "pfuture")) "posframe" ("2020-10-15 20:48:43" ("emacs") (:type git :flavor melpa :host github :repo "tumashu/posframe" :package "posframe" :local-repo "posframe")) "magit" ("2020-10-15 20:48:43" ("emacs" "async" "dash" "git-commit" "transient" "with-editor") (:type git :flavor melpa :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "Documentation/magit.texi" "Documentation/AUTHORS.md" "LICENSE" (:exclude "lisp/magit-libgit.el") "magit-pkg.el") :host github :repo "magit/magit" :package "magit" :local-repo "magit")) "git-commit" ("2020-10-15 20:48:43" ("emacs" "dash" "transient" "with-editor") (:flavor melpa :files ("lisp/git-commit.el" "git-commit-pkg.el") :package "git-commit" :local-repo "magit" :type git :repo "magit/magit" :host github)) "transient" ("2020-10-15 20:48:43" ("emacs") (:type git :flavor melpa :files ("lisp/*.el" "docs/transient.texi" "transient-pkg.el") :host github :repo "magit/transient" :package "transient" :local-repo "transient")) "with-editor" ("2020-10-15 20:48:43" ("emacs" "async") (:type git :flavor melpa :host github :repo "magit/with-editor" :package "with-editor" :local-repo "with-editor")) "git-gutter" ("2020-10-15 20:48:43" ("emacs") (:type git :flavor melpa :host github :repo "emacsorphanage/git-gutter" :package "git-gutter" :local-repo "git-gutter")) "smart-mode-line" ("2020-10-15 20:48:43" ("emacs" "rich-minority") (:type git :flavor melpa :host github :repo "Malabarba/smart-mode-line" :package "smart-mode-line" :local-repo "smart-mode-line")) "rich-minority" ("2020-10-15 20:48:43" ("cl-lib") (:type git :flavor melpa :host github :repo "Malabarba/rich-minority" :package "rich-minority" :local-repo "rich-minority")) "multiple-cursors" ("2020-10-15 20:48:44" ("cl-lib") (:type git :flavor melpa :host github :repo "magnars/multiple-cursors.el" :package "multiple-cursors" :local-repo "multiple-cursors.el")) "treemacs-projectile" ("2020-10-15 20:48:44" ("emacs" "projectile" "treemacs") (:flavor melpa :files ("src/extra/treemacs-projectile.el" "treemacs-projectile-pkg.el") :package "treemacs-projectile" :local-repo "treemacs" :type git :repo "Alexander-Miller/treemacs" :host github)) "treemacs-magit" ("2020-10-15 20:48:44" ("emacs" "treemacs" "pfuture" "magit") (:flavor melpa :files ("src/extra/treemacs-magit.el" "treemacs-magit-pkg.el") :package "treemacs-magit" :local-repo "treemacs" :type git :repo "Alexander-Miller/treemacs" :host github)) "which-key" ("2020-10-15 20:48:44" ("emacs") (:type git :flavor melpa :host github :repo "justbur/emacs-which-key" :package "which-key" :local-repo "emacs-which-key")) "free-keys" ("2020-10-15 20:48:44" ("cl-lib") (:type git :flavor melpa :host github :repo "Fuco1/free-keys" :package "free-keys" :local-repo "free-keys")) "winum" ("2020-10-15 20:48:44" ("cl-lib" "dash") (:type git :flavor melpa :host github :repo "deb0ch/emacs-winum" :package "winum" :local-repo "emacs-winum")) "beacon" ("2020-10-15 20:48:44" ("seq") (:type git :flavor melpa :host github :repo "Malabarba/beacon" :package "beacon" :local-repo "beacon")) "dimmer" ("2020-10-15 20:48:44" ("emacs") (:type git :flavor melpa :host github :repo "gonewest818/dimmer.el" :package "dimmer" :local-repo "dimmer.el")) "buffer-move" ("2020-10-15 20:48:44" nil (:type git :flavor melpa :host github :repo "lukhas/buffer-move" :package "buffer-move" :local-repo "buffer-move")) "yasnippet" ("2020-10-15 20:48:44" ("cl-lib") (:type git :flavor melpa :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :host github :repo "joaotavora/yasnippet" :package "yasnippet" :local-repo "yasnippet")) "general" ("2020-10-15 20:48:44" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "noctuid/general.el" :package "general" :local-repo "general.el")) "dumb-jump" ("2020-10-15 20:48:44" ("emacs" "s" "dash" "popup") (:type git :flavor melpa :host github :repo "jacktasia/dumb-jump" :package "dumb-jump" :local-repo "dumb-jump")) "vterm" ("2020-10-15 20:48:44" ("emacs") (:type git :flavor melpa :files ("*" (:exclude ".dir-locals.el" ".gitignore" ".clang-format" ".travis.yml") "vterm-pkg.el") :host github :repo "akermu/emacs-libvterm" :package "vterm" :local-repo "emacs-libvterm")) "eglot" ("2020-09-30 21:51:47" ("emacs" "jsonrpc" "flymake" "project" "xref" "eldoc") (:type git :flavor melpa :host github :repo "joaotavora/eglot" :package "eglot" :local-repo "eglot")) "jsonrpc" ("2020-09-30 21:51:47" ("emacs") (:type git :host github :repo "emacsmirror/jsonrpc" :package "jsonrpc" :local-repo "jsonrpc")) "flymake" ("2020-09-30 21:51:47" ("emacs" "eldoc") (:type git :host github :repo "emacsmirror/flymake" :package "flymake" :local-repo "flymake")) "eldoc" ("2020-09-30 21:51:47" ("emacs") (:type git :host github :repo "emacsmirror/eldoc" :package "eldoc" :local-repo "eldoc")) "project" ("2020-09-30 21:51:47" ("emacs" "xref") (:type git :host github :repo "emacsmirror/project" :package "project" :local-repo "project")) "xref" ("2020-09-30 21:51:47" ("emacs") (:type git :host github :repo "emacsmirror/xref" :package "xref" :local-repo "xref"))))

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight straight-autoloads straight-x) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

(fn &optional SOURCES ACTION)" t nil) (autoload 'straight-visit-package-website "straight" "Interactively select a recipe, and visit the package's website." t nil) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a non-nil `:no-build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil if package was actually installed, and nil
otherwise (this can only happen if NO-CLONE is non-nil).

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t nil) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a non-nil `:no-build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t nil) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t nil) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t nil) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t nil) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded." nil nil) (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted." nil nil) (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t nil) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t nil) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t nil) (register-definition-prefixes "straight" '("straight-")) (defvar straight-x-pinned-packages nil "List of pinned packages.") (register-definition-prefixes "straight-x" '("straight-x-")) (provide 'straight-autoloads)) "undo-tree" ((undo-tree undo-tree-autoloads) (autoload 'undo-tree-mode "undo-tree" "Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

If called interactively, enable Undo-Tree mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}

(fn &optional ARG)" t nil) (put 'global-undo-tree-mode 'globalized-minor-mode t) (defvar global-undo-tree-mode nil "Non-nil if Global Undo-Tree mode is enabled.
See the `global-undo-tree-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.") (custom-autoload 'global-undo-tree-mode "undo-tree" nil) (autoload 'global-undo-tree-mode "undo-tree" "Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global Undo-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "undo-tree" '("*undo-tree-id-counter*" "buffer-undo-tree" "turn-on-undo-tree-mode" "undo-"))) (provide 'undo-tree-autoloads)) "goto-chg" ((goto-chg-autoloads goto-chg) (autoload 'goto-last-change "goto-chg" "Go to the point where the last edit was made in the current buffer.
Repeat the command to go to the second last edit, etc.

To go back to more recent edit, the reverse of this command, use \\[goto-last-change-reverse]
or precede this command with \\[universal-argument] - (minus).

It does not go to the same point twice even if there has been many edits
there. I call the minimal distance between distinguishable edits \"span\".
Set variable `glc-default-span' to control how close is \"the same point\".
Default span is 8.
The span can be changed temporarily with \\[universal-argument] right before \\[goto-last-change]:
\\[universal-argument] <NUMBER> set current span to that number,
\\[universal-argument] (no number) multiplies span by 4, starting with default.
The so set span remains until it is changed again with \\[universal-argument], or the consecutive
repetition of this command is ended by any other command.

When span is zero (i.e. \\[universal-argument] 0) subsequent \\[goto-last-change] visits each and
every point of edit and a message shows what change was made there.
In this case it may go to the same point twice.

This command uses undo information. If undo is disabled, so is this command.
At times, when undo information becomes too large, the oldest information is
discarded. See variable `undo-limit'.

(fn ARG)" t nil) (autoload 'goto-last-change-reverse "goto-chg" "Go back to more recent changes after \\[goto-last-change] have been used.
See `goto-last-change' for use of prefix argument.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "goto-chg" '("glc-"))) (provide 'goto-chg-autoloads)) "evil" ((evil-maps evil-macros evil-core evil-development evil-command-window evil-common evil-jumps evil evil-integration evil-autoloads evil-commands evil-types evil-ex evil-digraphs evil-vars evil-repeat evil-states evil-keybindings evil-pkg evil-search) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-command-window" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-commands" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-common" '("bounds-of-evil-" "evil-" "forward-evil-"))) (autoload 'evil-mode "evil" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-core" '("evil-" "turn-o"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-digraphs" '("evil-digraph"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-ex" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-integration" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-jumps" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-macros" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-maps" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-repeat" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-search" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-states" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-types" '("evil-ex-get-optional-register-and-count"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-vars" '("evil-"))) (provide 'evil-autoloads)) "evil-surround" ((evil-surround-autoloads evil-surround) (autoload 'evil-surround-delete "evil-surround" "Delete the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with
the overlays OUTER and INNER, where OUTER includes the delimiters
and INNER excludes them. The intersection (i.e., difference)
between these overlays is what is deleted.

(fn CHAR &optional OUTER INNER)" t nil) (autoload 'evil-surround-change "evil-surround" "Change the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with the
overlays OUTER and INNER, which are passed to `evil-surround-delete'.

(fn CHAR &optional OUTER INNER)" t nil) (autoload 'evil-surround-mode "evil-surround" "Buffer-local minor mode to emulate surround.vim.

If called interactively, enable Evil-Surround mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'turn-on-evil-surround-mode "evil-surround" "Enable evil-surround-mode in the current buffer." nil nil) (autoload 'turn-off-evil-surround-mode "evil-surround" "Disable evil-surround-mode in the current buffer." nil nil) (put 'global-evil-surround-mode 'globalized-minor-mode t) (defvar global-evil-surround-mode nil "Non-nil if Global Evil-Surround mode is enabled.
See the `global-evil-surround-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-surround-mode'.") (custom-autoload 'global-evil-surround-mode "evil-surround" nil) (autoload 'global-evil-surround-mode "evil-surround" "Toggle Evil-Surround mode in all buffers.
With prefix ARG, enable Global Evil-Surround mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Surround mode is enabled in all buffers where
`turn-on-evil-surround-mode' would do it.
See `evil-surround-mode' for more information on Evil-Surround mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-surround" '("evil-surround-"))) (provide 'evil-surround-autoloads)) "evil-indent-textobject" ((evil-indent-textobject evil-indent-textobject-autoloads) (eval-after-load 'evil '(progn (autoload 'evil-indent-i-indent "evil-indent-textobject" nil t) (autoload 'evil-indent-a-indent "evil-indent-textobject" nil t) (autoload 'evil-indent-a-indent-lines "evil-indent-textobject" nil t) (define-key evil-inner-text-objects-map "i" 'evil-indent-i-indent) (define-key evil-outer-text-objects-map "i" 'evil-indent-a-indent) (define-key evil-outer-text-objects-map "I" 'evil-indent-a-indent-lines))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-indent-textobject" '("evil-indent--"))) (provide 'evil-indent-textobject-autoloads)) "evil-org" ((evil-org-autoloads evil-org-agenda evil-org) (autoload 'evil-org-mode "evil-org" "Buffer local minor mode for evil-org

If called interactively, enable Evil-Org mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-org" '("evil-org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-org-agenda" '("evil-org-agenda-set-keys"))) (provide 'evil-org-autoloads)) "doom-themes" ((doom-tomorrow-day-theme doom-henna-theme doom-one-light-theme doom-fairy-floss-theme doom-themes-ext-org doom-rouge-theme doom-laserwave-theme doom-opera-theme doom-horizon-theme doom-themes-base doom-themes-ext-neotree doom-outrun-electric-theme doom-ephemeral-theme doom-Iosvkem-theme doom-opera-light-theme doom-gruvbox-light-theme doom-city-lights-theme doom-oceanic-next-theme doom-challenger-deep-theme doom-spacegrey-theme doom-vibrant-theme doom-acario-light-theme doom-dark+-theme doom-palenight-theme doom-themes-ext-treemacs doom-peacock-theme doom-nord-theme doom-monokai-spectrum-theme doom-monokai-classic-theme doom-sourcerer-theme doom-nova-theme doom-moonlight-theme doom-material-theme doom-gruvbox-theme doom-themes-autoloads doom-themes doom-manegarm-theme doom-dracula-theme doom-nord-light-theme doom-snazzy-theme doom-wilmersdorf-theme doom-tomorrow-night-theme doom-solarized-dark-theme doom-one-theme doom-acario-dark-theme doom-solarized-light-theme doom-molokai-theme doom-themes-ext-visual-bell doom-monokai-pro-theme) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-Iosvkem-theme" '("doom-Iosvkem"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-acario-dark-theme" '("doom-acario-dark"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-acario-light-theme" '("doom-acario-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-challenger-deep-theme" '("doom-challenger-deep"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-city-lights-theme" '("doom-city-lights"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-dark+-theme" '("doom-dark+"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-dracula-theme" '("doom-dracula"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-ephemeral-theme" '("doom-ephemeral"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-fairy-floss-theme" '("doom-fairy-floss"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-gruvbox-light-theme" '("doom-gruvbox-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-gruvbox-theme" '("doom-gruvbox"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-henna-theme" '("doom-henna"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-horizon-theme" '("doom-horizon"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-laserwave-theme" '("doom-laserwave"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-manegarm-theme" '("doom-manegarm"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-material-theme" '("doom-material"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-molokai-theme" '("doom-molokai"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-classic-theme" '("doom-monokai-classic"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-pro-theme" '("doom-monokai-pro"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-spectrum-theme" '("doom-monokai-spectrum"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-moonlight-theme" '("doom-moonlight"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nord-light-theme" '("doom-nord-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nord-theme" '("doom-nord"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nova-theme" '("doom-nova"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-oceanic-next-theme" '("doom-oceanic-next"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-one-light-theme" '("doom-one-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-one-theme" '("doom-one"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-opera-light-theme" '("doom-opera-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-opera-theme" '("doom-opera"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-outrun-electric-theme" '("doom-outrun-electric"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-palenight-theme" '("doom-palenight"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-peacock-theme" '("doom-peacock"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-rouge-theme" '("doom-rouge"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-snazzy-theme" '("doom-snazzy"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-solarized-dark-theme" '("doom-solarized-dark"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-solarized-light-theme" '("doom-solarized-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-sourcerer-theme" '("doom-sourcerer"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-spacegrey-theme" '("doom-spacegrey"))) (autoload 'doom-name-to-rgb "doom-themes" "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame).

(fn COLOR)" nil nil) (autoload 'doom-blend "doom-themes" "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)

(fn COLOR1 COLOR2 ALPHA)" nil nil) (autoload 'doom-darken "doom-themes" "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1).

(fn COLOR ALPHA)" nil nil) (autoload 'doom-lighten "doom-themes" "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1).

(fn COLOR ALPHA)" nil nil) (autoload 'doom-color "doom-themes" "Retrieve a specific color named NAME (a symbol) from the current theme.

(fn NAME &optional TYPE)" nil nil) (autoload 'doom-ref "doom-themes" "TODO

(fn FACE PROP &optional CLASS)" nil nil) (autoload 'doom-themes-set-faces "doom-themes" "Customize THEME (a symbol) with FACES.

If THEME is nil, it applies to all themes you load. FACES is a list of Doom
theme face specs. These is a simplified spec. For example:

  (doom-themes-set-faces 'user
    '(default :background red :foreground blue)
    '(doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
    '(doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
    '(doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
    '(doom-modeline-buffer-project-root :foreground green :weight 'bold))

(fn THEME &rest FACES)" nil nil) (function-put 'doom-themes-set-faces 'lisp-indent-function 'defun) (when (and (boundp 'custom-theme-load-path) load-file-name) (let* ((base (file-name-directory load-file-name)) (dir (expand-file-name "themes/" base))) (add-to-list 'custom-theme-load-path (or (and (file-directory-p dir) dir) base)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes" '("def-doom-theme" "doom-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-base" '("doom-themes-base-"))) (autoload 'doom-themes-neotree-config "doom-themes-ext-neotree" "Install doom-themes' neotree configuration.

Includes an Atom-esque icon theme and highlighting based on filetype." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-ext-neotree" '("doom-"))) (autoload 'doom-themes-org-config "doom-themes-ext-org" "Enable custom fontification & improves theme integration with org-mode." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-ext-org" '("doom-"))) (autoload 'doom-themes-treemacs-config "doom-themes-ext-treemacs" "Install doom-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-ext-treemacs" '("doom-themes-"))) (autoload 'doom-themes-visual-bell-fn "doom-themes-ext-visual-bell" "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it." nil nil) (autoload 'doom-themes-visual-bell-config "doom-themes-ext-visual-bell" "Enable flashing the mode-line on error." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-tomorrow-day-theme" '("doom-tomorrow-day"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-tomorrow-night-theme" '("doom-tomorrow-night"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-vibrant-theme" '("doom-vibrant"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-wilmersdorf-theme" '("doom-wilmersdorf"))) (provide 'doom-themes-autoloads)) "dash" ((dash-autoloads dash) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-keep" "-l" "-m" "-non" "-only-some" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-"))) (provide 'dash-autoloads)) "s" ((s s-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "s" '("s-"))) (provide 's-autoloads)) "f" ((f f-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "f" '("f-"))) (provide 'f-autoloads)) "org" ((org-protocol ox-publish org-autoloads ob-java org-datetree org-attach ob-plantuml ob-css ox-icalendar org-crypt ox-man ox-html ol-bbdb org-capture ob-stan org-table org-plot ob-js ob-ref org-list org-element org-agenda org-src ol-w3m org-tempo ol-bibtex ob-exp ol-eshell org-entities ob-vala ob-sql ob-table ob-shen org-id ob-sed org-timer ob-latex ox-odt org-mouse ol-docview org-compat ol-mhe ox-md ob-shell ob-mscgen ox-beamer ob-lua org-lint org-macs ox-ascii ob-abc ol-rmail org-num ob-emacs-lisp org-mobile ox ol-info org-faces org-goto ob-fortran ob-screen org-inlinetask org-feed ob-calc ob-makefile ob-eshell ob-haskell org-pcomplete ob ox-latex ob-org ob-lisp org-install org-indent ol-irc org-duration ob-lob ob-ruby ob-ditaa org-archive org-clock ob-J ob-lilypond org-footnote ob-scheme ob-clojure ol ob-io ob-perl ob-dot ob-eval ob-octave ob-maxima ob-comint ob-matlab ob-sqlite ob-forth org-ctags ob-sass org-macro ox-texinfo ob-processing org ol-eww ox-org ob-core ob-tangle ob-asymptote ob-R ob-hledger org-colview ob-ebnf ob-coq ob-groovy org-keys org-loaddefs ob-awk ob-python org-attach-git org-habit ob-picolisp ob-gnuplot ob-C ob-ocaml ol-gnus org-refile ob-ledger) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-C" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-J" '("obj-" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-R" '("ob-R-" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-abc" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-asymptote" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-awk" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-calc" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-clojure" '("ob-clojure-" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-comint" '("org-babel-comint-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-coq" '("coq-program-name" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-core" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-css" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ditaa" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-dot" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ebnf" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-emacs-lisp" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-eshell" '("ob-eshell-session-live-p" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-eval" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-exp" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-forth" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-fortran" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-gnuplot" '("*org-babel-gnuplot-" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-groovy" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-haskell" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-hledger" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-io" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-java" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-js" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-latex" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ledger" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lilypond" '("lilypond-mode" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lisp" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lob" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lua" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-makefile" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-maxima" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-mscgen" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ocaml" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-octave" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-org" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-perl" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-picolisp" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-plantuml" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-processing" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-python" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ref" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ruby" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sass" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-scheme" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-screen" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sed" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-shell" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-shen" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sql" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sqlite" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-stan" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-table" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-tangle" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-vala" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-bbdb" '("org-bbdb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-bibtex" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-docview" '("org-docview-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-eshell" '("org-eshell-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-eww" '("org-eww-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-gnus" '("org-gnus-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-info" '("org-info-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-irc" '("org-irc-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-mhe" '("org-mhe-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-rmail" '("org-rmail-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-w3m" '("org-w3m-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org" '("org-" "turn-on-org-cdlatex"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-agenda" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-archive" '("org-a"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-attach" '("org-attach-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-attach-git" '("org-attach-git-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-capture" '("org-capture-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-clock" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-colview" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-compat" '("org-"))) (autoload 'org-encrypt-entry "org-crypt" "Encrypt the content of the current headline." t nil) (autoload 'org-decrypt-entry "org-crypt" "Decrypt the content of the current headline." t nil) (autoload 'org-encrypt-entries "org-crypt" "Encrypt all top-level entries in the current buffer." t nil) (autoload 'org-decrypt-entries "org-crypt" "Decrypt all entries in the current buffer." t nil) (autoload 'org-crypt-use-before-save-magic "org-crypt" "Add a hook to automatically encrypt entries before a file is saved to disk." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-crypt" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ctags" '("org-ctags-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-datetree" '("org-datetree-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-duration" '("org-duration-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-element" '("org-element-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-entities" '("org-entit"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-faces" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-feed" '("org-feed-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-footnote" '("org-footnote-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-goto" '("org-goto-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-habit" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-id" '("org-id-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-indent" '("org-indent-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-inlinetask" '("org-inlinetask-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-keys" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-lint" '("org-lint-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-list" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-macro" '("org-macro-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-macs" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mobile" '("org-mobile-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mouse" '("org-mouse-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-num" '("org-num-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-pcomplete" '("org-" "pcomplete/org-mode/"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-plot" '("org-plot"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-protocol" '("org-protocol-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-refile" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-src" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-table" '("org"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-tempo" '("org-tempo-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-timer" '("org-timer-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox" '("org-export-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-ascii" '("org-ascii-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-beamer" '("org-beamer-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-html" '("org-html-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-icalendar" '("org-icalendar-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-latex" '("org-latex-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-man" '("org-man-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-md" '("org-md-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-odt" '("org-odt-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-org" '("org-org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-publish" '("org-publish-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-texinfo" '("org-texinfo-"))) (provide 'org-autoloads)) "emacsql" ((emacsql-autoloads emacsql emacsql-compiler) (autoload 'emacsql-show-last-sql "emacsql" "Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer.

(fn &optional PREFIX)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql" '("emacsql-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-compiler" '("emacsql-"))) (provide 'emacsql-autoloads)) "emacsql-sqlite" ((emacsql-sqlite emacsql-sqlite-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-sqlite" '("emacsql-sqlite-"))) (provide 'emacsql-sqlite-autoloads)) "org-roam" ((org-roam org-roam-graph org-roam-capture org-roam-doctor org-roam-dev org-roam-db org-roam-autoloads org-roam-protocol org-roam-buffer org-roam-completion org-roam-macs org-roam-dailies org-roam-compat) (defalias 'org-roam 'org-roam-buffer-toggle-display) (defvar org-roam-mode nil "Non-nil if Org-Roam mode is enabled.
See the `org-roam-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-mode'.") (custom-autoload 'org-roam-mode "org-roam" nil) (autoload 'org-roam-mode "org-roam" "Minor mode for Org-roam.

This mode sets up several hooks, to ensure that the cache is updated on file
changes, renames and deletes. It is also in charge of graceful termination of
the database connection.

When called interactively, toggle `org-roam-mode'. with prefix
ARG, enable `org-roam-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive. If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively.

(fn &optional ARG)" t nil) (autoload 'org-roam-version "org-roam" "Return `org-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area.

(fn &optional MESSAGE)" t nil) (autoload 'org-roam-diagnostics "org-roam" "Collect and print info for `org-roam' issues." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam" '("org-roam-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-buffer" '("org-roam-buffer"))) (autoload 'org-roam-capture "org-roam-capture" "Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-capture" '("org-roam-capture-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-completion" '("org-roam-completion-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-dailies" '("org-roam-dailies-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-db" '("org-roam-db"))) (autoload 'org-roam-doctor "org-roam-doctor" "Perform a check on the current buffer to ensure cleanliness.
If CHECKALL, run the check for all Org-roam files.

(fn &optional CHECKALL)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-doctor" '("org-roam-doctor-"))) (autoload 'org-roam-graph "org-roam-graph" "Build and possibly display a graph for FILE from NODE-QUERY.
If FILE is nil, default to current buffer's file name.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for FILE.
  - `\\[universal-argument]' N   show the graph for FILE limiting nodes to N steps.
  - `\\[universal-argument] \\[universal-argument]' build the graph.
  - `\\[universal-argument]' -   build the graph for FILE.
  - `\\[universal-argument]' -N  build the graph for FILE limiting nodes to N steps.

(fn &optional ARG FILE NODE-QUERY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-graph" '("org-roam-graph-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-macs" '("org-roam-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-protocol" '("org-roam-protocol-open-"))) (provide 'org-roam-autoloads)) "bind-key" ((bind-key-autoloads bind-key) (autoload 'bind-key "bind-key" "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap variable or symbol.
For example:

  (bind-key \"M-h\" #'some-interactive-function my-mode-map)

  (bind-key \"M-h\" #'some-interactive-function 'my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time.

(fn KEY-NAME COMMAND &optional KEYMAP PREDICATE)" nil t) (autoload 'unbind-key "bind-key" "Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details.

(fn KEY-NAME &optional KEYMAP)" nil t) (autoload 'bind-key* "bind-key" "Similar to `bind-key', but overrides any mode-specific bindings.

(fn KEY-NAME COMMAND &optional PREDICATE)" nil t) (autoload 'bind-keys "bind-key" "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

(fn &rest ARGS)" nil t) (autoload 'bind-keys* "bind-key" "

(fn &rest ARGS)" nil t) (autoload 'describe-personal-keybindings "bind-key" "Display all the personal keybindings defined by `bind-key'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bind-key" '("bind-key" "compare-keybindings" "get-binding-description" "override-global-m" "personal-keybindings"))) (provide 'bind-key-autoloads)) "use-package" ((use-package-core use-package-bind-key use-package-lint use-package use-package-diminish use-package-delight use-package-jump use-package-ensure use-package-autoloads) (autoload 'use-package-autoload-keymap "use-package-bind-key" "Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL. It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword. It
works by binding the given key sequence to an invocation of this
function for a particular keymap. The keymap is expected to be
defined by the package. In this way, loading the package is
deferred until the prefix key sequence is pressed.

(fn KEYMAP-SYMBOL PACKAGE OVERRIDE)" nil nil) (autoload 'use-package-normalize-binder "use-package-bind-key" "

(fn NAME KEYWORD ARGS)" nil nil) (defalias 'use-package-normalize/:bind 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind* 'use-package-normalize-binder) (defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode) (defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode) (autoload 'use-package-handler/:bind "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional BIND-MACRO)" nil nil) (defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder) (autoload 'use-package-handler/:bind-keymap "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional OVERRIDE)" nil nil) (autoload 'use-package-handler/:bind-keymap* "use-package-bind-key" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-bind-key" '("use-package-handler/:bind*"))) (autoload 'use-package "use-package-core" "Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.
:hook            Specify hook(s) to attach this package to.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic', `:hook',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:after           Delay the use-package declaration until after the named modules
                 have loaded. Once load, it will be as though the use-package
                 declaration (without `:after') had been seen at that moment.
:demand          Prevent the automatic deferred loading introduced by constructs
                 such as `:bind' (see `:defer' for the complete list).

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.
:diminish        Support for diminish.el (if installed).
:delight         Support for delight.el (if installed).
:custom          Call `custom-set' or `set-default' with each variable
                 definition without modifying the Emacs `custom-file'.
                 (compare with `custom-set-variables').
:custom-face     Call `customize-set-faces' with each face definition.
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive.

(fn NAME &rest ARGS)" nil t) (function-put 'use-package 'lisp-indent-function '1) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-core" '("use-package-"))) (autoload 'use-package-normalize/:delight "use-package-delight" "Normalize arguments to delight.

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:delight "use-package-delight" "

(fn NAME KEYWORD ARGS REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-delight" '("use-package-normalize-delight"))) (autoload 'use-package-normalize/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-diminish" '("use-package-normalize-diminish"))) (autoload 'use-package-normalize/:ensure "use-package-ensure" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:ensure "use-package-ensure" "

(fn NAME KEYWORD ENSURE REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-ensure" '("use-package-"))) (autoload 'use-package-jump-to-package-form "use-package-jump" "Attempt to find and jump to the `use-package' form that loaded
PACKAGE. This will only find the form if that form actually
required PACKAGE. If PACKAGE was previously required then this
function will jump to the file that originally required PACKAGE
instead.

(fn PACKAGE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-jump" '("use-package-find-require"))) (autoload 'use-package-lint "use-package-lint" "Check for errors in use-package declarations.
For example, if the module's `:if' condition is met, but even
with the specified `:load-path' the module cannot be found." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-lint" '("use-package-lint-declaration"))) (provide 'use-package-autoloads)) "diminish" ((diminish-autoloads diminish) (autoload 'diminish "diminish" "Diminish mode-line display of minor mode MODE to TO-WHAT (default \"\").

Interactively, enter (with completion) the name of any minor mode, followed
on the next line by what you want it diminished to (default empty string).
The response to neither prompt should be quoted.  However, in Lisp code,
both args must be quoted, the first as a symbol, the second as a string,
as in (diminish 'jiggle-mode \" Jgl\").

The mode-line displays of minor modes usually begin with a space, so
the modes' names appear as separate words on the mode line.  However, if
you're having problems with a cramped mode line, you may choose to use single
letters for some modes, without leading spaces.  Capitalizing them works
best; if you then diminish some mode to \"X\" but have abbrev-mode enabled as
well, you'll get a display like \"AbbrevX\".  This function prepends a space
to TO-WHAT if it's > 1 char long & doesn't already begin with a space.

(fn MODE &optional TO-WHAT)" t nil) (autoload 'diminish-undo "diminish" "Restore mode-line display of diminished mode MODE to its minor-mode value.
Do nothing if the arg is a minor mode that hasn't been diminished.

Interactively, enter (with completion) the name of any diminished mode (a
mode that was formerly a minor mode on which you invoked \\[diminish]).
To restore all diminished modes to minor status, answer `diminished-modes'.
The response to the prompt shouldn't be quoted.  However, in Lisp code,
the arg must be quoted as a symbol, as in (diminish-undo 'diminished-modes).

(fn MODE)" t nil) (autoload 'diminished-modes "diminish" "Echo all active diminished or minor modes as if they were minor.
The display goes in the echo area; if it's too long even for that,
you can see the whole thing in the *Messages* buffer.
This doesn't change the status of any modes; it just lets you see
what diminished modes would be on the mode-line if they were still minor." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "diminish" '("diminish"))) (provide 'diminish-autoloads)) "avy" ((avy avy-autoloads) (autoload 'avy-process "avy" "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)" nil nil) (autoload 'avy-goto-char "avy" "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-char-in-line "avy" "Jump to the currently visible CHAR in the current line.

(fn CHAR)" t nil) (autoload 'avy-goto-char-2 "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn CHAR1 CHAR2 &optional ARG BEG END)" t nil) (autoload 'avy-goto-char-2-above "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t nil) (autoload 'avy-goto-char-2-below "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t nil) (autoload 'avy-isearch "avy" "Jump to one of the current isearch candidates." t nil) (autoload 'avy-goto-word-0 "avy" "Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t nil) (autoload 'avy-goto-whitespace-end "avy" "Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t nil) (autoload 'avy-goto-word-1 "avy" "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

(fn CHAR &optional ARG BEG END SYMBOL)" t nil) (autoload 'avy-goto-word-1-above "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-word-1-below "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1 "avy" "Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1-above "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1-below "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-subword-0 "avy" "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

(fn &optional ARG PREDICATE BEG END)" t nil) (autoload 'avy-goto-subword-1 "avy" "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-word-or-subword-1 "avy" "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t nil) (autoload 'avy-goto-line "avy" "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

(fn &optional ARG)" t nil) (autoload 'avy-goto-line-above "avy" "Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t nil) (autoload 'avy-goto-line-below "avy" "Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t nil) (autoload 'avy-goto-end-of-line "avy" "Call `avy-goto-line' and move to the end of the line.

(fn &optional ARG)" t nil) (autoload 'avy-copy-line "avy" "Copy a selected line above the current line.
ARG lines can be used.

(fn ARG)" t nil) (autoload 'avy-move-line "avy" "Move a selected line above the current line.
ARG lines can be used.

(fn ARG)" t nil) (autoload 'avy-copy-region "avy" "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t nil) (autoload 'avy-move-region "avy" "Select two lines and move the text between them above the current line." t nil) (autoload 'avy-kill-region "avy" "Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t nil) (autoload 'avy-kill-ring-save-region "avy" "Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn ARG)" t nil) (autoload 'avy-kill-whole-line "avy" "Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

(fn ARG)" t nil) (autoload 'avy-kill-ring-save-whole-line "avy" "Select line and save the whole selected line as if killed, but don\342\200\231t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

(fn ARG)" t nil) (autoload 'avy-setup-default "avy" "Setup the default shortcuts." nil nil) (autoload 'avy-goto-char-timer "avy" "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "avy" '("avy-"))) (provide 'avy-autoloads)) "browse-kill-ring" ((browse-kill-ring browse-kill-ring-autoloads) (autoload 'browse-kill-ring-default-keybindings "browse-kill-ring" "Set up M-y (`yank-pop') so that it can invoke `browse-kill-ring'.
Normally, if M-y was not preceeded by C-y, then it has no useful
behavior.  This function sets things up so that M-y will invoke
`browse-kill-ring'." t nil) (autoload 'browse-kill-ring "browse-kill-ring" "Display items in the `kill-ring' in another buffer." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "browse-kill-ring" '("browse-kill-ring-"))) (provide 'browse-kill-ring-autoloads)) "company" ((company company-dabbrev-code company-abbrev company-tng company-template company-dabbrev company-keywords company-nxml company-oddmuse company-capf company-yasnippet company-elisp company-bbdb company-cmake company-autoloads company-clang company-semantic company-gtags company-etags company-files company-ispell company-css company-tempo) (autoload 'company-mode "company" "\"complete anything\"; is an in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

If called interactively, enable Company mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'.  If you want to start a specific backend, call
it interactively or use `company-begin-backend'.

By default, the completions list is sorted alphabetically, unless the
backend chooses otherwise, or `company-transformers' changes it later.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

(fn &optional ARG)" t nil) (put 'global-company-mode 'globalized-minor-mode t) (defvar global-company-mode nil "Non-nil if Global Company mode is enabled.
See the `global-company-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-mode'.") (custom-autoload 'global-company-mode "company" nil) (autoload 'global-company-mode "company" "Toggle Company mode in all buffers.
With prefix ARG, enable Global Company mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Company mode is enabled in all buffers where
`company-mode-on' would do it.
See `company-mode' for more information on Company mode.

(fn &optional ARG)" t nil) (autoload 'company-manual-begin "company" nil t nil) (autoload 'company-complete "company" "Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company" '("company-"))) (autoload 'company-abbrev "company-abbrev" "`company-mode' completion backend for abbrev.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-abbrev" '("company-abbrev-insert"))) (autoload 'company-bbdb "company-bbdb" "`company-mode' completion backend for BBDB.

(fn COMMAND &optional ARG &rest IGNORE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-bbdb" '("company-bbdb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-capf" '("company-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-clang" '("company-clang"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-cmake" '("company-cmake"))) (autoload 'company-css "company-css" "`company-mode' completion backend for `css-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-css" '("company-css-"))) (autoload 'company-dabbrev "company-dabbrev" "dabbrev-like `company-mode' completion backend.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-dabbrev" '("company-dabbrev-"))) (autoload 'company-dabbrev-code "company-dabbrev-code" "dabbrev-like `company-mode' backend for code.
The backend looks for all symbols in the current buffer that aren't in
comments or strings.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-dabbrev-code" '("company-dabbrev-code-"))) (autoload 'company-elisp "company-elisp" "`company-mode' completion backend for Emacs Lisp.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-elisp" '("company-elisp-"))) (autoload 'company-etags "company-etags" "`company-mode' completion backend for etags.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-etags" '("company-etags-"))) (autoload 'company-files "company-files" "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-files" '("company-file"))) (autoload 'company-gtags "company-gtags" "`company-mode' completion backend for GNU Global.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-gtags" '("company-gtags-"))) (autoload 'company-ispell "company-ispell" "`company-mode' completion backend using Ispell.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-ispell" '("company-ispell-"))) (autoload 'company-keywords "company-keywords" "`company-mode' backend for programming language keywords.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-keywords" '("company-keywords-"))) (autoload 'company-nxml "company-nxml" "`company-mode' completion backend for `nxml-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-nxml" '("company-nxml-"))) (autoload 'company-oddmuse "company-oddmuse" "`company-mode' completion backend for `oddmuse-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-oddmuse" '("company-oddmuse-"))) (autoload 'company-semantic "company-semantic" "`company-mode' completion backend using CEDET Semantic.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-semantic" '("company-semantic-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-template" '("company-template-"))) (autoload 'company-tempo "company-tempo" "`company-mode' completion backend for tempo.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-tempo" '("company-tempo-"))) (autoload 'company-tng-frontend "company-tng" "When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion.

(fn COMMAND)" nil nil) (define-obsolete-function-alias 'company-tng-configure-default 'company-tng-mode "0.9.14" "Applies the default configuration to enable company-tng.") (defvar company-tng-mode nil "Non-nil if Company-Tng mode is enabled.
See the `company-tng-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-tng-mode'.") (custom-autoload 'company-tng-mode "company-tng" nil) (autoload 'company-tng-mode "company-tng" "This minor mode enables `company-tng-frontend'.

If called interactively, enable Company-Tng mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-tng" '("company-tng-"))) (autoload 'company-yasnippet "company-yasnippet" "`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook 'js-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push '(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") 'company-yasnippet)

(fn COMMAND &optional ARG &rest IGNORE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-yasnippet" '("company-yasnippet-"))) (provide 'company-autoloads)) "dash-functional" ((dash-functional-autoloads dash-functional) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dash-functional" '("-a" "-c" "-f" "-iteratefn" "-juxt" "-not" "-o" "-p" "-rpartial"))) (provide 'dash-functional-autoloads)) "frame-local" ((frame-local frame-local-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "frame-local" '("frame-local-"))) (provide 'frame-local-autoloads)) "company-box" ((company-box company-box-doc company-box-autoloads company-box-icons) (autoload 'company-box-mode "company-box" "Company-box minor mode.

If called interactively, enable Company-Box mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-box" '("company-box-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-box-doc" '("company-box-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-box-icons" '("company-box-icons-"))) (provide 'company-box-autoloads)) "dired-hacks-utils" ((dired-hacks-utils dired-hacks-utils-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-hacks-utils" '("dired-"))) (provide 'dired-hacks-utils-autoloads)) "dired-subtree" ((dired-subtree dired-subtree-autoloads) (autoload 'dired-subtree-narrow "dired-subtree" "Narrow the buffer to this subtree." t nil) (autoload 'dired-subtree-up "dired-subtree" "Jump up one directory.

(fn &optional ARG)" t nil) (autoload 'dired-subtree-down "dired-subtree" "Jump down one directory.

(fn &optional ARG)" t nil) (autoload 'dired-subtree-next-sibling "dired-subtree" "Go to the next sibling.

(fn &optional ARG)" t nil) (autoload 'dired-subtree-previous-sibling "dired-subtree" "Go to the previous sibling.

(fn &optional ARG)" t nil) (autoload 'dired-subtree-beginning "dired-subtree" "Go to the first file in this subtree." t nil) (autoload 'dired-subtree-end "dired-subtree" "Go to the first file in this subtree." t nil) (autoload 'dired-subtree-mark-subtree "dired-subtree" "Mark all files in this subtree.

With prefix argument mark all the files in subdirectories
recursively.

(fn &optional ALL)" t nil) (autoload 'dired-subtree-unmark-subtree "dired-subtree" "Unmark all files in this subtree.

With prefix argument unmark all the files in subdirectories
recursively.

(fn &optional ALL)" t nil) (autoload 'dired-subtree-revert "dired-subtree" "Revert the subtree.

This means reinserting the content of this subtree and all its
children." t nil) (autoload 'dired-subtree-insert "dired-subtree" "Insert subtree under this directory." t nil) (autoload 'dired-subtree-remove "dired-subtree" "Remove subtree at point." t nil) (autoload 'dired-subtree-toggle "dired-subtree" "Insert subtree at point or remove it if it was not present." t nil) (autoload 'dired-subtree-cycle "dired-subtree" "Org-mode like cycle visibility:

1) Show subtree
2) Show subtree recursively (if previous command was cycle)
3) Remove subtree

Numeric prefix will set max depth

(fn &optional MAX-DEPTH)" t nil) (autoload 'dired-subtree-only-this-file "dired-subtree" "Remove all the siblings on the route from this file to the top-most directory.

With ARG non-nil, do not remove expanded directories in parents.

(fn &optional ARG)" t nil) (autoload 'dired-subtree-only-this-directory "dired-subtree" "Remove all the siblings on the route from this directory to the top-most directory.

With ARG non-nil, do not remove expanded directories in parents.

(fn &optional ARG)" t nil) (autoload 'dired-subtree-apply-filter "dired-subtree" "Push a local filter for this subtree.

This depends on `dired-filter' package.

It works exactly the same as global dired filters, only
restricted to a subtree.  The global filter is also applied to
the subtree.  The filter action is read from `dired-filter-map'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-subtree" '("dired-"))) (provide 'dired-subtree-autoloads)) "dired-narrow" ((dired-narrow-autoloads dired-narrow) (autoload 'dired-narrow-regexp "dired-narrow" "Narrow a dired buffer to the files matching a regular expression." t nil) (autoload 'dired-narrow "dired-narrow" "Narrow a dired buffer to the files matching a string.

If the string contains spaces, then each word is matched against
the file name separately.  To succeed, all of them have to match
but the order does not matter.

For example \"foo bar\" matches filename \"bar-and-foo.el\"." t nil) (autoload 'dired-narrow-fuzzy "dired-narrow" "Narrow a dired buffer to the files matching a fuzzy string.

A fuzzy string is constructed from the filter string by inserting
\".*\" between each letter.  This is then matched as regular
expression against the file name." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-narrow" '("dired-narrow-"))) (provide 'dired-narrow-autoloads)) "vscode-icon" ((vscode-icon vscode-icon-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vscode-icon" '("vscode-icon-"))) (provide 'vscode-icon-autoloads)) "dired-sidebar" ((dired-sidebar-autoloads dired-sidebar) (autoload 'dired-sidebar-toggle-sidebar "dired-sidebar" "Toggle the project explorer window.
Optional argument DIR Use DIR as sidebar root if available.

With universal argument, use current directory.

(fn &optional DIR)" t nil) (autoload 'dired-sidebar-toggle-with-current-directory "dired-sidebar" "Like `dired-sidebar-toggle-sidebar' but use current-directory." t nil) (autoload 'dired-sidebar-show-sidebar "dired-sidebar" "Show sidebar displaying buffer B.

(fn &optional B)" t nil) (autoload 'dired-sidebar-hide-sidebar "dired-sidebar" "Hide the sidebar in the selected frame." t nil) (autoload 'dired-sidebar-jump-to-sidebar "dired-sidebar" "Jump to `dired-sidebar' buffer if it is showing.

If it's not showing, act as `dired-sidebar-toggle-sidebar'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-sidebar" '("dired-sidebar-"))) (provide 'dired-sidebar-autoloads)) "diredfl" ((diredfl-autoloads diredfl) (autoload 'diredfl-mode "diredfl" "Enable additional font locking in `dired-mode'.

If called interactively, enable Diredfl mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'diredfl-global-mode 'globalized-minor-mode t) (defvar diredfl-global-mode nil "Non-nil if Diredfl-Global mode is enabled.
See the `diredfl-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `diredfl-global-mode'.") (custom-autoload 'diredfl-global-mode "diredfl" nil) (autoload 'diredfl-global-mode "diredfl" "Toggle Diredfl mode in all buffers.
With prefix ARG, enable Diredfl-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Diredfl mode is enabled in all buffers where
`(lambda nil (when (derived-mode-p 'dired-mode) (diredfl-mode)))' would do it.
See `diredfl-mode' for more information on Diredfl mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "diredfl" '("diredfl-"))) (provide 'diredfl-autoloads)) "dired-git-info" ((dired-git-info dired-git-info-autoloads) (autoload 'dired-git-info-mode "dired-git-info" "Toggle git message info in current dired buffer.

If called interactively, enable Dired-Git-Info mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'dired-git-info-auto-enable "dired-git-info" "Enable `dired-git-info-mode' if current dired buffer is in a git repo.

Add this function to `dired-after-readin-hook' to enable the mode
automatically inside git repos." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-git-info" '("dgi-"))) (provide 'dired-git-info-autoloads)) "elisp-slime-nav" ((elisp-slime-nav-autoloads elisp-slime-nav) (autoload 'elisp-slime-nav-mode "elisp-slime-nav" "Enable Slime-style navigation of elisp symbols using M-. and M-,

If called interactively, enable Elisp-Slime-Nav mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'turn-on-elisp-slime-nav-mode 'elisp-slime-nav-mode "2020-01-30") (autoload 'elisp-slime-nav-find-elisp-thing-at-point "elisp-slime-nav" "Find the elisp thing at point, be it a function, variable, library or face.

With a prefix arg, or if there is no thing at point, prompt for
the symbol to jump to.

Argument SYM-NAME is the thing to find.

(fn SYM-NAME)" t nil) (autoload 'elisp-slime-nav-describe-elisp-thing-at-point "elisp-slime-nav" "Display the full documentation of the elisp thing at point.

The named subject may be a function, variable, library or face.

With a prefix arg, or if there is not \"thing\" at point, prompt
for the symbol to jump to.

Argument SYM-NAME is the thing to find.

(fn SYM-NAME)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-slime-nav" '("elisp-slime-nav-"))) (provide 'elisp-slime-nav-autoloads)) "macrostep" ((macrostep-c macrostep-autoloads macrostep) (autoload 'macrostep-mode "macrostep" "Minor mode for inline expansion of macros in Emacs Lisp source buffers.

If called interactively, enable Macrostep mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\<macrostep-keymap>Progressively expand macro forms with \\[macrostep-expand], collapse them with \\[macrostep-collapse],
and move back and forth with \\[macrostep-next-macro] and \\[macrostep-prev-macro].
Use \\[macrostep-collapse-all] or collapse all visible expansions to
quit and return to normal editing.

\\{macrostep-keymap}

(fn &optional ARG)" t nil) (autoload 'macrostep-expand "macrostep" "Expand the macro form following point by one step.

Enters `macrostep-mode' if it is not already active, making the
buffer temporarily read-only. If macrostep-mode is active and the
form following point is not a macro form, search forward in the
buffer and expand the next macro form found, if any.

With a prefix argument, the expansion is displayed in a separate
buffer instead of inline in the current buffer.  Setting
`macrostep-expand-in-separate-buffer' to non-nil swaps these two
behaviors.

(fn &optional TOGGLE-SEPARATE-BUFFER)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "macrostep" '("macrostep-"))) (autoload 'macrostep-c-mode-hook "macrostep-c" nil nil nil) (add-hook 'c-mode-hook #'macrostep-c-mode-hook) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "macrostep-c" '("macrostep-c-"))) (provide 'macrostep-autoloads)) "paredit" ((paredit-autoloads paredit) (autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  unbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are unbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing unbalanced parentheses instead.
\\<paredit-mode-map>

If called interactively, enable Paredit mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "paredit" '("?\\" "disable-paredit-mode" "paredit-"))) (provide 'paredit-autoloads)) "highlight-parentheses" ((highlight-parentheses highlight-parentheses-autoloads) (autoload 'highlight-parentheses-mode "highlight-parentheses" "Minor mode to highlight the surrounding parentheses.

If called interactively, enable Highlight-Parentheses mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-highlight-parentheses-mode 'globalized-minor-mode t) (defvar global-highlight-parentheses-mode nil "Non-nil if Global Highlight-Parentheses mode is enabled.
See the `global-highlight-parentheses-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-highlight-parentheses-mode'.") (custom-autoload 'global-highlight-parentheses-mode "highlight-parentheses" nil) (autoload 'global-highlight-parentheses-mode "highlight-parentheses" "Toggle Highlight-Parentheses mode in all buffers.
With prefix ARG, enable Global Highlight-Parentheses mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Highlight-Parentheses mode is enabled in all buffers where
`(lambda nil (highlight-parentheses-mode 1))' would do it.
See `highlight-parentheses-mode' for more information on Highlight-Parentheses mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "highlight-parentheses" '("highlight-parentheses-" "hl-paren-face"))) (provide 'highlight-parentheses-autoloads)) "eshell-git-prompt" ((eshell-git-prompt eshell-git-prompt-autoloads) (autoload 'eshell-git-prompt-use-theme "eshell-git-prompt" "Pick up a Eshell prompt theme from `eshell-git-prompt-themes' to use.

(fn THEME)" t nil) (autoload 'eshell/use-theme "eshell-git-prompt" "List all available themes and pick one from Eshell.

(fn &optional THEME)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eshell-git-prompt" '("eshell-git-prompt-" "with-face"))) (provide 'eshell-git-prompt-autoloads)) "eyebrowse" ((eyebrowse eyebrowse-autoloads) (autoload 'eyebrowse-setup-evil-keys "eyebrowse" "Set up key bindings specific to Evil.
Currently only gt, gT, gc and zx are supported." nil nil) (autoload 'eyebrowse-setup-opinionated-keys "eyebrowse" "Set up more opinionated key bindings for using eyebrowse.

M-0..M-9, C-< / C->, C-'and C-\" are used for switching.  If
IGNORE-EVIL isn't set and Evil is detected, extra key bindings
will be set up with `eyebrowse-setup-evil-keys' as well.

(fn &optional IGNORE-EVIL)" nil nil) (defvar eyebrowse-mode nil "Non-nil if Eyebrowse mode is enabled.
See the `eyebrowse-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eyebrowse-mode'.") (custom-autoload 'eyebrowse-mode "eyebrowse" nil) (autoload 'eyebrowse-mode "eyebrowse" "Toggle `eyebrowse-mode'.
This global minor mode provides a set of keybindings for
switching window configurations.  It tries mimicking the tab
behaviour of `ranger`, a file manager.

If called interactively, enable Eyebrowse mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eyebrowse" '("eyebrowse-"))) (provide 'eyebrowse-autoloads)) "epl" ((epl-autoloads epl) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epl" '("epl-"))) (provide 'epl-autoloads)) "pkg-info" ((pkg-info-autoloads pkg-info) (autoload 'pkg-info-library-original-version "pkg-info" "Get the original version in the header of LIBRARY.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no X-Original-Version
header.

See Info node `(elisp)Library Headers' for more information
about library headers.

(fn LIBRARY &optional SHOW)" t nil) (autoload 'pkg-info-library-version "pkg-info" "Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers.

(fn LIBRARY &optional SHOW)" t nil) (autoload 'pkg-info-defining-library-original-version "pkg-info" "Get the original version of the library defining FUNCTION.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

(fn FUNCTION &optional SHOW)" t nil) (autoload 'pkg-info-defining-library-version "pkg-info" "Get the version of the library defining FUNCTION.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

(fn FUNCTION &optional SHOW)" t nil) (autoload 'pkg-info-package-version "pkg-info" "Get the version of an installed PACKAGE.

If SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed.

(fn PACKAGE &optional SHOW)" t nil) (autoload 'pkg-info-version-info "pkg-info" "Obtain complete version info for LIBRARY and PACKAGE.

LIBRARY is a symbol denoting a named feature, or a library name
as string.  PACKAGE is a symbol denoting an ELPA package.  If
omitted or nil, default to LIBRARY.

If SHOW is non-nil, show the version in the minibuffer.

When called interactively, prompt for LIBRARY.  When called
interactively with prefix argument, prompt for PACKAGE as well.

Return a string with complete version information for LIBRARY.
This version information contains the version from the headers of
LIBRARY, and the version of the installed PACKAGE, the LIBRARY is
part of.  If PACKAGE is not installed, or if the PACKAGE version
is the same as the LIBRARY version, do not include a package
version.

(fn LIBRARY &optional PACKAGE SHOW)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pkg-info" '("pkg-info-"))) (provide 'pkg-info-autoloads)) "let-alist" ((let-alist let-alist-autoloads) (autoload 'let-alist "let-alist" "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site
      .site.contents))

essentially expands to

  (let ((.title (cdr (assq \\='title alist)))
        (.body  (cdr (assq \\='body alist)))
        (.site  (cdr (assq \\='site alist)))
        (.site.contents (cdr (assq \\='contents (cdr (assq \\='site alist))))))
    (if (and .title .body)
        .body
      .site
      .site.contents))

If you nest `let-alist' invocations, the inner one can't access
the variables of the outer one. You can, however, access alists
inside the original alist by using dots inside the symbol, as
displayed in the example above.

(fn ALIST &rest BODY)" nil t) (function-put 'let-alist 'lisp-indent-function '1) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "let-alist" '("let-alist--"))) (provide 'let-alist-autoloads)) "flycheck" ((flycheck flycheck-autoloads flycheck-ert flycheck-buttercup) (autoload 'flycheck-manual "flycheck" "Open the Flycheck manual." t nil) (autoload 'flycheck-mode "flycheck" "Flycheck is a minor mode for on-the-fly syntax checking.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

If you run into issues, use `\\[flycheck-verify-setup]' to get help.

Flycheck supports many languages out of the box, and many
additional ones are available on MELPA.  Adding new ones is very
easy.  Complete documentation is available online at URL
`https://www.flycheck.org/en/latest/'.  Please report issues and
request features at URL `https://github.com/flycheck/flycheck'.

Flycheck displays its status in the mode line.  In the default
configuration, it looks like this:

`FlyC'     This buffer has not been checked yet.
`FlyC-'    Flycheck doesn't have a checker for this buffer.
`FlyC*'    Flycheck is running.  Expect results soon!
`FlyC:3|2' This buffer contains three warnings and two errors.
           Use `\\[flycheck-list-errors]' to see the list.

You may also see the following icons:
`FlyC!'    The checker crashed.
`FlyC.'    The last syntax check was manually interrupted.
`FlyC?'    The checker did something unexpected, like exiting with 1
           but returning no errors.

The following keybindings are available in `flycheck-mode':

\\{flycheck-mode-map}
(you can change the prefix by customizing
`flycheck-keymap-prefix')

If called interactively, enable Flycheck mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is \342\200\230toggle\342\200\231; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-flycheck-mode 'globalized-minor-mode t) (defvar global-flycheck-mode nil "Non-nil if Global Flycheck mode is enabled.
See the `global-flycheck-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-mode'.") (custom-autoload 'global-flycheck-mode "flycheck" nil) (autoload 'global-flycheck-mode "flycheck" "Toggle Flycheck mode in all buffers.
With prefix ARG, enable Global Flycheck mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Flycheck mode is enabled in all buffers where
`flycheck-mode-on-safe' would do it.
See `flycheck-mode' for more information on Flycheck mode.

(fn &optional ARG)" t nil) (autoload 'flycheck-define-error-level "flycheck" "Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0; info is -10, warning is 10, and
     error is 100.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:compilation-level LEVEL'

     A number indicating the broad class of messages that errors
     at this level belong to: one of 0 (info), 1 (warning), or
     2 or nil (error).  Defaults to nil.

     This is used by `flycheck-checker-pattern-to-error-regexp'
     to map error levels into `compilation-mode''s hierarchy and
     to get proper highlighting of errors in `compilation-mode'.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Another useful
     property for error level categories is `priority', to
     influence the stacking of multiple error level overlays.

`:fringe-bitmap BITMAPS'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level, or a cons of two bitmaps (one for
     narrow fringes and one for wide fringes).  See Info node
     `(elisp)Fringe Bitmaps' for more information about fringe
     bitmaps, including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:margin-spec SPEC'
     A display specification indicating what to display in the
     margin when `flycheck-indication-mode' is `left-margin' or
     `right-margin'.  See Info node `(elisp)Displaying in the
     Margins'.  If omitted, Flycheck generates an image spec from
     the fringe bitmap.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'.

(fn LEVEL &rest PROPERTIES)" nil nil) (function-put 'flycheck-define-error-level 'lisp-indent-function '1) (autoload 'flycheck-define-command-checker "flycheck" "Define SYMBOL as syntax checker to run a command.

Define SYMBOL as generic syntax checker via
`flycheck-define-generic-checker', which uses an external command
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`flycheck-define-generic-checker'.

In addition to the properties understood by
`flycheck-define-generic-checker', the following PROPERTIES
constitute a command syntax checker.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of command checkers is `flycheck-sanitize-errors'.

`:command COMMAND'
     The command to run for syntax checking.

     COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns PATTERNS'
     A list of patterns to parse the output of the `:command'.

     Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
     LEVEL is a Flycheck error level (see
     `flycheck-define-error-level'), followed by one or more RX
     `SEXP's which parse an error of that level and extract line,
     column, file name and the message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for some special RX forms provided
     by Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

     This property is optional.  If omitted, however, an
     `:error-parser' is mandatory.

`:error-parser FUNCTION'
     A function to parse errors with.

     The function shall accept three arguments OUTPUT CHECKER
     BUFFER.  OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

     This property is optional.  If omitted, it defaults to
     `flycheck-parse-with-patterns'.  In this case,
     `:error-patterns' is mandatory.

`:standard-input t'
     Whether to send the buffer contents on standard input.

     If this property is given and has a non-nil value, send the
     contents of the buffer on standard input.

     Defaults to nil.

Note that you may not give `:start', `:interrupt', and
`:print-doc' for a command checker.  You can give a custom
`:verify' function, though, whose results will be appended to the
default `:verify' function of command checkers.

(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil nil) (function-put 'flycheck-define-command-checker 'lisp-indent-function '1) (function-put 'flycheck-define-command-checker 'doc-string-elt '2) (autoload 'flycheck-def-config-file-var "flycheck" "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable using `defcustom', to
provide configuration files for the given syntax CHECKER.
CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.  It can be either a string or a list of
strings.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'.

(fn SYMBOL CHECKER &optional FILE-NAME &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-config-file-var 'lisp-indent-function '3) (autoload 'flycheck-def-option-var "flycheck" "Define SYMBOL as option variable with INIT-VALUE for CHECKER.

SYMBOL is declared as customizable variable using `defcustom', to
provide an option for the given syntax CHECKERS (a checker or a
list of checkers).  INIT-VALUE is the initial value of the
variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
forwarded to `defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'.

(fn SYMBOL INIT-VALUE CHECKERS DOCSTRING &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-option-var 'lisp-indent-function '3) (function-put 'flycheck-def-option-var 'doc-string-elt '4) (autoload 'flycheck-define-checker "flycheck" "Define SYMBOL as command syntax checker with DOCSTRING and PROPERTIES.

Like `flycheck-define-command-checker', but PROPERTIES must not
be quoted.  Also, implicitly define the executable variable for
SYMBOL with `flycheck-def-executable-var'.

(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil t) (function-put 'flycheck-define-checker 'lisp-indent-function '1) (function-put 'flycheck-define-checker 'doc-string-elt '2) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck" '("flycheck-" "help-flycheck-checker-d" "list-flycheck-errors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-buttercup" '("flycheck-buttercup-format-error-list"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-ert" '("flycheck-er"))) (provide 'flycheck-autoloads)) "evil-leader" ((evil-leader evil-leader-autoloads) (autoload 'global-evil-leader-mode "evil-leader" "Global minor mode for <leader> support.

If called interactively, enable Global Evil-Leader mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'evil-leader-mode "evil-leader" "Minor mode to enable <leader> support.

If called interactively, enable Evil-Leader mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'evil-leader/set-key "evil-leader" "Bind `key' to command `def' in `evil-leader/default-map'.

Key has to be readable by `read-kbd-macro' and `def' a command.
Accepts further `key' `def' pairs.

(fn KEY DEF &rest BINDINGS)" t nil) (autoload 'evil-leader/set-key-for-mode "evil-leader" "Create keybindings for major-mode `mode' with `key' bound to command `def'.

See `evil-leader/set-key'.

(fn MODE KEY DEF &rest BINDINGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-leader" '("evil-leader"))) (provide 'evil-leader-autoloads)) "async" ((async-pkg async-autoloads async-bytecomp smtpmail-async dired-async async) (autoload 'async-start-process "async" "Start the executable PROGRAM asynchronously named NAME.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory.

(fn NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS)" nil nil) (autoload 'async-start "async" "Execute START-FUNC (often a lambda) in a subordinate Emacs process.
When done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message \"This is a test\")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message \"Async process done, result should be 222: %s\"
                  result)))

If FINISH-FUNC is nil or missing, a future is returned that can
be inspected using `async-get', blocking until the value is
ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message \"This is a test\")
                     (sleep-for 3)
                     222))))

        (message \"I'm going to do some work here\") ;; ....

        (message \"Waiting on async process, result should be 222: %s\"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any
return value from the child process, pass the `ignore' symbol as
the second argument (if you don't, and never call `async-get', it
will leave *emacs* process buffers hanging around):

    (async-start
     (lambda ()
       (delete-file \"a remote file on a slow link\" nil))
     'ignore)

Note: Even when FINISH-FUNC is present, a future is still
returned except that it yields no value (since the value is
passed to FINISH-FUNC).  Call `async-get' on such a future always
returns nil.  It can still be useful, however, as an argument to
`async-ready' or `async-wait'.

(fn START-FUNC &optional FINISH-FUNC)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async" '("async-"))) (autoload 'async-byte-recompile-directory "async-bytecomp" "Compile all *.el files in DIRECTORY asynchronously.
All *.elc files are systematically deleted before proceeding.

(fn DIRECTORY &optional QUIET)" nil nil) (defvar async-bytecomp-package-mode nil "Non-nil if Async-Bytecomp-Package mode is enabled.
See the `async-bytecomp-package-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `async-bytecomp-package-mode'.") (custom-autoload 'async-bytecomp-package-mode "async-bytecomp" nil) (autoload 'async-bytecomp-package-mode "async-bytecomp" "Byte compile asynchronously packages installed with package.el.
Async compilation of packages can be controlled by
`async-bytecomp-allowed-packages'.

If called interactively, enable Async-Bytecomp-Package mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'async-byte-compile-file "async-bytecomp" "Byte compile Lisp code FILE asynchronously.

Same as `byte-compile-file' but asynchronous.

(fn FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async-bytecomp" '("async-byte"))) (defvar dired-async-mode nil "Non-nil if Dired-Async mode is enabled.
See the `dired-async-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dired-async-mode'.") (custom-autoload 'dired-async-mode "dired-async" nil) (autoload 'dired-async-mode "dired-async" "Do dired actions asynchronously.

If called interactively, enable Dired-Async mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-copy "dired-async" "Run \342\200\230dired-do-copy\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-symlink "dired-async" "Run \342\200\230dired-do-symlink\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-hardlink "dired-async" "Run \342\200\230dired-do-hardlink\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-rename "dired-async" "Run \342\200\230dired-do-rename\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-async" '("dired-async-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smtpmail-async" '("async-smtpmail-"))) (provide 'async-autoloads)) "popup" ((popup popup-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "popup" '("popup-"))) (provide 'popup-autoloads)) "helm-core" ((helm-core-autoloads helm helm-source helm-core-pkg helm-lib helm-multi-match) (autoload 'helm-configuration "helm" "Customize Helm." t nil) (autoload 'helm-define-multi-key "helm" "In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function runs sequentially for each KEY press.
If DELAY is specified, switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list take no args.
E.g.
    (defun foo ()
      (interactive)
      (message \"Run foo\"))
    (defun bar ()
      (interactive)
      (message \"Run bar\"))
    (defun baz ()
      (interactive)
      (message \"Run baz\"))

(helm-define-multi-key global-map (kbd \"<f5> q\") '(foo bar baz) 2)

Each time \"<f5> q\" is pressed, the next function is executed.
Waiting more than 2 seconds between key presses switches back to
executing the first function on the next hit.

(fn KEYMAP KEY FUNCTIONS &optional DELAY)" nil nil) (autoload 'helm-multi-key-defun "helm" "Define NAME as a multi-key command running FUNS.
After DELAY seconds, the FUNS list is reinitialized.
See `helm-define-multi-key'.

(fn NAME DOCSTRING FUNS &optional DELAY)" nil t) (function-put 'helm-multi-key-defun 'lisp-indent-function '2) (autoload 'helm-define-key-with-subkeys "helm" "Define in MAP a KEY and SUBKEY to COMMAND.

This allows typing KEY to call COMMAND the first time and
type only SUBKEY on subsequent calls.

Arg MAP is the keymap to use, SUBKEY is the initial short
key binding to call COMMAND.

Arg OTHER-SUBKEYS is an alist specifying other short key bindings
to use once started, e.g.:

    (helm-define-key-with-subkeys global-map
       (kbd \"C-x v n\") ?n 'git-gutter:next-hunk
       '((?p . git-gutter:previous-hunk)))

In this example, `C-x v n' will run `git-gutter:next-hunk'
subsequent \"n\" will run this command again and subsequent \"p\"
will run `git-gutter:previous-hunk'.

If specified PROMPT can be displayed in minibuffer to describe
SUBKEY and OTHER-SUBKEYS.  Arg EXIT-FN specifies a function to run
on exit.

For any other key pressed, run their assigned command as defined
in MAP and then exit the loop running EXIT-FN, if specified.

NOTE: SUBKEY and OTHER-SUBKEYS bindings support only char syntax
and vectors, so don't use strings to define them.

(fn MAP KEY SUBKEY COMMAND &optional OTHER-SUBKEYS PROMPT EXIT-FN)" nil nil) (function-put 'helm-define-key-with-subkeys 'lisp-indent-function '1) (autoload 'helm-debug-open-last-log "helm" "Open Helm log file or buffer of last Helm session." t nil) (autoload 'helm "helm" "Main function to execute helm sources.

PLIST is a list like

(:key1 val1 :key2 val2 ...)

 or

(&optional sources input prompt resume preselect
            buffer keymap default history allow-nest).

** Keywords

Keywords supported:

- :sources
- :input
- :prompt
- :resume
- :preselect
- :buffer
- :keymap
- :default
- :history
- :allow-nest

Extra LOCAL-VARS keywords are supported, see the \"** Other
keywords\" section below.

Basic keywords are the following:

*** :sources

One of the following:

- List of sources
- Symbol whose value is a list of sources
- Alist representing a Helm source.
  - In this case the source has no name and is referenced in
    `helm-sources' as a whole alist.

*** :input

Initial input of minibuffer (temporary value of `helm-pattern')

*** :prompt

Minibuffer prompt. Default value is `helm--prompt'.

*** :resume

If t, allow resumption of the previous session of this Helm
command, skipping initialization.

If 'noresume, this instance of `helm' cannot be resumed.

*** :preselect

Initially selected candidate (string or regexp).

*** :buffer

Buffer name for this Helm session. `helm-buffer' will take this value.

*** :keymap

[Obsolete]

Keymap used at the start of this Helm session.

It is overridden by keymaps specified in sources, and is kept
only for backward compatibility.

Keymaps should be specified in sources using the :keymap slot
instead. See `helm-source'.

This keymap is not restored by `helm-resume'.

*** :default

Default value inserted into the minibuffer with
\\<minibuffer-local-map>\\[next-history-element].

It can be a string or a list of strings, in this case
\\<minibuffer-local-map>\\[next-history-element] cycles through
the list items, starting with the first.

If nil, `thing-at-point' is used.

If `helm-maybe-use-default-as-input' is non-nil, display is
updated using this value if this value matches, otherwise it is
ignored. If :input is specified, it takes precedence on :default.

*** :history

Minibuffer input, by default, is pushed to `minibuffer-history'.

When an argument HISTORY is provided, input is pushed to
HISTORY. HISTORY should be a valid symbol.

*** :allow-nest

Allow running this Helm command in a running Helm session.

** Other keywords

Other keywords are interpreted as local variables of this Helm
session. The `helm-' prefix can be omitted. For example,

(helm :sources 'helm-source-buffers-list
       :buffer \"*helm buffers*\"
       :candidate-number-limit 10)

Starts a Helm session with the variable
`helm-candidate-number-limit' set to 10.

** Backward compatibility

For backward compatibility, positional parameters are
supported:

(helm sources input prompt resume preselect
       buffer keymap default history allow-nest)

However, the use of non-keyword args is deprecated.

(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY ALLOW-NEST OTHER-LOCAL-VARS)" nil nil) (autoload 'helm-cycle-resume "helm" "Cycle in `helm-buffers' list and resume when waiting more than 1.2s." t nil) (autoload 'helm-other-buffer "helm" "Simplified Helm interface with other `helm-buffer'.
Call `helm' only with SOURCES and BUFFER as args.

(fn SOURCES BUFFER)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm" '("helm-" "with-helm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-lib" '("helm-" "with-helm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-multi-match" '("helm-m"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-source" '("helm-"))) (provide 'helm-core-autoloads)) "helm" ((helm-adaptive helm-mode helm-locate helm-for-files helm-grep helm-color helm-eval helm-sys helm-find helm-man helm-global-bindings helm-dabbrev helm-external helm-net helm-misc helm-occur helm-comint helm-buffers helm-files helm-regexp helm-config helm-ring helm-easymenu helm-semantic helm-id-utils helm-autoloads helm-font helm-elisp-package helm-shell helm-tags helm-x-files helm-eshell helm-imenu helm-pkg helm-help helm-elisp helm-bookmark helm-types helm-command helm-info helm-utils) (defvar helm-adaptive-mode nil "Non-nil if Helm-Adaptive mode is enabled.
See the `helm-adaptive-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-adaptive-mode'.") (custom-autoload 'helm-adaptive-mode "helm-adaptive" nil) (autoload 'helm-adaptive-mode "helm-adaptive" "Toggle adaptive sorting in all sources.

If called interactively, enable Helm-Adaptive mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'helm-reset-adaptive-history "helm-adaptive" "Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted
`helm-adaptive-history-file'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-adaptive" '("helm-adapt"))) (autoload 'helm-bookmarks "helm-bookmark" "Preconfigured `helm' for bookmarks." t nil) (autoload 'helm-filtered-bookmarks "helm-bookmark" "Preconfigured `helm' for bookmarks (filtered by category).
Optional source `helm-source-bookmark-addressbook' is loaded only
if external addressbook-bookmark package is installed." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-bookmark" '("bmkext-jump-" "bookmark" "helm-"))) (autoload 'helm-buffers-list "helm-buffers" "Preconfigured `helm' to list buffers." t nil) (autoload 'helm-mini "helm-buffers" "Preconfigured `helm' displaying `helm-mini-default-sources'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-buffers" '("helm-"))) (autoload 'helm-colors "helm-color" "Preconfigured `helm' for color." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-color" '("helm-"))) (autoload 'helm-comint-prompts "helm-comint" "Pre-configured `helm' to browse the prompts of the current comint buffer." t nil) (autoload 'helm-comint-prompts-all "helm-comint" "Pre-configured `helm' to browse the prompts of all comint sessions." t nil) (autoload 'helm-comint-input-ring "helm-comint" "Preconfigured `helm' that provide completion of `comint' history." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-comint" '("helm-"))) (autoload 'helm-M-x "helm-command" "Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x'
`execute-extended-command'.

Unlike regular `M-x' Emacs vanilla `execute-extended-command'
command, the prefix args if needed, can be passed AFTER starting
`helm-M-x'.  When a prefix arg is passed BEFORE starting
`helm-M-x', the first `C-u' while in `helm-M-x' session will
disable it.

You can get help on each command by persistent action.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-command" '("helm-"))) (autoload 'helm-dabbrev "helm-dabbrev" "Preconfigured helm for dynamic abbreviations." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-dabbrev" '("helm-dabbrev-"))) (autoload 'helm-lisp-completion-at-point "helm-elisp" "Preconfigured Helm for Lisp symbol completion at point." t nil) (autoload 'helm-complete-file-name-at-point "helm-elisp" "Preconfigured Helm to complete file name at point.

(fn &optional FORCE)" t nil) (autoload 'helm-lisp-indent "helm-elisp" nil t nil) (autoload 'helm-lisp-completion-or-file-name-at-point "helm-elisp" "Preconfigured Helm to complete Lisp symbol or filename at point.
Filename completion happens if string start after or between a
double quote." t nil) (autoload 'helm-apropos "helm-elisp" "Preconfigured Helm to describe commands, functions, variables and faces.
In non interactives calls DEFAULT argument should be provided as
a string, i.e. the `symbol-name' of any existing symbol.

(fn DEFAULT)" t nil) (autoload 'helm-manage-advice "helm-elisp" "Preconfigured `helm' to disable/enable function advices." t nil) (autoload 'helm-locate-library "helm-elisp" "Preconfigured helm to locate elisp libraries." t nil) (autoload 'helm-timers "helm-elisp" "Preconfigured `helm' for timers." t nil) (autoload 'helm-complex-command-history "helm-elisp" "Preconfigured `helm' for complex command history." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-elisp" '("helm-" "with-helm-show-completion"))) (autoload 'helm-list-elisp-packages "helm-elisp-package" "Preconfigured `helm' for listing and handling Emacs packages.

(fn ARG)" t nil) (autoload 'helm-list-elisp-packages-no-fetch "helm-elisp-package" "Preconfigured Helm for Emacs packages.

Same as `helm-list-elisp-packages' but don't fetch packages on
remote.  Called with a prefix ARG always fetch packages on
remote.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-elisp-package" '("helm-"))) (autoload 'helm-esh-pcomplete "helm-eshell" "Preconfigured `helm' to provide Helm completion in Eshell." t nil) (autoload 'helm-eshell-history "helm-eshell" "Preconfigured Helm for Eshell history." t nil) (autoload 'helm-eshell-prompts "helm-eshell" "Pre-configured `helm' to browse the prompts of the current Eshell." t nil) (autoload 'helm-eshell-prompts-all "helm-eshell" "Pre-configured `helm' to browse the prompts of all Eshell sessions." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-eshell" '("helm-e"))) (autoload 'helm-eval-expression "helm-eval" "Preconfigured `helm' for `helm-source-evaluation-result'.

(fn ARG)" t nil) (autoload 'helm-eval-expression-with-eldoc "helm-eval" "Preconfigured `helm' for `helm-source-evaluation-result' with `eldoc' support." t nil) (autoload 'helm-calcul-expression "helm-eval" "Preconfigured `helm' for `helm-source-calculation-result'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-eval" '("helm-"))) (autoload 'helm-run-external-command "helm-external" "Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-external-commands-list'.

(fn PROGRAM)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-external" '("helm-"))) (defvar helm-ff-cache-mode nil "Non-nil if Helm-Ff-Cache mode is enabled.
See the `helm-ff-cache-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-ff-cache-mode'.") (custom-autoload 'helm-ff-cache-mode "helm-files" nil) (autoload 'helm-ff-cache-mode "helm-files" "Auto refresh `helm-find-files' cache when emacs is idle.

If called interactively, enable Helm-Ff-Cache mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

You probably don't want to start this mode directly.  Instead you
should customize `helm-ff-keep-cached-candidates' to a non nil
value to enable it.

With `helm-ff-keep-cached-candidates' set to a nil value the mode
will disable itself.

When Emacs is idle, refresh the cache all the
`helm-ff-refresh-cache-delay' seconds then stop when done or after
`helm-ff-cache-mode-max-idle-time' if emacs is still idle.

(fn &optional ARG)" t nil) (autoload 'helm-projects-history "helm-files" "

(fn ARG)" t nil) (autoload 'helm-browse-project "helm-files" "Preconfigured helm to browse projects.
Browse files and see status of project with its VCS.
Only HG and GIT are supported for now.
Fall back to `helm-browse-project-find-files' if current
directory is not under control of one of those VCS.
With a prefix ARG browse files recursively, with two prefix ARG
rebuild the cache.
If the current directory is found in the cache, start
`helm-browse-project-find-files' even with no prefix ARG.
NOTE: The prefix ARG have no effect on the VCS controlled
directories.

Needed dependencies for VCS:
<https://github.com/emacs-helm/helm-ls-git>
and
<https://github.com/emacs-helm/helm-ls-hg>.

(fn ARG)" t nil) (autoload 'helm-find-files "helm-files" "Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on
files.

(fn ARG)" t nil) (autoload 'helm-delete-tramp-connection "helm-files" "Allow deleting tramp connection or marked tramp connections at once.

This replace `tramp-cleanup-connection' which is partially broken
in Emacs < to 25.1.50.1 (See Emacs Bug#24432).

It allows additionally to delete more than one connection at
once." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-files" '("eshell-command-aliases-list" "helm-"))) (autoload 'helm-find "helm-find" "Preconfigured `helm' for the find shell command.

Recursively find files whose names are matched by all specified
globbing PATTERNs under the current directory using the external
program specified in `find-program' (usually \"find\").  Every
input PATTERN is silently wrapped into two stars: *PATTERN*.

With prefix argument, prompt for a directory to search.

When user option `helm-findutils-search-full-path' is non-nil,
match against complete paths, otherwise, against file names
without directory part.

The (possibly empty) list of globbing PATTERNs can be followed by
the separator \"*\" plus any number of additional arguments that
are passed to \"find\" literally.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-find" '("helm-"))) (autoload 'helm-select-xfont "helm-font" "Preconfigured `helm' to select Xfont." t nil) (autoload 'helm-ucs "helm-font" "Preconfigured `helm' for `ucs-names'.

Called with a prefix arg force reloading cache.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-font" '("helm-"))) (autoload 'helm-for-files "helm-for-files" "Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'." t nil) (autoload 'helm-multi-files "helm-for-files" "Preconfigured helm like `helm-for-files' but running locate only on demand.

Allow toggling back and forth from locate to others sources with
`helm-multi-files-toggle-locate-binding' key.
This avoids launching locate needlessly when what you are
searching for is already found." t nil) (autoload 'helm-recentf "helm-for-files" "Preconfigured `helm' for `recentf'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-for-files" '("helm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-global-bindings" '("helm-"))) (autoload 'helm-goto-precedent-file "helm-grep" "Go to previous file in Helm grep/etags buffers." t nil) (autoload 'helm-goto-next-file "helm-grep" "Go to previous file in Helm grep/etags buffers." t nil) (autoload 'helm-do-grep-ag "helm-grep" "Preconfigured `helm' for grepping with AG in `default-directory'.
With prefix arg prompt for type if available with your AG
version.

(fn ARG)" t nil) (autoload 'helm-grep-do-git-grep "helm-grep" "Preconfigured `helm' for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-grep" '("helm-"))) (autoload 'helm-documentation "helm-help" "Preconfigured `helm' for Helm documentation.
With a prefix arg refresh the documentation.

Find here the documentation of all documented sources." t nil) (defvar helm-comp-read-mode-line "\\<helm-comp-read-map>C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf") (defvar helm-read-file-name-mode-line-string "\\<helm-read-file-map>\\[helm-help]:Help C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf" "String displayed in mode-line in `helm-source-find-files'.") (defvar helm-top-mode-line "\\<helm-top-map>\\[helm-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-help" '("helm-"))) (autoload 'helm-gid "helm-id-utils" "Preconfigured `helm' for `gid' command line of `ID-Utils'.
Need A database created with the command `mkid' above
`default-directory'.
Need id-utils as dependency which provide `mkid', `gid' etc..
See <https://www.gnu.org/software/idutils/>." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-id-utils" '("helm-gid-"))) (autoload 'helm-imenu "helm-imenu" "Preconfigured `helm' for `imenu'." t nil) (autoload 'helm-imenu-in-all-buffers "helm-imenu" "Preconfigured `helm' for fetching imenu entries in all buffers with similar mode as current.
A mode is similar as current if it is the same, it is derived
i.e. `derived-mode-p' or it have an association in
`helm-imenu-all-buffer-assoc'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-imenu" '("helm-"))) (autoload 'helm-info "helm-info" "Preconfigured `helm' for searching Info files' indices.

With a prefix argument \\[universal-argument], set REFRESH to
non-nil.

Optional parameter REFRESH, when non-nil, re-evaluates
`helm-default-info-index-list'.  If the variable has been
customized, set it to its saved value.  If not, set it to its
standard value. See `custom-reevaluate-setting' for more.

REFRESH is useful when new Info files are installed.  If
`helm-default-info-index-list' has not been customized, the new
Info files are made available.

(fn &optional REFRESH)" t nil) (autoload 'helm-info-at-point "helm-info" "Preconfigured `helm' for searching info at point." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-info" '("helm-"))) (autoload 'helm-projects-find-files "helm-locate" "Find files with locate in `helm-locate-project-list'.
With a prefix arg refresh the database in each project.

(fn UPDATE)" t nil) (autoload 'helm-locate "helm-locate" "Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it if
it doesn't exists.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-locate" '("helm-"))) (autoload 'helm-man-woman "helm-man" "Preconfigured `helm' for Man and Woman pages.
With a prefix arg reinitialize the cache.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-man" '("helm-"))) (autoload 'helm-world-time "helm-misc" "Preconfigured `helm' to show world time.
Default action change TZ environment variable locally to emacs." t nil) (autoload 'helm-insert-latex-math "helm-misc" "Preconfigured helm for latex math symbols completion." t nil) (autoload 'helm-ratpoison-commands "helm-misc" "Preconfigured `helm' to execute ratpoison commands." t nil) (autoload 'helm-stumpwm-commands "helm-misc" "Preconfigured helm for stumpwm commands." t nil) (autoload 'helm-minibuffer-history "helm-misc" "Preconfigured `helm' for `minibuffer-history'." t nil) (defvar helm-epa-mode nil "Non-nil if Helm-Epa mode is enabled.
See the `helm-epa-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-epa-mode'.") (custom-autoload 'helm-epa-mode "helm-misc" nil) (autoload 'helm-epa-mode "helm-misc" "Enable helm completion on gpg keys in epa functions.

If called interactively, enable Helm-Epa mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'helm-epa-list-keys "helm-misc" "List all gpg keys.
This is the helm interface for `epa-list-keys'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-misc" '("helm-"))) (autoload 'helm-comp-read "helm-mode" "Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, vector, obarray or hash-table.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- FUZZY: Enable fuzzy matching.

- REVERSE-HISTORY: When non--nil display history source after current
  source completion.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A list containing specific history, default is nil.
  When it is non--nil, all elements of HISTORY are displayed in
  a special source before COLLECTION.

- INPUT-HISTORY: A symbol. The minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.

- CASE-FOLD: Same as `helm-case-fold-search'.

- DEL-INPUT: Boolean, when non--nil (default) remove the partial
  minibuffer input from HISTORY is present.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  Default is `helm-comp-read-mode-line'.

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (the keymap will be shared with history source)

- NAME: The name related to this local source.

- HEADER-NAME: A function to alter NAME, see `helm'.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute.

- SORT: A predicate to give to `sort' e.g `string-lessp'
  Use this only on small data as it is inefficient.
  If you want to sort faster add a sort function to
  FC-TRANSFORMER.
  Note that FUZZY when enabled is already providing a sort function.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function
  or a list of functions.

- HIST-FC-TRANSFORMER: A `filtered-candidate-transformer'
  function for the history source.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: (default is non--nil) See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-source-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

- MATCH-PART: Allow matching only one part of candidate.
  See match-part documentation in `helm-source'.

- MATCH-DYNAMIC: See match-dynamic in `helm-source-sync'
  It has no effect when used with CANDIDATES-IN-BUFFER.

- ALLOW-NEST: Allow nesting this `helm-comp-read' in a helm session.
  See `helm'.

- MULTILINE: See multiline in `helm-source'.

- COERCE: See coerce in `helm-source'.

- GROUP: See group in `helm-source'.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That means you can pass prefix args before or after calling a command
that use `helm-comp-read'.  See `helm-M-x' for example.

(fn PROMPT COLLECTION &key TEST INITIAL-INPUT DEFAULT PRESELECT (BUFFER \"*Helm Completions*\") MUST-MATCH FUZZY REVERSE-HISTORY (REQUIRES-PATTERN 0) HISTORY INPUT-HISTORY (CASE-FOLD helm-comp-read-case-fold-search) (DEL-INPUT t) (PERSISTENT-ACTION nil) (PERSISTENT-HELP \"DoNothing\") (MODE-LINE helm-comp-read-mode-line) HELP-MESSAGE (KEYMAP helm-comp-read-map) (NAME \"Helm Completions\") HEADER-NAME CANDIDATES-IN-BUFFER MATCH-PART MATCH-DYNAMIC EXEC-WHEN-ONLY-ONE QUIT-WHEN-NO-CAND (VOLATILE t) SORT FC-TRANSFORMER HIST-FC-TRANSFORMER (MARKED-CANDIDATES helm-comp-read-use-marked) NOMARK (ALISTP t) (CANDIDATE-NUMBER-LIMIT helm-candidate-number-limit) MULTILINE ALLOW-NEST COERCE (GROUP \\='helm))" nil nil) (autoload 'helm-read-file-name "helm-mode" "Read a file name with helm completion.
It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start reading file name, default to `default-directory'.

- BUFFER: `helm-buffer' name, defaults to \"*Helm Completions*\".

- TEST: A predicate called with one arg 'candidate'.

- NORET: Allow disabling helm-ff-RET (have no effect if helm-ff-RET
                                      isn't bound to RET).

- CASE-FOLD: Same as `helm-case-fold-search'.

- PRESELECT: helm preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be 'confirm, nil, or t.

- FUZZY: Enable fuzzy matching when non-nil (Enabled by default).

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: Don't use `all-completions' in history (take effect only on history).

- PERSISTENT-ACTION-IF: a persistent if action function.

- PERSISTENT-HELP: persistent help message.

- MODE-LINE: A mode line message, default is `helm-read-file-name-mode-line-string'.

(fn PROMPT &key (NAME \"Read File Name\") (INITIAL-INPUT default-directory) (BUFFER \"*Helm file completions*\") TEST NORET (CASE-FOLD helm-file-name-case-fold-search) PRESELECT HISTORY MUST-MATCH (FUZZY t) DEFAULT MARKED-CANDIDATES (CANDIDATE-NUMBER-LIMIT helm-ff-candidate-number-limit) NOMARK (ALISTP t) (PERSISTENT-ACTION-IF \\='helm-find-files-persistent-action-if) (PERSISTENT-HELP \"Hit1 Expand Candidate, Hit2 or (C-u) Find file\") (MODE-LINE helm-read-file-name-mode-line-string))" nil nil) (defvar helm-mode nil "Non-nil if Helm mode is enabled.
See the `helm-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.") (custom-autoload 'helm-mode "helm-mode" nil) (autoload 'helm-mode "helm-mode" "Toggle generic helm completion.

All functions in Emacs that use `completing-read',
`read-file-name', `completion-in-region' and friends will use helm
interface when this mode is turned on.

However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can toggle it with M-x `helm-mode'.

About `ido-mode':
DO NOT enable `ido-everywhere' when using `helm-mode'.  Instead of
using `ido-mode', add the commands where you want to use ido to
`helm-completing-read-handlers-alist' with `ido' as value.

Note: This mode is incompatible with Emacs23.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-mode" '("helm-"))) (autoload 'helm-browse-url-firefox "helm-net" "Same as `browse-url-firefox' but detach from Emacs.

So when you quit Emacs you can keep your Firefox session open and
not be prompted to kill the Firefox process.

NOTE: Probably not supported on some systems (e.g., Windows).

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-opera "helm-net" "Browse URL with Opera browser and detach from Emacs.

So when you quit Emacs you can keep your Opera session open and
not be prompted to kill the Opera process.

NOTE: Probably not supported on some systems (e.g., Windows).

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-chromium "helm-net" "Browse URL with Google Chrome browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-uzbl "helm-net" "Browse URL with uzbl browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-conkeror "helm-net" "Browse URL with conkeror browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-next "helm-net" "Browse URL with next browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-surfraw "helm-net" "Preconfigured `helm' to search PATTERN with search ENGINE.

(fn PATTERN ENGINE)" t nil) (autoload 'helm-google-suggest "helm-net" "Preconfigured `helm' for Google search with Google suggest." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-net" '("helm-"))) (autoload 'helm-occur "helm-occur" "Preconfigured helm for searching lines matching pattern in `current-buffer'.

When `helm-source-occur' is member of
`helm-sources-using-default-as-input' which is the default,
symbol at point is searched at startup.

When a region is marked search only in this region by narrowing.

To search in multiples buffers start from one of the commands listing
buffers (i.e. a helm command using `helm-source-buffers-list' like
`helm-mini') and use the multi occur buffers action.

This is the helm implementation that collect lines matching pattern
like vanilla Emacs `occur' but have nothing to do with it, the search
engine beeing completely different and also much faster." t nil) (autoload 'helm-occur-visible-buffers "helm-occur" "Run helm-occur on all visible buffers in frame." t nil) (autoload 'helm-occur-from-isearch "helm-occur" "Invoke `helm-occur' from isearch.

To use this bind it to a key in `isearch-mode-map'." t nil) (autoload 'helm-multi-occur-from-isearch "helm-occur" "Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

To use this bind it to a key in `isearch-mode-map'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-occur" '("helm-"))) (autoload 'helm-regexp "helm-regexp" "Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-regexp" '("helm-"))) (autoload 'helm-mark-ring "helm-ring" "Preconfigured `helm' for `helm-source-mark-ring'." t nil) (autoload 'helm-global-mark-ring "helm-ring" "Preconfigured `helm' for `helm-source-global-mark-ring'." t nil) (autoload 'helm-all-mark-rings "helm-ring" "Preconfigured `helm' for `helm-source-global-mark-ring' and `helm-source-mark-ring'." t nil) (autoload 'helm-register "helm-ring" "Preconfigured `helm' for Emacs registers." t nil) (autoload 'helm-show-kill-ring "helm-ring" "Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line." t nil) (autoload 'helm-execute-kmacro "helm-ring" "Preconfigured helm for keyboard macros.
Define your macros with `f3' and `f4'.
See (info \"(emacs) Keyboard Macros\") for detailed infos.
This command is useful when used with persistent action." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-ring" '("helm-"))) (autoload 'helm-semantic "helm-semantic" "Preconfigured `helm' for `semantic'.
If ARG is supplied, pre-select symbol at point instead of current.

(fn ARG)" t nil) (autoload 'helm-semantic-or-imenu "helm-semantic" "Preconfigured helm for `semantic' or `imenu'.
If ARG is supplied, pre-select symbol at point instead of current
semantic tag in scope.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-semantic" '("helm-s"))) (defalias 'helm-shell-prompts 'helm-comint-prompts) (defalias 'helm-shell-prompts-all 'helm-comint-prompts-all) (defvar helm-top-poll-mode nil "Non-nil if Helm-Top-Poll mode is enabled.
See the `helm-top-poll-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-top-poll-mode'.") (custom-autoload 'helm-top-poll-mode "helm-sys" nil) (autoload 'helm-top-poll-mode "helm-sys" "Refresh automatically helm top buffer once enabled.

If called interactively, enable Helm-Top-Poll mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'helm-top "helm-sys" "Preconfigured `helm' for top command." t nil) (autoload 'helm-list-emacs-process "helm-sys" "Preconfigured `helm' for Emacs process." t nil) (autoload 'helm-xrandr-set "helm-sys" "Preconfigured helm for xrandr." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-sys" '("helm-"))) (autoload 'helm-etags-select "helm-tags" "Preconfigured helm for etags.
If called with a prefix argument REINIT
or if any of the tag files have been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories,
     by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command.

(fn REINIT)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-tags" '("helm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-types" '("helm-"))) (defvar helm-popup-tip-mode nil "Non-nil if Helm-Popup-Tip mode is enabled.
See the `helm-popup-tip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-popup-tip-mode'.") (custom-autoload 'helm-popup-tip-mode "helm-utils" nil) (autoload 'helm-popup-tip-mode "helm-utils" "Show help-echo informations in a popup tip at end of line.

If called interactively, enable Helm-Popup-Tip mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-utils" '("helm-" "with-helm-display-marked-candidates"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-x-files" '("helm-"))) (provide 'helm-autoloads)) "helm-descbinds" ((helm-descbinds-autoloads helm-descbinds) (defvar helm-descbinds-mode nil "Non-nil if Helm-Descbinds mode is enabled.
See the `helm-descbinds-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-descbinds-mode'.") (custom-autoload 'helm-descbinds-mode "helm-descbinds" nil) (autoload 'helm-descbinds-mode "helm-descbinds" "Use `helm' for `describe-bindings'.

If called interactively, enable Helm-Descbinds mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'helm-descbinds-install "helm-descbinds" "Use `helm-descbinds' as a replacement of `describe-bindings'." t nil) (autoload 'helm-descbinds-uninstall "helm-descbinds" "Restore original `describe-bindings'." t nil) (autoload 'helm-descbinds "helm-descbinds" "A convenient helm version of `describe-bindings'.

Turning on `helm-descbinds-mode' is the recommended way to
install this command to replace `describe-bindings'.

You complete against a list of keys + command pairs presented in
a similar way as `describe-bindings' does, split into sections
defined by the types of the key bindings (minor and major modes,
global bindings, etc).

The default action executes a command as if the binding had been
entered, or narrows the commands according to a prefix key,
respectively.

The persistent action pops up a help buffer for the selected
command without quitting.

For key translation maps, the default actions are not very
useful, yet they are listed for completeness.

(fn &optional PREFIX BUFFER)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-descbinds" '("helm-descbind"))) (provide 'helm-descbinds-autoloads)) "helm-swoop" ((helm-swoop-autoloads helm-swoop) (autoload 'helm-swoop-back-to-last-point "helm-swoop" "Go back to last position where `helm-swoop' was called.
If CANCEL is non-nil, store `helm-swoop-last-point'.

(fn &optional CANCEL)" t nil) (autoload 'helm-swoop "helm-swoop" "List the all lines to another buffer, which is able to squeeze by
 any words you input. At the same time, the original buffer's cursor
 is jumping line to line according to moving up and down the list.

(fn &key QUERY SOURCE (MULTILINE current-prefix-arg))" t nil) (autoload 'helm-swoop-from-isearch "helm-swoop" "Invoke `helm-swoop' from isearch." t nil) (autoload 'helm-multi-swoop "helm-swoop" "Multi swoop for QUERY in BUFLIST.

Usage:
  \\[execute-extended-command] helm-multi-swoop
  1. Select any buffers by [C-SPC] or [M-SPC]
  2. Press [RET] to start `helm-multi-swoop'

\\[universal-argument] \\[execute-extended-command] helm-multi-swoop
If you have done helm-multi-swoop before, you can skip select buffers step.
Last selected buffers will be applied to helm-multi-swoop.

(fn &optional QUERY BUFLIST)" t nil) (autoload 'helm-multi-swoop-all "helm-swoop" "Apply all buffers to helm-multi-swoop for QUERY.

(fn &optional QUERY)" t nil) (autoload 'helm-multi-swoop-org "helm-swoop" "Applie all `org-mode' buffers to helm-multi-swoop for QUERY.

(fn &optional QUERY)" t nil) (autoload 'helm-multi-swoop-current-mode "helm-swoop" "Applie all buffers of the same mode as the current buffer to helm-multi-swoop for QUERY.

(fn &optional QUERY)" t nil) (autoload 'helm-multi-swoop-projectile "helm-swoop" "Apply all opened buffers of the current project to helm-multi-swoop for QUERY.

(fn &optional QUERY)" t nil) (autoload 'helm-swoop-without-pre-input "helm-swoop" "Start helm-swoop without pre input query." t nil) (autoload 'helm-swoop-symble-pre-input "helm-swoop" "Start helm-swoop without pre input query." t nil) (autoload 'helm-multi-swoop-edit "helm-swoop" "Multi swoop edit." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-swoop" '("get-buffers-matching-mode" "helm-"))) (provide 'helm-swoop-autoloads)) "projectile" ((projectile-autoloads projectile) (autoload 'projectile-version "projectile" "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

(fn &optional SHOW-VERSION)" t nil) (autoload 'projectile-invalidate-cache "projectile" "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument PROMPT prompts for the name of the project whose cache
to invalidate.

(fn PROMPT)" t nil) (autoload 'projectile-purge-file-from-cache "projectile" "Purge FILE from the cache of the current project.

(fn FILE)" t nil) (autoload 'projectile-purge-dir-from-cache "projectile" "Purge DIR from the cache of the current project.

(fn DIR)" t nil) (autoload 'projectile-cache-current-file "projectile" "Add the currently visited file to the cache." t nil) (autoload 'projectile-discover-projects-in-directory "projectile" "Discover any projects in DIRECTORY and add them to the projectile cache.
This function is not recursive and only adds projects with roots
at the top level of DIRECTORY.

(fn DIRECTORY)" t nil) (autoload 'projectile-discover-projects-in-search-path "projectile" "Discover projects in `projectile-project-search-path'.
Invoked automatically when `projectile-mode' is enabled." t nil) (autoload 'projectile-switch-to-buffer "projectile" "Switch to a project buffer." t nil) (autoload 'projectile-switch-to-buffer-other-window "projectile" "Switch to a project buffer and show it in another window." t nil) (autoload 'projectile-switch-to-buffer-other-frame "projectile" "Switch to a project buffer and show it in another frame." t nil) (autoload 'projectile-display-buffer "projectile" "Display a project buffer in another window without selecting it." t nil) (autoload 'projectile-project-buffers-other-buffer "projectile" "Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned." t nil) (autoload 'projectile-multi-occur "projectile" "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context.

(fn &optional NLINES)" t nil) (autoload 'projectile-find-other-file "projectile" "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-other-file-other-window "projectile" "Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-other-file-other-frame "projectile" "Switch between files with the same name but different extensions in other frame.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-file-dwim "projectile" "Jump to a project's files using completion based on context.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\" immediately
 because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename like
 \"projectile/a\", a list of files with character 'a' in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-dwim-other-window "projectile" "Jump to a project's files using completion based on context in other window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-window' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-dwim-other-frame "projectile" "Jump to a project's files using completion based on context in other frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-frame' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file "projectile" "Jump to a project's file using completion.
With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-other-window "projectile" "Jump to a project's file using completion and show it in another window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-other-frame "projectile" "Jump to a project's file using completion and show it in another frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-toggle-project-read-only "projectile" "Toggle project read only." t nil) (autoload 'projectile-find-dir "projectile" "Jump to a project's directory using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-dir-other-window "projectile" "Jump to a project's directory in other window using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-dir-other-frame "projectile" "Jump to a project's directory in other frame using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-test-file "projectile" "Jump to a project's test file using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-related-file-other-window "projectile" "Open related file in other window." t nil) (autoload 'projectile-find-related-file-other-frame "projectile" "Open related file in other frame." t nil) (autoload 'projectile-find-related-file "projectile" "Open related file." t nil) (autoload 'projectile-related-files-fn-groups "projectile" "Generate a related-files-fn which relates as KIND for files in each of GROUPS.

(fn KIND GROUPS)" nil nil) (autoload 'projectile-related-files-fn-extensions "projectile" "Generate a related-files-fn which relates as KIND for files having EXTENSIONS.

(fn KIND EXTENSIONS)" nil nil) (autoload 'projectile-related-files-fn-test-with-prefix "projectile" "Generate a related-files-fn which relates tests and impl for files with EXTENSION based on TEST-PREFIX.

(fn EXTENSION TEST-PREFIX)" nil nil) (autoload 'projectile-related-files-fn-test-with-suffix "projectile" "Generate a related-files-fn which relates tests and impl for files with EXTENSION based on TEST-SUFFIX.

(fn EXTENSION TEST-SUFFIX)" nil nil) (autoload 'projectile-project-info "projectile" "Display info for current project." t nil) (autoload 'projectile-find-implementation-or-test-other-window "projectile" "Open matching implementation or test file in other window." t nil) (autoload 'projectile-find-implementation-or-test-other-frame "projectile" "Open matching implementation or test file in other frame." t nil) (autoload 'projectile-toggle-between-implementation-and-test "projectile" "Toggle between an implementation file and its test file." t nil) (autoload 'projectile-grep "projectile" "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp.

(fn &optional REGEXP ARG)" t nil) (autoload 'projectile-ag "projectile" "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

(fn SEARCH-TERM &optional ARG)" t nil) (autoload 'projectile-ripgrep "projectile" "Run a Ripgrep search with `SEARCH-TERM' at current project root.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

(fn SEARCH-TERM &optional ARG)" t nil) (autoload 'projectile-regenerate-tags "projectile" "Regenerate the project's [e|g]tags." t nil) (autoload 'projectile-find-tag "projectile" "Find tag in project." t nil) (autoload 'projectile-run-command-in-root "projectile" "Invoke `execute-extended-command' in the project's root." t nil) (autoload 'projectile-run-shell-command-in-root "projectile" "Invoke `shell-command' in the project's root." t nil) (autoload 'projectile-run-async-shell-command-in-root "projectile" "Invoke `async-shell-command' in the project's root." t nil) (autoload 'projectile-run-gdb "projectile" "Invoke `gdb' in the project's root." t nil) (autoload 'projectile-run-shell "projectile" "Invoke `shell' in the project's root.

Switch to the project specific shell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn ARG)" t nil) (autoload 'projectile-run-eshell "projectile" "Invoke `eshell' in the project's root.

Switch to the project specific eshell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn ARG)" t nil) (autoload 'projectile-run-ielm "projectile" "Invoke `ielm' in the project's root.

Switch to the project specific ielm buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn ARG)" t nil) (autoload 'projectile-run-term "projectile" "Invoke `term' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn ARG)" t nil) (autoload 'projectile-run-vterm "projectile" "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-replace "projectile" "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

(fn &optional ARG)" t nil) (autoload 'projectile-replace-regexp "projectile" "Replace a regexp in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

(fn &optional ARG)" t nil) (autoload 'projectile-kill-buffers "projectile" "Kill project buffers.

The buffer are killed according to the value of
`projectile-kill-buffers-filter'." t nil) (autoload 'projectile-save-project-buffers "projectile" "Save all project buffers." t nil) (autoload 'projectile-dired "projectile" "Open `dired' at the root of the project." t nil) (autoload 'projectile-dired-other-window "projectile" "Open `dired'  at the root of the project in another window." t nil) (autoload 'projectile-dired-other-frame "projectile" "Open `dired' at the root of the project in another frame." t nil) (autoload 'projectile-vc "projectile" "Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available.

If PROJECT-ROOT is given, it is opened instead of the project
root directory of the current buffer file.  If interactively
called with a prefix argument, the user is prompted for a project
directory to open.

(fn &optional PROJECT-ROOT)" t nil) (autoload 'projectile-recentf "projectile" "Show a list of recently visited files in a project." t nil) (autoload 'projectile-configure-project "projectile" "Run project configure command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-compile-project "projectile" "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-test-project "projectile" "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-install-project "projectile" "Run project install command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-package-project "projectile" "Run project package command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-run-project "projectile" "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-repeat-last-command "projectile" "Run last projectile external command.

External commands are: `projectile-configure-project',
`projectile-compile-project', `projectile-test-project',
`projectile-install-project', `projectile-package-project',
and `projectile-run-project'.

If the prefix argument SHOW_PROMPT is non nil, the command can be edited.

(fn SHOW-PROMPT)" t nil) (autoload 'projectile-switch-project "projectile" "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t nil) (autoload 'projectile-switch-open-project "projectile" "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t nil) (autoload 'projectile-find-file-in-directory "projectile" "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in.

(fn &optional DIRECTORY)" t nil) (autoload 'projectile-find-file-in-known-projects "projectile" "Jump to a file in any of the known projects." t nil) (autoload 'projectile-cleanup-known-projects "projectile" "Remove known projects that don't exist anymore." t nil) (autoload 'projectile-clear-known-projects "projectile" "Clear both `projectile-known-projects' and `projectile-known-projects-file'." t nil) (autoload 'projectile-remove-known-project "projectile" "Remove PROJECT from the list of known projects.

(fn &optional PROJECT)" t nil) (autoload 'projectile-remove-current-project-from-known-projects "projectile" "Remove the current project from the list of known projects." t nil) (autoload 'projectile-add-known-project "projectile" "Add PROJECT-ROOT to the list of known projects.

(fn PROJECT-ROOT)" t nil) (autoload 'projectile-ibuffer "projectile" "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied.

(fn PROMPT-FOR-PROJECT)" t nil) (autoload 'projectile-commander "projectile" "Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods." t nil) (autoload 'projectile-browse-dirty-projects "projectile" "Browse dirty version controlled projects.

With a prefix argument, or if CACHED is non-nil, try to use the cached
dirty project list.

(fn &optional CACHED)" t nil) (autoload 'projectile-edit-dir-locals "projectile" "Edit or create a .dir-locals.el file of the project." t nil) (defvar projectile-mode nil "Non-nil if Projectile mode is enabled.
See the `projectile-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-mode'.") (custom-autoload 'projectile-mode "projectile" nil) (autoload 'projectile-mode "projectile" "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'projectile-global-mode 'projectile-mode "1.0") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "projectile" '("??" "compilation-find-file-projectile-find-compilation-buffer" "def-projectile-commander-method" "delete-file-projectile-remove-from-cache" "projectile-"))) (provide 'projectile-autoloads)) "helm-projectile" ((helm-projectile-autoloads helm-projectile) (defvar helm-projectile-fuzzy-match t "Enable fuzzy matching for Helm Projectile commands.
This needs to be set before loading helm-projectile.el.") (custom-autoload 'helm-projectile-fuzzy-match "helm-projectile" t) (autoload 'helm-projectile-find-file-dwim "helm-projectile" "Find file at point based on context." t nil) (autoload 'helm-projectile-find-other-file "helm-projectile" "Switch between files with the same name but different extensions using Helm.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'helm-projectile-on "helm-projectile" "Turn on `helm-projectile' key bindings." t nil) (autoload 'helm-projectile-off "helm-projectile" "Turn off `helm-projectile' key bindings." t nil) (autoload 'helm-projectile-grep "helm-projectile" "Helm version of `projectile-grep'.
DIR is the project root, if not set then current directory is used

(fn &optional DIR)" t nil) (autoload 'helm-projectile-ack "helm-projectile" "Helm version of projectile-ack.

(fn &optional DIR)" t nil) (autoload 'helm-projectile-ag "helm-projectile" "Helm version of `projectile-ag'.

(fn &optional OPTIONS)" t nil) (autoload 'helm-projectile-rg "helm-projectile" "Projectile version of `helm-rg'." t nil) (autoload 'helm-projectile-toggle "helm-projectile" "Toggle Helm version of Projectile commands.

(fn TOGGLE)" nil nil) (autoload 'helm-projectile "helm-projectile" "Use projectile with Helm instead of ido.

With a prefix ARG invalidates the cache first.
If invoked outside of a project, displays a list of known projects to jump.

(fn &optional ARG)" t nil) (eval-after-load 'projectile '(progn (define-key projectile-command-map (kbd "h") #'helm-projectile))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-projectile" '("helm-"))) (provide 'helm-projectile-autoloads)) "helm-rg" ((helm-rg helm-rg-autoloads) (autoload 'helm-rg "helm-rg" "Search for the PCRE regexp RG-PATTERN extremely quickly with ripgrep.

When invoked interactively with a prefix argument, or when PFX is non-nil,
set the cwd for the ripgrep process to `default-directory'. Otherwise use the
cwd as described by `helm-rg-default-directory'.

If PATHS is non-nil, ripgrep will search only those paths, relative to the
process's cwd. Otherwise, the process's cwd will be searched.

Note that ripgrep respects glob patterns from .gitignore, .rgignore, and .ignore
files, excluding files matching those patterns. This composes with the glob
defined by `helm-rg-default-glob-string', which only finds files matching the
glob, and can be overridden with `helm-rg--set-glob', which is defined in
`helm-rg-map'.

There are many more `defcustom' forms, which are visible by searching for \"defcustom\" in the
`helm-rg' source (which can be located using `find-function'). These `defcustom' forms set defaults
for options which can be modified while invoking `helm-rg' using the keybindings listed below.

The ripgrep command's help output can be printed into its own buffer for
reference with the interactive command `helm-rg-display-help'.

\\{helm-rg-map}

(fn RG-PATTERN &optional PFX PATHS)" t nil) (autoload 'helm-rg-display-help "helm-rg" "Display a buffer with the ripgrep command's usage help.

The help buffer will be reused if it was already created. A prefix argument when
invoked interactively, or a non-nil value for PFX, will display the help buffer
in the current window. Otherwise, if the help buffer is already being displayed
in some window, select that window, or else display the help buffer with
`pop-to-buffer'.

(fn &optional PFX)" t nil) (autoload 'helm-rg-from-isearch "helm-rg" "Invoke `helm-rg' from isearch." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-rg" '("helm-"))) (provide 'helm-rg-autoloads)) "helm-xref" ((helm-xref-autoloads helm-xref) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-xref" '("helm-xref-"))) (provide 'helm-xref-autoloads)) "helm-ls-git" ((helm-ls-git-autoloads helm-ls-git) (autoload 'helm-ls-git-ls "helm-ls-git" "

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-ls-git" '("helm-"))) (provide 'helm-ls-git-autoloads)) "ivy" ((ivy-autoloads ivy-faces colir ivy-avy ivy ivy-overlay elpa) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "colir" '("colir-"))) (autoload 'ivy-resume "ivy" "Resume the last completion session, or SESSION if non-nil.
With a prefix arg, try to restore a recorded completion session,
if one exists.

(fn &optional SESSION)" t nil) (autoload 'ivy-read "ivy" "Read a string in the minibuffer, with completion.

PROMPT is a string, normally ending in a colon and a space.
`ivy-count-format' is prepended to PROMPT during completion.

COLLECTION is either a list of strings, a function, an alist, or
a hash table, supplied for `minibuffer-completion-table'.

PREDICATE is applied to filter out the COLLECTION immediately.
This argument is for compatibility with `completing-read'.

When REQUIRE-MATCH is non-nil, only members of COLLECTION can be
selected.

If INITIAL-INPUT is non-nil, then insert that input in the
minibuffer initially.

HISTORY is a name of a variable to hold the completion session
history.

KEYMAP is composed with `ivy-minibuffer-map'.

PRESELECT, when non-nil, determines which one of the candidates
matching INITIAL-INPUT to select initially.  An integer stands
for the position of the desired candidate in the collection,
counting from zero.  Otherwise, use the first occurrence of
PRESELECT in the collection.  Comparison is first done with
`equal'.  If that fails, and when applicable, match PRESELECT as
a regular expression.

DEF is for compatibility with `completing-read'.

UPDATE-FN is called each time the candidate list is re-displayed.

When SORT is non-nil, `ivy-sort-functions-alist' determines how
to sort candidates before displaying them.

ACTION is a function to call after selecting a candidate.
It takes one argument, the selected candidate. If COLLECTION is
an alist, the argument is a cons cell, otherwise it's a string.

MULTI-ACTION, when non-nil, is called instead of ACTION when
there are marked candidates. It takes the list of candidates as
its only argument. When it's nil, ACTION is called on each marked
candidate.

UNWIND is a function of no arguments to call before exiting.

RE-BUILDER is a function transforming input text into a regex
pattern.

MATCHER is a function which can override how candidates are
filtered based on user input.  It takes a regex pattern and a
list of candidates, and returns the list of matching candidates.

DYNAMIC-COLLECTION is a boolean specifying whether the list of
candidates is updated after each input by calling COLLECTION.

EXTRA-PROPS is a plist that can be used to store
collection-specific session-specific data.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session.

(fn PROMPT COLLECTION &key PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY PRESELECT DEF KEYMAP UPDATE-FN SORT ACTION MULTI-ACTION UNWIND RE-BUILDER MATCHER DYNAMIC-COLLECTION EXTRA-PROPS CALLER)" nil nil) (autoload 'ivy-completing-read "ivy" "Read a string in the minibuffer, with completion.

This interface conforms to `completing-read' and can be used for
`completing-read-function'.

PROMPT is a string that normally ends in a colon and a space.
COLLECTION is either a list of strings, an alist, an obarray, or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is a boolean value or a symbol.  See `completing-read'.
INITIAL-INPUT is a string inserted into the minibuffer initially.
HISTORY is a list of previously selected inputs.
DEF is the default value.
INHERIT-INPUT-METHOD is currently ignored.

(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY DEF INHERIT-INPUT-METHOD)" nil nil) (defvar ivy-mode nil "Non-nil if Ivy mode is enabled.
See the `ivy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-mode'.") (custom-autoload 'ivy-mode "ivy" nil) (autoload 'ivy-mode "ivy" "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}

(fn &optional ARG)" t nil) (autoload 'ivy-switch-buffer "ivy" "Switch to another buffer." t nil) (autoload 'ivy-switch-view "ivy" "Switch to one of the window views stored by `ivy-push-view'." t nil) (autoload 'ivy-switch-buffer-other-window "ivy" "Switch to another buffer in another window." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy" '("ivy-" "with-ivy-window"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-avy" '("ivy-avy"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-overlay" '("ivy-"))) (provide 'ivy-autoloads)) "swiper" ((swiper-autoloads swiper) (autoload 'swiper-avy "swiper" "Jump to one of the current swiper candidates." t nil) (autoload 'swiper-backward "swiper" "`isearch-backward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-thing-at-point "swiper" "`swiper' with `ivy-thing-at-point'." t nil) (autoload 'swiper-all-thing-at-point "swiper" "`swiper-all' with `ivy-thing-at-point'." t nil) (autoload 'swiper "swiper" "`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-all "swiper" "Run `swiper' for all open buffers.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-isearch "swiper" "A `swiper' that's not line-based.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-isearch-backward "swiper" "Like `swiper-isearch' but the first result is before the point.

(fn &optional INITIAL-INPUT)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swiper" '("swiper-"))) (provide 'swiper-autoloads)) "counsel" ((counsel counsel-autoloads) (autoload 'counsel-company "counsel" "Complete using `company-candidates'." t nil) (autoload 'counsel-irony "counsel" "Inline C/C++ completion using Irony." t nil) (autoload 'counsel-describe-variable "counsel" "Forward to `describe-variable'.

Variables declared using `defcustom' are highlighted according to
`ivy-highlight-face'." t nil) (autoload 'counsel-describe-function "counsel" "Forward to `describe-function'.

Interactive functions (i.e., commands) are highlighted according
to `ivy-highlight-face'." t nil) (autoload 'counsel-describe-symbol "counsel" "Forward to `describe-symbol'." t nil) (autoload 'counsel-set-variable "counsel" "Set a variable SYM, with completion.

When the selected variable is a `defcustom' with the type boolean
or radio, offer completion of all possible values.

Otherwise, offer a variant of `eval-expression', with the initial
input corresponding to the chosen variable.

With a prefix arg, restrict list to variables defined using
`defcustom'.

(fn SYM)" t nil) (autoload 'counsel-apropos "counsel" "Show all matching symbols.
See `apropos' for further information on what is considered
a symbol and how to search for them." t nil) (autoload 'counsel-info-lookup-symbol "counsel" "Forward SYMBOL to `info-lookup-symbol' with ivy completion.
With prefix arg MODE a query for the symbol help mode is offered.

(fn SYMBOL &optional MODE)" t nil) (autoload 'counsel-M-x "counsel" "Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.
This function integrates with either the `amx' or `smex' package
when available, in that order of precedence.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-command-history "counsel" "Show the history of commands." t nil) (autoload 'counsel-load-library "counsel" "Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'." t nil) (autoload 'counsel-find-library "counsel" "Visit a selected the Emacs Lisp library.
The libraries are offered from `load-path'." t nil) (autoload 'counsel-load-theme "counsel" "Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'." t nil) (autoload 'counsel-descbinds "counsel" "Show a list of all defined keys and their definitions.
If non-nil, show only bindings that start with PREFIX.
BUFFER defaults to the current one.

(fn &optional PREFIX BUFFER)" t nil) (autoload 'counsel-describe-face "counsel" "Completion for `describe-face'." t nil) (autoload 'counsel-faces "counsel" "Complete faces with preview.
Actions are provided by default for describing or customizing the
selected face." t nil) (autoload 'counsel-git "counsel" "Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-git-grep "counsel" "Grep for a string in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY CMD)" t nil) (autoload 'counsel-git-stash "counsel" "Search through all available git stashes." t nil) (autoload 'counsel-git-change-worktree "counsel" "Find the file corresponding to the current buffer on a different worktree." t nil) (autoload 'counsel-git-checkout "counsel" "Call the \"git checkout\" command." t nil) (autoload 'counsel-git-log "counsel" "Call the \"git log --grep\" shell command." t nil) (autoload 'counsel-find-file "counsel" "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-dired "counsel" "Forward to `dired'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-recentf "counsel" "Find a file on `recentf-list'." t nil) (autoload 'counsel-buffer-or-recentf "counsel" "Find a buffer visiting a file or file on `recentf-list'." t nil) (autoload 'counsel-bookmark "counsel" "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist." t nil) (autoload 'counsel-bookmarked-directory "counsel" "Ivy interface for bookmarked directories.

With a prefix argument, this command creates a new bookmark which points to the
current value of `default-directory'." t nil) (autoload 'counsel-file-register "counsel" "Search file in register.

You cannot use Emacs' normal register commands to create file
registers.  Instead you must use the `set-register' function like
so: `(set-register ?i \"/home/eric/.emacs.d/init.el\")'.  Now you
can use `C-x r j i' to open that file." t nil) (autoload 'counsel-locate-action-extern "counsel" "Pass X to `xdg-open' or equivalent command via the shell.

(fn X)" t nil) (autoload 'counsel-locate "counsel" "Call the \"locate\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-tracker "counsel" nil t nil) (autoload 'counsel-fzf "counsel" "Open a file using the fzf shell command.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
FZF-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY FZF-PROMPT)" t nil) (autoload 'counsel-dpkg "counsel" "Call the \"dpkg\" shell command." t nil) (autoload 'counsel-rpm "counsel" "Call the \"rpm\" shell command." t nil) (autoload 'counsel-file-jump "counsel" "Jump to a file below the current directory.
List all files within the current directory or any of its sub-directories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil) (autoload 'counsel-dired-jump "counsel" "Jump to a directory (see `dired-jump') below the current directory.
List all sub-directories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil) (autoload 'counsel-ag "counsel" "Grep for a string in a root directory using ag.

By default, the root directory is the first directory containing a .git subdirectory.

INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
CALLER is passed to `ivy-read'.

With a `\\[universal-argument]' prefix argument, prompt for INITIAL-DIRECTORY.
With a `\\[universal-argument] \\[universal-argument]' prefix argument, prompt additionally for EXTRA-AG-ARGS.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-AG-ARGS AG-PROMPT &key CALLER)" t nil) (autoload 'counsel-pt "counsel" "Grep for a string in the current directory using pt.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-pt-base-command' instead of
`counsel-ag-base-command'.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-ack "counsel" "Grep for a string in the current directory using ack.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-ack-base-command' replacing
`counsel-ag-base-command'.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-rg "counsel" "Grep for a string in the current directory using rg.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

Example input with inclusion and exclusion file patterns:
    require i -- -g*.el

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-RG-ARGS RG-PROMPT)" t nil) (autoload 'counsel-grep "counsel" "Grep for a string in the file visited by the current buffer.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-grep-backward "counsel" "Grep for a string in the file visited by the current buffer going
backward similar to `swiper-backward'. When non-nil, INITIAL-INPUT is
the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-grep-or-swiper "counsel" "Call `swiper' for small buffers and `counsel-grep' for large ones.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-grep-or-swiper-backward "counsel" "Call `swiper-backward' for small buffers and `counsel-grep-backward' for
large ones.  When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-recoll "counsel" "Search for a string in the recoll database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel--org-get-tags "counsel" nil nil nil) (autoload 'counsel-org-tag "counsel" "Add or remove tags in `org-mode'." t nil) (autoload 'counsel-org-tag-agenda "counsel" "Set tags for the current agenda item." t nil) (defalias 'counsel-org-goto #'counsel-outline) (autoload 'counsel-org-goto-all "counsel" "Go to a different location in any org file." t nil) (autoload 'counsel-org-file "counsel" "Browse all attachments for current Org file." t nil) (autoload 'counsel-org-entity "counsel" "Complete Org entities using Ivy." t nil) (autoload 'counsel-org-capture "counsel" "Capture something." t nil) (autoload 'counsel-org-agenda-headlines "counsel" "Choose from headers of `org-mode' files in the agenda." t nil) (autoload 'counsel-org-link "counsel" "Insert a link to an headline with completion." t nil) (autoload 'counsel-mark-ring "counsel" "Browse `mark-ring' interactively.
Obeys `widen-automatically', which see." t nil) (autoload 'counsel-evil-marks "counsel" "Ivy replacement for `evil-show-marks'.
By default, this function respects `counsel-evil-marks-exclude-registers'.
When ARG is non-nil, display all active evil registers.

(fn &optional ARG)" t nil) (autoload 'counsel-package "counsel" "Install or delete packages.

Packages not currently installed are prefixed with \"+\", and
selecting one of these will try to install it.
Packages currently installed are prefixed with \"-\", and
selecting one of these will try to delete it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Describe package
  \\[ivy-dispatching-done] h: Visit package's homepage" t nil) (autoload 'counsel-tmm "counsel" "Text-mode emulation of looking and choosing from a menu bar." t nil) (autoload 'counsel-yank-pop "counsel" "Ivy replacement for `yank-pop'.
With a plain prefix argument (\\[universal-argument]),
temporarily toggle the value of `counsel-yank-pop-after-point'.
Any other value of ARG has the same meaning as in `yank-pop', but
`counsel-yank-pop-preselect-last' determines its default value.
See also `counsel-yank-pop-filter' for how to filter candidates.

Note: Duplicate elements of `kill-ring' are always deleted.

(fn &optional ARG)" t nil) (autoload 'counsel-register "counsel" "Interactively choose a register." t nil) (autoload 'counsel-evil-registers "counsel" "Ivy replacement for `evil-show-registers'." t nil) (autoload 'counsel-imenu "counsel" "Jump to a buffer position indexed by imenu." t nil) (autoload 'counsel-list-processes "counsel" "Offer completion for `process-list'.
The default action deletes the selected process.
An extra action allows to switch to the process buffer." t nil) (autoload 'counsel-minibuffer-history "counsel" "Browse minibuffer history." t nil) (autoload 'counsel-esh-history "counsel" "Browse Eshell history." t nil) (autoload 'counsel-shell-history "counsel" "Browse shell history." t nil) (autoload 'counsel-slime-repl-history "counsel" "Browse Slime REPL history." t nil) (autoload 'counsel-hydra-heads "counsel" "Call a head of the current/last hydra." t nil) (autoload 'counsel-semantic "counsel" "Jump to a semantic tag in the current buffer." t nil) (autoload 'counsel-semantic-or-imenu "counsel" nil t nil) (autoload 'counsel-outline "counsel" "Jump to an outline heading with completion." t nil) (autoload 'counsel-ibuffer "counsel" "Use ibuffer to switch to another buffer.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\").

(fn &optional NAME)" t nil) (autoload 'counsel-switch-to-shell-buffer "counsel" "Switch to a shell buffer, or create one." t nil) (autoload 'counsel-unicode-char "counsel" "Insert COUNT copies of a Unicode character at point.
COUNT defaults to 1.

(fn &optional COUNT)" t nil) (autoload 'counsel-colors-emacs "counsel" "Show a list of all supported colors for a particular frame.

You can insert or kill the name or hexadecimal RGB value of the
selected color." t nil) (autoload 'counsel-colors-web "counsel" "Show a list of all W3C web colors for use in CSS.

You can insert or kill the name or hexadecimal RGB value of the
selected color." t nil) (autoload 'counsel-fonts "counsel" "Show a list of all supported font families for a particular frame.

You can insert or kill the name of the selected font." t nil) (autoload 'counsel-kmacro "counsel" "Interactively choose and run a keyboard macro.

With prefix argument, run macro that many times.

Macros are run using the current value of `kmacro-counter-value'
and their respective counter format. Displayed next to each macro is
the counter's format and initial value.

One can use actions to copy the counter format or initial counter
value of a macro, using them for a new macro." t nil) (autoload 'counsel-geiser-doc-look-up-manual "counsel" "Search Scheme documentation." t nil) (autoload 'counsel-rhythmbox "counsel" "Choose a song from the Rhythmbox library to play or enqueue.

(fn &optional ARG)" t nil) (autoload 'counsel-linux-app "counsel" "Launch a Linux desktop application, similar to Alt-<F2>.
When ARG is non-nil, ignore NoDisplay property in *.desktop files.

(fn &optional ARG)" t nil) (autoload 'counsel-wmctrl "counsel" "Select a desktop window using wmctrl." t nil) (autoload 'counsel-switch-buffer "counsel" "Switch to another buffer.
Display a preview of the selected ivy completion candidate buffer
in the current window." t nil) (autoload 'counsel-switch-buffer-other-window "counsel" "Switch to another buffer in another window.
Display a preview of the selected ivy completion candidate buffer
in the current window." t nil) (autoload 'counsel-compile "counsel" "Call `compile' completing with smart suggestions, optionally for DIR.

Additional actions:

\\{counsel-compile-map}

(fn &optional DIR)" t nil) (autoload 'counsel-compile-env "counsel" "Update `counsel-compile-env' interactively." t nil) (autoload 'counsel-minor "counsel" "Enable or disable minor mode.

Disabled minor modes are prefixed with \"+\", and
selecting one of these will enable it.
Enabled minor modes are prefixed with \"-\", and
selecting one of these will enable it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Go to minor mode definition
  \\[ivy-dispatching-done] h: Describe minor mode" t nil) (autoload 'counsel-major "counsel" nil t nil) (autoload 'counsel-compilation-errors "counsel" "Compilation errors." t nil) (autoload 'counsel-flycheck "counsel" "Flycheck errors." t nil) (defvar counsel-mode nil "Non-nil if Counsel mode is enabled.
See the `counsel-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `counsel-mode'.") (custom-autoload 'counsel-mode "counsel" nil) (autoload 'counsel-mode "counsel" "Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements.

Local bindings (`counsel-mode-map'):
\\{counsel-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "counsel" '("counsel-" "ivy-function-called-at-point" "tmm-km-list"))) (provide 'counsel-autoloads)) "smex" ((smex smex-autoloads) (autoload 'smex "smex" nil t nil) (autoload 'smex-major-mode-commands "smex" "Like `smex', but limited to commands that are relevant to the active major mode." t nil) (autoload 'smex-initialize "smex" nil t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smex" '("smex-"))) (provide 'smex-autoloads)) "lv" ((lv-autoloads lv) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lv" '("lv-"))) (provide 'lv-autoloads)) "hydra" ((hydra hydra-autoloads hydra-ox hydra-examples) (autoload 'defhydra "hydra" "Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit, :bind, and :column.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

:column is a string that sets the column for all subsequent heads.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t) (function-put 'defhydra 'lisp-indent-function 'defun) (function-put 'defhydra 'doc-string-elt '3) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra" '("defhydra" "hydra-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-examples" '("hydra-" "org-agenda-cts" "whitespace-mode"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-ox" '("hydra-ox"))) (provide 'hydra-autoloads)) "ht" ((ht-autoloads ht) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ht" 'nil)) (provide 'ht-autoloads)) "spinner" ((spinner-autoloads spinner) (autoload 'spinner-create "spinner" "Create a spinner of the given TYPE.
The possible TYPEs are described in `spinner--type-to-frames'.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If BUFFER-LOCAL is non-nil, the spinner will be automatically
deactivated if the buffer is killed.  If BUFFER-LOCAL is a
buffer, use that instead of current buffer.

When started, in order to function properly, the spinner runs a
timer which periodically calls `force-mode-line-update' in the
curent buffer.  If BUFFER-LOCAL was set at creation time, then
`force-mode-line-update' is called in that buffer instead.  When
the spinner is stopped, the timer is deactivated.

DELAY, if given, is the number of seconds to wait after starting
the spinner before actually displaying it. It is safe to cancel
the spinner before this time, in which case it won't display at
all.

(fn &optional TYPE BUFFER-LOCAL FPS DELAY)" nil nil) (autoload 'spinner-start "spinner" "Start a mode-line spinner of given TYPE-OR-OBJECT.
If TYPE-OR-OBJECT is an object created with `make-spinner',
simply activate it.  This method is designed for minor modes, so
they can use the spinner as part of their lighter by doing:
    \\='(:eval (spinner-print THE-SPINNER))
To stop this spinner, call `spinner-stop' on it.

If TYPE-OR-OBJECT is anything else, a buffer-local spinner is
created with this type, and it is displayed in the
`mode-line-process' of the buffer it was created it.  Both
TYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).
To stop this spinner, call `spinner-stop' in the same buffer.

Either way, the return value is a function which can be called
anywhere to stop this spinner.  You can also call `spinner-stop'
in the same buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

DELAY, if given, is the number of seconds to wait until actually
displaying the spinner. It is safe to cancel the spinner before
this time, in which case it won't display at all.

(fn &optional TYPE-OR-OBJECT FPS DELAY)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spinner" '("spinner"))) (provide 'spinner-autoloads)) "markdown-mode" ((markdown-mode-autoloads markdown-mode) (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files.

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)) (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files.

(fn)" t nil) (autoload 'markdown-view-mode "markdown-mode" "Major mode for viewing Markdown content.

(fn)" t nil) (autoload 'gfm-view-mode "markdown-mode" "Major mode for viewing GitHub Flavored Markdown content.

(fn)" t nil) (autoload 'markdown-live-preview-mode "markdown-mode" "Toggle native previewing on save for a specific markdown file.

If called interactively, enable Markdown-Live-Preview mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "markdown-mode" '("defun-markdown-" "gfm-" "markdown"))) (provide 'markdown-mode-autoloads)) "lsp-mode" ((lsp-completion lsp-protocol lsp-diagnostics lsp-modeline lsp-mode lsp-headerline lsp lsp-mode-autoloads lsp-lens) (define-obsolete-variable-alias 'lsp-prefer-capf 'lsp-completion-provider "lsp-mode 7.0.1") (autoload 'lsp-completion-at-point "lsp-completion" "Get lsp completions." nil nil) (autoload 'lsp-completion--enable "lsp-completion" "Enable LSP completion support." nil nil) (autoload 'lsp-completion-mode "lsp-completion" "Toggle LSP completion support.

If called interactively, enable Lsp-Completion mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (add-hook 'lsp-configure-hook (lambda nil (when (and lsp-auto-configure lsp-completion-enable) (lsp-completion--enable)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-completion" '("lsp-completion-"))) (define-obsolete-variable-alias 'lsp-diagnostic-package 'lsp-diagnostics-provider "lsp-mode 7.0.1") (define-obsolete-variable-alias 'lsp-flycheck-default-level 'lsp-diagnostics-flycheck-default-level "lsp-mode 7.0.1") (autoload 'lsp-diagnostics--enable "lsp-diagnostics" "Enable LSP checker support." nil nil) (autoload 'lsp-diagnostics-mode "lsp-diagnostics" "Toggle LSP diagnostics integration.

If called interactively, enable Lsp-Diagnostics mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (add-hook 'lsp-configure-hook (lambda nil (when lsp-auto-configure (lsp-diagnostics--enable)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-diagnostics" '("lsp-diagnostics-"))) (autoload 'lsp-headerline-breadcrumb-mode "lsp-headerline" "Toggle breadcrumb on headerline.

If called interactively, enable Lsp-Headerline-Breadcrumb mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'lsp-breadcrumb-go-to-symbol "lsp-headerline" "Go to the symbol on breadcrumb at SYMBOL-POSITION.

(fn SYMBOL-POSITION)" t nil) (autoload 'lsp-breadcrumb-narrow-to-symbol "lsp-headerline" "Narrow to the symbol range on breadcrumb at SYMBOL-POSITION.

(fn SYMBOL-POSITION)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-headerline" '("lsp-headerline-"))) (autoload 'lsp-lens-show "lsp-lens" "Display lenses in the buffer." t nil) (autoload 'lsp-lens-hide "lsp-lens" "Delete all lenses." t nil) (autoload 'lsp-lens-mode "lsp-lens" "Toggle code-lens overlays.

If called interactively, enable Lsp-Lens mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'lsp-avy-lens "lsp-lens" "Click lsp lens using `avy' package." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-lens" '("lsp-lens-"))) (put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp) (put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i)))) (autoload 'lsp "lsp-mode" "Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start. 

(fn &optional ARG)" t nil) (autoload 'lsp-deferred "lsp-mode" "Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-mode" '("lsp-" "make-lsp-client" "when-lsp-workspace" "with-lsp-workspace"))) (define-obsolete-variable-alias 'lsp-diagnostics-modeline-scope 'lsp-modeline-diagnostics-scope "lsp-mode 7.0.1") (autoload 'lsp-modeline-code-actions-mode "lsp-modeline" "Toggle code actions on modeline.

If called interactively, enable Lsp-Modeline-Code-Actions mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'lsp-diagnostics-modeline-mode 'lsp-modeline-diagnostics-mode "lsp-mode 7.0.1") (autoload 'lsp-modeline-diagnostics-mode "lsp-modeline" "Toggle diagnostics modeline.

If called interactively, enable Lsp-Modeline-Diagnostics mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-modeline" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-protocol" '("dash-expand:&RangeToPoint" "lsp"))) (provide 'lsp-mode-autoloads)) "ccls" ((ccls-tree ccls-inheritance-hierarchy ccls-semantic-highlight ccls-common ccls-member-hierarchy ccls ccls-autoloads ccls-code-lens ccls-call-hierarchy) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ccls" '("ccls-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ccls-call-hierarchy" '("ccls-call-hierarchy"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ccls-code-lens" '("ccls-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ccls-inheritance-hierarchy" '("ccls-inheritance-hierarchy"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ccls-member-hierarchy" '("ccls-member-hierarchy"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ccls-semantic-highlight" '("ccls-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ccls-tree" '("ccls-tree-"))) (provide 'ccls-autoloads)) "lsp-ui" ((lsp-ui lsp-ui-peek lsp-ui-doc lsp-ui-sideline lsp-ui-autoloads lsp-ui-imenu lsp-ui-flycheck) (autoload 'lsp-ui-mode "lsp-ui" "Toggle language server UI mode on or off.
\342\200\230lsp-ui-mode\342\200\231 is a minor mode that contains a series of useful UI
integrations for \342\200\230lsp-mode\342\200\231.  With a prefix argument ARG, enable
language server UI mode if ARG is positive, and disable it
otherwise.  If called from Lisp, enable the mode if ARG is
omitted or nil, and toggle it if ARG is \342\200\230toggle\342\200\231.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui" '("lsp-ui-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-doc" '("lsp-ui-doc-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-flycheck" '("lsp-ui-flycheck-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-imenu" '("lsp-ui-imenu"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-peek" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-sideline" '("lsp-ui-sideline"))) (provide 'lsp-ui-autoloads)) "company-lsp" ((company-lsp company-lsp-autoloads) (autoload 'company-lsp "company-lsp" "Define a company backend for lsp-mode.

See the documentation of `company-backends' for COMMAND and ARG.

(fn COMMAND &optional ARG &rest _)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-lsp" '("company-lsp-"))) (provide 'company-lsp-autoloads)) "bui" ((bui-core bui-info bui-button bui-utils bui bui-entry bui-history bui-autoloads bui-list) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui" '("bui-define-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-button" '("bui"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-core" '("bui-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-entry" '("bui-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-history" '("bui-history"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-info" '("bui-info-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-list" '("bui-list-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-utils" '("bui-"))) (provide 'bui-autoloads)) "ace-window" ((ace-window ace-window-autoloads) (autoload 'ace-select-window "ace-window" "Ace select window." t nil) (autoload 'ace-delete-window "ace-window" "Ace delete window." t nil) (autoload 'ace-swap-window "ace-window" "Ace swap window." t nil) (autoload 'ace-delete-other-windows "ace-window" "Ace delete other windows." t nil) (autoload 'ace-display-buffer "ace-window" "Make `display-buffer' and `pop-to-buffer' select using `ace-window'.
See sample config for `display-buffer-base-action' and `display-buffer-alist':
https://github.com/abo-abo/ace-window/wiki/display-buffer.

(fn BUFFER ALIST)" nil nil) (autoload 'ace-window "ace-window" "Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.
See `aw-scope' which extends it to work with frames.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window.

(fn ARG)" t nil) (defvar ace-window-display-mode nil "Non-nil if Ace-Window-Display mode is enabled.
See the `ace-window-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-display-mode'.") (custom-autoload 'ace-window-display-mode "ace-window" nil) (autoload 'ace-window-display-mode "ace-window" "Minor mode for showing the ace window key in the mode line.

If called interactively, enable Ace-Window-Display mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ace-window" '("ace-window-mode" "aw-"))) (provide 'ace-window-autoloads)) "pfuture" ((pfuture-autoloads pfuture) (autoload 'pfuture-new "pfuture" "Create a new future process for command CMD.
Any arguments after the command are interpreted as arguments to the command.
This will return a process object with additional 'stderr and 'stdout
properties, which can be read via (process-get process 'stdout) and
(process-get process 'stderr) or alternatively with
(pfuture-result process) or (pfuture-stderr process).

Note that CMD must be a *sequence* of strings, meaning
this is wrong: (pfuture-new \"git status\")
this is right: (pfuture-new \"git\" \"status\")

(fn &rest CMD)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pfuture" '("pfuture-"))) (provide 'pfuture-autoloads)) "treemacs" ((treemacs treemacs-async treemacs-header-line treemacs-bookmarks treemacs-workspaces treemacs-follow-mode treemacs-icons treemacs-logging treemacs-core-utils treemacs-compatibility treemacs-themes treemacs-mouse-interface treemacs-visuals treemacs-dom treemacs-extensions treemacs-rendering treemacs-tags treemacs-interface treemacs-tag-follow-mode treemacs-persistence treemacs-macros treemacs-customization treemacs-diagnostics treemacs-autoloads treemacs-faces treemacs-fringe-indicator treemacs-mode treemacs-filewatch-mode treemacs-scope) (autoload 'treemacs-version "treemacs" "Return the `treemacs-version'." t nil) (autoload 'treemacs "treemacs" "Initialise or toggle treemacs.
* If the treemacs window is visible hide it.
* If a treemacs buffer exists, but is not visible show it.
* If no treemacs buffer exists for the current frame create and show it.
* If the workspace is empty additionally ask for the root path of the first
  project to add." t nil) (autoload 'treemacs-find-file "treemacs" "Find and focus the current file in the treemacs window.
If the current buffer has visits no file or with a prefix ARG ask for the
file instead.
Will show/create a treemacs buffers if it is not visible/does not exist.
For the most part only useful when `treemacs-follow-mode' is not active.

(fn &optional ARG)" t nil) (autoload 'treemacs-find-tag "treemacs" "Find and move point to the tag at point in the treemacs view.
Most likely to be useful when `treemacs-tag-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root.  If no treemacs buffer exists it will be created with the current file's
containing directory as root.  Will do nothing if the current buffer is not
visiting a file or Emacs cannot find any tags for the current file." t nil) (autoload 'treemacs-select-window "treemacs" "Select the treemacs window if it is visible.
Bring it to the foreground if it is not visible.
Initialise a new treemacs buffer as calling `treemacs' would if there is no
treemacs buffer for this frame." t nil) (autoload 'treemacs-show-changelog "treemacs" "Show the changelog of treemacs." t nil) (autoload 'treemacs-edit-workspaces "treemacs" "Edit your treemacs workspaces and projects as an `org-mode' file." t nil) (autoload 'treemacs-display-current-project-exclusively "treemacs" "Display the current project, and *only* the current project.
Like `treemacs-add-and-display-current-project' this will add the current
project to treemacs based on either projectile or the built-in project.el.
However the 'exclusive' part means that it will make the current project the
only project, all other projects *will be removed* from the current workspace." t nil) (autoload 'treemacs-add-and-display-current-project "treemacs" "Open treemacs and add the current project root to the workspace.
The project is determined first by projectile (if treemacs-projectile is
installed), then by project.el.
If the project is already registered with treemacs just move point to its root.
An error message is displayed if the current buffer is not part of any project." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs" '("treemacs-version"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-async" '("treemacs-"))) (autoload 'treemacs-bookmark "treemacs-bookmarks" "Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location, in a similar
fashion to `treemacs-find-file'.

With a prefix argument ARG treemacs will also open the bookmarked location.

(fn &optional ARG)" t nil) (autoload 'treemacs--bookmark-handler "treemacs-bookmarks" "Open Treemacs into a bookmark RECORD.

(fn RECORD)" nil nil) (autoload 'treemacs-add-bookmark "treemacs-bookmarks" "Add the current node to Emacs' list of bookmarks.
For file and directory nodes their absolute path is saved.  Tag nodes
additionally also save the tag's position.  A tag can only be bookmarked if the
treemacs node is pointing to a valid buffer position." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-bookmarks" '("treemacs--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-compatibility" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-core-utils" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-customization" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-diagnostics" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-dom" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-extensions" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-filewatch-mode" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-follow-mode" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-fringe-indicator" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-header-line" '("treemacs-header-buttons-format"))) (autoload 'treemacs-resize-icons "treemacs-icons" "Resize the current theme's icons to the given SIZE.

If SIZE is 'nil' the icons are not resized and will retain their default size of
22 pixels.

There is only one size, the icons are square and the aspect ratio will be
preserved when resizing them therefore width and height are the same.

Resizing the icons only works if Emacs was built with ImageMagick support, or if
using Emacs >= 27.1,which has native image resizing support.  If this is not the
case this function will not have any effect.

Custom icons are not taken into account, only the size of treemacs' own icons
png are changed.

(fn SIZE)" t nil) (autoload 'treemacs-define-custom-icon "treemacs-icons" "Define a custom ICON for the current theme to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period.  This makes it possible to match file names like
'.gitignore' and 'Makefile'.

Additionally FILE-EXTENSIONS are also not case sensitive and will be stored in a
down-cased state.

(fn ICON &rest FILE-EXTENSIONS)" nil nil) (autoload 'treemacs-map-icons-with-auto-mode-alist "treemacs-icons" "Remaps icons for EXTENSIONS according to `auto-mode-alist'.
EXTENSIONS should be a list of file extensions such that they match the regex
stored in `auto-mode-alist', for example '(\".cc\").
MODE-ICON-ALIST is an alist that maps which mode from `auto-mode-alist' should
be assigned which treemacs icon, for example
'((c-mode . treemacs-icon-c)
  (c++-mode . treemacs-icon-cpp))

(fn EXTENSIONS MODE-ICON-ALIST)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-icons" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-interface" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-logging" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-macros" '("treemacs-"))) (autoload 'treemacs-mode "treemacs-mode" "A major mode for displaying the file system in a tree layout.

(fn)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-mode" '("treemacs-"))) (autoload 'treemacs-node-buffer-and-position "treemacs-mouse-interface" "Return source buffer or list of buffer and position for the current node.
This information can be used for future display.  Stay in the selected window
and ignore any prefix argument.

(fn &optional _)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-mouse-interface" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-persistence" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-rendering" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-scope" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-tag-follow-mode" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-tags" '("treemacs--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-themes" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-visuals" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-workspaces" '("treemacs-"))) (provide 'treemacs-autoloads)) "lsp-treemacs" ((lsp-treemacs-autoloads lsp-treemacs lsp-treemacs-themes) (autoload 'lsp-treemacs-errors-list "lsp-treemacs" "Display error list." t nil) (autoload 'lsp-treemacs-symbols "lsp-treemacs" "Show symbols view." t nil) (autoload 'lsp-treemacs-java-deps-list "lsp-treemacs" "Display error list." t nil) (autoload 'lsp-treemacs-java-deps-follow "lsp-treemacs" nil t nil) (defvar lsp-treemacs-sync-mode nil "Non-nil if Lsp-Treemacs-Sync mode is enabled.
See the `lsp-treemacs-sync-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-treemacs-sync-mode'.") (custom-autoload 'lsp-treemacs-sync-mode "lsp-treemacs" nil) (autoload 'lsp-treemacs-sync-mode "lsp-treemacs" "Global minor mode for synchronizing lsp-mode workspace folders and treemacs projects.

If called interactively, enable Lsp-Treemacs-Sync mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'lsp-treemacs-references "lsp-treemacs" "Show the references for the symbol at point.
With a prefix argument, select the new window and expand the tree of references automatically.

(fn ARG)" t nil) (autoload 'lsp-treemacs-implementations "lsp-treemacs" "Show the implementations for the symbol at point.
With a prefix argument, select the new window expand the tree of implementations automatically.

(fn ARG)" t nil) (autoload 'lsp-treemacs-call-hierarchy "lsp-treemacs" "Show the incoming call hierarchy for the symbol at point.
With a prefix argument, show the outgoing call hierarchy.

(fn OUTGOING)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-treemacs" '("lsp-treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-treemacs-themes" '("lsp-treemacs-theme"))) (provide 'lsp-treemacs-autoloads)) "posframe" ((posframe-autoloads posframe) (autoload 'posframe-workable-p "posframe" "Test posframe workable status." nil nil) (autoload 'posframe-show "posframe" "Pop up a posframe and show STRING at POSITION.

POSITION can be:
1. An integer, meaning point position.
2. A cons of two integers, meaning absolute X and Y coordinates.
3. Other type, in which case the corresponding POSHANDLER should be
   provided.

POSHANDLER is a function of one argument returning an actual
position.  Its argument is a plist of the following form:

  (:position xxx
   :position-info xxx
   :poshandler xxx
   :font-height xxx
   :font-width xxx
   :posframe xxx
   :posframe-width xxx
   :posframe-height xxx
   :posframe-buffer xxx
   :parent-frame xxx
   :parent-window-left xxx
   :parent-window-top xxx
   :parent-frame-width xxx
   :parent-frame-height xxx
   :parent-window xxx
   :parent-window-width  xxx
   :parent-window-height xxx
   :minibuffer-height
   :mode-line-height
   :header-line-height
   :tab-line-height
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

By default, poshandler is auto-selected based on the type of POSITION,
but the selection can be overridden using the POSHANDLER argument.
The builtin poshandler functions are listed below:

1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-bottom-center'
6.  `posframe-poshandler-frame-bottom-left-corner'
7.  `posframe-poshandler-frame-bottom-right-corner'
8.  `posframe-poshandler-window-center'
9.  `posframe-poshandler-window-top-center'
10. `posframe-poshandler-window-top-left-corner'
11. `posframe-poshandler-window-top-right-corner'
12. `posframe-poshandler-window-bottom-center'
13. `posframe-poshandler-window-bottom-left-corner'
14. `posframe-poshandler-window-bottom-right-corner'
15. `posframe-poshandler-point-top-left-corner'
16. `posframe-poshandler-point-bottom-left-corner'
17. `posframe-poshandler-point-bottom-left-corner-upward'

This posframe's buffer is BUFFER-OR-NAME, which can be a buffer
or a name of a (possibly nonexistent) buffer.

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before being shown in posframe.

WIDTH, MIN-WIDTH, HEIGHT and MIN-HEIGHT, specify bounds on the
new total size of posframe.  MIN-HEIGHT and MIN-WIDTH default to
the values of \342\200\230window-min-height\342\200\231 and \342\200\230window-min-width\342\200\231
respectively.  These arguments are specified in the canonical
character width and height of posframe.

If LEFT-FRINGE or RIGHT-FRINGE is a number, left fringe or
right fringe with be shown with the specified width.

By default, posframe shows no borders, but users can specify
borders by setting INTERNAL-BORDER-WIDTH to a positive number.
Border color can be specified by INTERNAL-BORDER-COLOR
or via the \342\200\230internal-border\342\200\231 face.

Posframe's font as well as foreground and background colors are
derived from the current frame by default, but can be overridden
using the FONT, FOREGROUND-COLOR and BACKGROUND-COLOR arguments,
respectively.

By default, posframe will display no header-line, mode-line and
tab-line.  In case a header-line, mode-line or tab-line is
desired, users can set RESPECT-HEADER-LINE, RESPECT-MODE-LINE or
RESPECT-TAB-LINE to t.

INITIALIZE is a function with no argument.  It will run when
posframe buffer is first selected with `with-current-buffer'
in `posframe-show', and only run once (for performance reasons).
If INITIALIZE is nil, `posframe-default-initialize-function' will
be used as fallback; this variable can be used to set posframe
buffer gobally.

If LINES-TRUNCATE is non-nil, then lines will truncate in the
posframe instead of wrap.

OVERRIDE-PARAMETERS is very powful, *all* the frame parameters
used by posframe's frame can be overridden by it.

TIMEOUT can specify the number of seconds after which the posframe
will auto-hide.

If REFRESH is a number, posframe's frame-size will be re-adjusted
every REFRESH seconds.

When ACCEPT-FOCUS is non-nil, posframe will accept focus.
be careful, you may face some bugs when set it to non-nil.

HIDEHANDLER is a function, when it return t, posframe will be
hide when `post-command-hook' is executed, this function has a
plist argument:

  (:posframe-buffer xxx
   :posframe-parent-buffer xxx)

The builtin hidehandler functions are listed below:

1. `posframe-hidehandler-when-buffer-switch'


You can use `posframe-delete-all' to delete all posframes.

(fn BUFFER-OR-NAME &key STRING POSITION POSHANDLER WIDTH HEIGHT MIN-WIDTH MIN-HEIGHT X-PIXEL-OFFSET Y-PIXEL-OFFSET LEFT-FRINGE RIGHT-FRINGE INTERNAL-BORDER-WIDTH INTERNAL-BORDER-COLOR FONT FOREGROUND-COLOR BACKGROUND-COLOR RESPECT-HEADER-LINE RESPECT-MODE-LINE RESPECT-TAB-LINE INITIALIZE NO-PROPERTIES KEEP-RATIO LINES-TRUNCATE OVERRIDE-PARAMETERS TIMEOUT REFRESH ACCEPT-FOCUS HIDEHANDLER &allow-other-keys)" nil nil) (autoload 'posframe-hide-all "posframe" "Hide all posframe frames." t nil) (autoload 'posframe-delete-all "posframe" "Delete all posframe frames and buffers." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "posframe" '("posframe-"))) (provide 'posframe-autoloads)) "dap-mode" ((dap-edge dap-cpptools dap-mode dap-utils dap-hydra dap-mode-autoloads dap-chrome dap-lldb dap-overlays dap-netcore dap-firefox dap-go dap-php dap-variables dap-elixir dap-launch dap-ui dap-node dap-mouse dap-gdb-lldb dap-pwsh dap-python dapui dap-ruby) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-chrome" '("dap-chrome-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-cpptools" '("dap-cpptools-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-edge" '("dap-edge-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-elixir" '("dap-elixir--populate-start-file-args"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-firefox" '("dap-firefox-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-gdb-lldb" '("dap-gdb-lldb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-go" '("dap-go-"))) (autoload 'dap-hydra "dap-hydra" "Run `dap-hydra/body'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-hydra" '("dap-hydra"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-launch" '("dap-launch-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-lldb" '("dap-lldb-"))) (defvar dap-mode nil "Non-nil if Dap mode is enabled.
See the `dap-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-mode'.") (custom-autoload 'dap-mode "dap-mode" nil) (autoload 'dap-mode "dap-mode" "Global minor mode for DAP mode.

If called interactively, enable Dap mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (defvar dap-auto-configure-mode nil "Non-nil if Dap-Auto-Configure mode is enabled.
See the `dap-auto-configure-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-auto-configure-mode'.") (custom-autoload 'dap-auto-configure-mode "dap-mode" nil) (autoload 'dap-auto-configure-mode "dap-mode" "Auto configure dap minor mode.

If called interactively, enable Dap-Auto-Configure mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-mode" '("dap-" "dash-expand:&dap-session"))) (defvar dap-tooltip-mode nil "Non-nil if Dap-Tooltip mode is enabled.
See the `dap-tooltip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-tooltip-mode'.") (custom-autoload 'dap-tooltip-mode "dap-mouse" nil) (autoload 'dap-tooltip-mode "dap-mouse" "Toggle the display of GUD tooltips.

If called interactively, enable Dap-Tooltip mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-mouse" '("dap-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-netcore" '("dap-netcore-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-node" '("dap-node-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-overlays" '("dap-overlays-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-php" '("dap-php-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-pwsh" '("dap-pwsh-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-python" '("dap-python-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-ruby" '("dap-ruby-"))) (defvar dap-ui-mode nil "Non-nil if Dap-Ui mode is enabled.
See the `dap-ui-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-ui-mode'.") (custom-autoload 'dap-ui-mode "dap-ui" nil) (autoload 'dap-ui-mode "dap-ui" "Displaying DAP visuals.

If called interactively, enable Dap-Ui mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'dap-ui-breakpoints-list "dap-ui" "List breakpoints." t nil) (defvar dap-ui-controls-mode nil "Non-nil if Dap-Ui-Controls mode is enabled.
See the `dap-ui-controls-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-ui-controls-mode'.") (custom-autoload 'dap-ui-controls-mode "dap-ui" nil) (autoload 'dap-ui-controls-mode "dap-ui" "Displaying DAP visuals.

If called interactively, enable Dap-Ui-Controls mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'dap-ui-sessions "dap-ui" "Show currently active sessions." t nil) (autoload 'dap-ui-locals "dap-ui" nil t nil) (autoload 'dap-ui-show-many-windows "dap-ui" "Show auto configured feature windows." t nil) (autoload 'dap-ui-hide-many-windows "dap-ui" "Hide all debug windows when sessions are dead." t nil) (autoload 'dap-ui-repl "dap-ui" "Start an adapter-specific REPL.
This could be used to evaluate JavaScript in a browser, to
evaluate python in the context of the debugee, ...." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-ui" '("dap-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-utils" '("dap-utils-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-variables" '("dap-variables-"))) (autoload 'dapui-loaded-sources "dapui" nil t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dapui" '("dapui-"))) (provide 'dap-mode-autoloads)) "transient" ((transient-autoloads transient) (autoload 'transient-insert-suffix "transient" "Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-insert-suffix 'lisp-indent-function 'defun) (autoload 'transient-append-suffix "transient" "Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-append-suffix 'lisp-indent-function 'defun) (autoload 'transient-replace-suffix "transient" "Replace the suffix at LOC in PREFIX with SUFFIX.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-replace-suffix 'lisp-indent-function 'defun) (autoload 'transient-remove-suffix "transient" "Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC)" nil nil) (function-put 'transient-remove-suffix 'lisp-indent-function 'defun) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "transient" '("transient-"))) (provide 'transient-autoloads)) "with-editor" ((with-editor-autoloads with-editor) (autoload 'with-editor-export-editor "with-editor" "Teach subsequent commands to use current Emacs instance as editor.

Set and export the environment variable ENVVAR, by default
\"EDITOR\".  The value is automatically generated to teach
commands to use the current Emacs instance as \"the editor\".

This works in `shell-mode', `term-mode' and `eshell-mode'.

(fn &optional (ENVVAR \"EDITOR\"))" t nil) (autoload 'with-editor-export-git-editor "with-editor" "Like `with-editor-export-editor' but always set `$GIT_EDITOR'." t nil) (autoload 'with-editor-export-hg-editor "with-editor" "Like `with-editor-export-editor' but always set `$HG_EDITOR'." t nil) (defvar shell-command-with-editor-mode nil "Non-nil if Shell-Command-With-Editor mode is enabled.
See the `shell-command-with-editor-mode' command
for a description of this minor mode.") (custom-autoload 'shell-command-with-editor-mode "with-editor" nil) (autoload 'shell-command-with-editor-mode "with-editor" "Teach `shell-command' to use current Emacs instance as editor.

If called interactively, enable Shell-Command-With-Editor mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

Teach `shell-command', and all commands that ultimately call that
command, to use the current Emacs instance as editor by executing
\"EDITOR=CLIENT COMMAND&\" instead of just \"COMMAND&\".

CLIENT is automatically generated; EDITOR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming no other variable overrides the effect of \"$EDITOR\".
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Alternatively you can use the `with-editor-async-shell-command',
which also allows the use of another variable instead of
\"EDITOR\".

(fn &optional ARG)" t nil) (autoload 'with-editor-async-shell-command "with-editor" "Like `async-shell-command' but with `$EDITOR' set.

Execute string \"ENVVAR=CLIENT COMMAND\" in an inferior shell;
display output, if any.  With a prefix argument prompt for an
environment variable, otherwise the default \"EDITOR\" variable
is used.  With a negative prefix argument additionally insert
the COMMAND's output at point.

CLIENT is automatically generated; ENVVAR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming it respects ENVVAR as an \"EDITOR\"-like variable.
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Also see `async-shell-command' and `shell-command'.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil) (autoload 'with-editor-shell-command "with-editor" "Like `shell-command' or `with-editor-async-shell-command'.
If COMMAND ends with \"&\" behave like the latter,
else like the former.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "with-editor" '("server-" "shell-command--shell-command-with-editor-mode" "start-file-process--with-editor-process-filter" "with-editor"))) (provide 'with-editor-autoloads)) "git-commit" ((git-commit-autoloads git-commit) (defvar global-git-commit-mode t "Non-nil if Global Git-Commit mode is enabled.
See the `global-git-commit-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-commit-mode'.") (custom-autoload 'global-git-commit-mode "git-commit" nil) (autoload 'global-git-commit-mode "git-commit" "Edit Git commit messages.
This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message.

If called interactively, enable Global Git-Commit mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (defconst git-commit-filename-regexp "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'") (autoload 'git-commit-setup-check-buffer "git-commit" nil nil nil) (autoload 'git-commit-setup "git-commit" nil nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-commit" '("git-commit-"))) (provide 'git-commit-autoloads)) "magit" ((magit-core magit-diff magit-imenu magit-wip magit-section magit-git magit-autoloads magit-fetch magit-process git-rebase magit-pkg magit-files magit-notes magit-branch magit-pull magit-push magit-tag magit-mode magit magit-margin magit-ediff magit-autorevert magit-refs magit-status magit-submodule magit-patch magit-stash magit-sequence magit-bisect magit-commit magit-gitignore magit-obsolete magit-bookmark magit-clone magit-utils magit-worktree magit-subtree magit-remote magit-log magit-reset magit-reflog magit-blame magit-repos magit-extras magit-merge magit-transient magit-apply) (autoload 'git-rebase-current-line "git-rebase" "Parse current line into a `git-rebase-action' instance.
If the current line isn't recognized as a rebase line, an
instance with all nil values is returned." nil nil) (autoload 'git-rebase-mode "git-rebase" "Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

(fn)" t nil) (defconst git-rebase-filename-regexp "/git-rebase-todo\\'") (add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp 'git-rebase-mode)) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-rebase" '("git-rebase-"))) (autoload 'magit-dispatch "magit" nil t) (autoload 'magit-run "magit" nil t) (autoload 'magit-git-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t nil) (autoload 'magit-git-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree.

(fn COMMAND)" t nil) (autoload 'magit-shell-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t nil) (autoload 'magit-shell-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree.

(fn COMMAND)" t nil) (autoload 'magit-version "magit" "Return the version of Magit currently in use.
If optional argument PRINT-DEST is non-nil, output
stream (interactively, the echo area, or the current buffer with
a prefix argument), also print the used versions of Magit, Git,
and Emacs to it.

(fn &optional PRINT-DEST)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit" '("magit-"))) (autoload 'magit-stage-file "magit-apply" "Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation.

(fn FILE)" t nil) (autoload 'magit-stage-modified "magit-apply" "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.

(fn &optional ALL)" t nil) (autoload 'magit-unstage-file "magit-apply" "Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation.

(fn FILE)" t nil) (autoload 'magit-unstage-all "magit-apply" "Remove all changes from the staging area." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-apply" '("magit-"))) (put 'magit-auto-revert-mode 'globalized-minor-mode t) (defvar magit-auto-revert-mode (not (or global-auto-revert-mode noninteractive)) "Non-nil if Magit-Auto-Revert mode is enabled.
See the `magit-auto-revert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-auto-revert-mode'.") (custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil) (autoload 'magit-auto-revert-mode "magit-autorevert" "Toggle Auto-Revert mode in all buffers.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Revert mode is enabled in all buffers where
`magit-turn-on-auto-revert-mode-if-desired' would do it.
See `auto-revert-mode' for more information on Auto-Revert mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-autorevert" '("auto-revert-buffer" "magit-"))) (autoload 'magit-bisect "magit-bisect" nil t) (autoload 'magit-bisect-start "magit-bisect" "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a know
good and a bad commit.  To move the session forward use the
other actions from the bisect transient command (\\<magit-status-mode-map>\\[magit-bisect]).

(fn BAD GOOD)" t nil) (autoload 'magit-bisect-reset "magit-bisect" "After bisecting, cleanup bisection state and return to original `HEAD'." t nil) (autoload 'magit-bisect-good "magit-bisect" "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question." t nil) (autoload 'magit-bisect-bad "magit-bisect" "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question." t nil) (autoload 'magit-bisect-skip "magit-bisect" "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one." t nil) (autoload 'magit-bisect-run "magit-bisect" "Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'.

(fn CMDLINE &optional BAD GOOD)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-bisect" '("magit-"))) (autoload 'magit-blame-echo "magit-blame" nil t) (autoload 'magit-blame-addition "magit-blame" nil t) (autoload 'magit-blame-removal "magit-blame" nil t) (autoload 'magit-blame-reverse "magit-blame" nil t) (autoload 'magit-blame "magit-blame" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-blame" '("magit-"))) (autoload 'magit--handle-bookmark "magit-bookmark" "Open a bookmark created by `magit--make-bookmark'.
Call the `magit-*-setup-buffer' function of the the major-mode
with the variables' values as arguments, which were recorded by
`magit--make-bookmark'.  Ignore `magit-display-buffer-function'.

(fn BOOKMARK)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-bookmark" '("magit--make-bookmark"))) (autoload 'magit-branch "magit" nil t) (autoload 'magit-checkout "magit-branch" "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch, then that becomes the current
branch.  If it is something else, then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

(git checkout REVISION).

(fn REVISION)" t nil) (autoload 'magit-branch-create "magit-branch" "Create BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-and-checkout "magit-branch" "Create and checkout BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-or-checkout "magit-branch" "Hybrid between `magit-checkout' and `magit-branch-and-checkout'.

Ask the user for an existing branch or revision.  If the user
input actually can be resolved as a branch or revision, then
check that out, just like `magit-checkout' would.

Otherwise create and checkout a new branch using the input as
its name.  Before doing so read the starting-point for the new
branch.  This is similar to what `magit-branch-and-checkout'
does.

(fn ARG &optional START-POINT)" t nil) (autoload 'magit-branch-checkout "magit-branch" "Checkout an existing or new local branch.

Read a branch name from the user offering all local branches and
a subset of remote branches as candidates.  Omit remote branches
for which a local branch by the same name exists from the list
of candidates.  The user can also enter a completely new branch
name.

- If the user selects an existing local branch, then check that
  out.

- If the user selects a remote branch, then create and checkout
  a new local branch with the same name.  Configure the selected
  remote branch as push target.

- If the user enters a new branch name, then create and check
  that out, after also reading the starting-point from the user.

In the latter two cases the upstream is also set.  Whether it is
set to the chosen START-POINT or something else depends on the
value of `magit-branch-adjust-remote-upstream-alist', just like
when using `magit-branch-and-checkout'.

(fn BRANCH &optional START-POINT)" t nil) (autoload 'magit-branch-orphan "magit-branch" "Create and checkout an orphan BRANCH with contents from revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-spinout "magit-branch" "Create new branch from the unpushed commits.
Like `magit-branch-spinoff' but remain on the current branch.
If there are any uncommitted changes, then behave exactly like
`magit-branch-spinoff'.

(fn BRANCH &optional FROM)" t nil) (autoload 'magit-branch-spinoff "magit-branch" "Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself.

If optional FROM is non-nil, then the source branch is reset
to `FROM~', instead of to the last commit it shares with its
upstream.  Interactively, FROM is only ever non-nil, if the
region selects some commits, and among those commits, FROM is
the commit that is the fewest commits ahead of the source
branch.

The commit at the other end of the selection actually does not
matter, all commits between FROM and `HEAD' are moved to the new
branch.  If FROM is not reachable from `HEAD' or is reachable
from the source branch's upstream, then an error is raised.

(fn BRANCH &optional FROM)" t nil) (autoload 'magit-branch-reset "magit-branch" "Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirm the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset.

(fn BRANCH TO &optional SET-UPSTREAM)" t nil) (autoload 'magit-branch-delete "magit-branch" "Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

(fn BRANCHES &optional FORCE)" t nil) (autoload 'magit-branch-rename "magit-branch" "Rename the branch named OLD to NEW.

With a prefix argument FORCE, rename even if a branch named NEW
already exists.

If `branch.OLD.pushRemote' is set, then unset it.  Depending on
the value of `magit-branch-rename-push-target' (which see) maybe
set `branch.NEW.pushRemote' and maybe rename the push-target on
the remote.

(fn OLD NEW &optional FORCE)" t nil) (autoload 'magit-branch-shelve "magit-branch" "Shelve a BRANCH.
Rename \"refs/heads/BRANCH\" to \"refs/shelved/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t nil) (autoload 'magit-branch-unshelve "magit-branch" "Unshelve a BRANCH
Rename \"refs/shelved/BRANCH\" to \"refs/heads/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t nil) (autoload 'magit-branch-configure "magit-branch" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-branch" '("magit-"))) (autoload 'magit-clone "magit-clone" nil t) (autoload 'magit-clone-regular "magit-clone" "Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (autoload 'magit-clone-shallow "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
With a prefix argument read the DEPTH of the clone;
otherwise use 1.

(fn REPOSITORY DIRECTORY ARGS DEPTH)" t nil) (autoload 'magit-clone-shallow-since "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits before DATE, which is read from the
user.

(fn REPOSITORY DIRECTORY ARGS DATE)" t nil) (autoload 'magit-clone-shallow-exclude "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits reachable from EXCLUDE, which is a
branch or tag read from the user.

(fn REPOSITORY DIRECTORY ARGS EXCLUDE)" t nil) (autoload 'magit-clone-bare "magit-clone" "Create a bare clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (autoload 'magit-clone-mirror "magit-clone" "Create a mirror of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-clone" '("magit-clone-"))) (autoload 'magit-commit "magit-commit" nil t) (autoload 'magit-commit-create "magit-commit" "Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.

(git commit [--amend] ARGS)

(fn &optional ARGS)" t nil) (autoload 'magit-commit-amend "magit-commit" "Amend the last commit.

(git commit --amend ARGS)

(fn &optional ARGS)" t nil) (autoload 'magit-commit-extend "magit-commit" "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  
(git commit
--amend --no-edit)

(fn &optional ARGS OVERRIDE-DATE)" t nil) (autoload 'magit-commit-reword "magit-commit" "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

(git commit --amend --only)

(fn &optional ARGS OVERRIDE-DATE)" t nil) (autoload 'magit-commit-fixup "magit-commit" "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-squash "magit-commit" "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-augment "magit-commit" "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-instant-fixup "magit-commit" "Create a fixup commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-instant-squash "magit-commit" "Create a squash commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-reshelve "magit-commit" "Change the committer date and possibly the author date of `HEAD'.

If you are the author of `HEAD', then both dates are changed,
otherwise only the committer date.  The current time is used
as the initial minibuffer input and the original author (if
that is you) or committer date is available as the previous
history element.

(fn DATE)" t nil) (autoload 'magit-commit-absorb "magit-commit" nil t) (autoload 'magit-commit-autofixup "magit-commit" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-commit" '("magit-"))) (autoload 'magit-diff "magit-diff" nil t) (autoload 'magit-diff-refresh "magit-diff" nil t) (autoload 'magit-diff-dwim "magit-diff" "Show changes for the thing at point.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-range "magit-diff" "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range).

(fn REV-OR-RANGE &optional ARGS FILES)" t nil) (autoload 'magit-diff-working-tree "magit-diff" "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t nil) (autoload 'magit-diff-staged "magit-diff" "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t nil) (autoload 'magit-diff-unstaged "magit-diff" "Show changes between the working tree and the index.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-unmerged "magit-diff" "Show changes that are being merged.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-while-committing "magit-diff" "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed.

(fn &optional ARGS)" t nil) (autoload 'magit-diff-buffer-file "magit-diff" "Show diff for the blob or file visited in the current buffer.

When the buffer visits a blob, then show the respective commit.
When the buffer visits a file, then show the differenced between
`HEAD' and the working tree.  In both cases limit the diff to
the file or blob." t nil) (autoload 'magit-diff-paths "magit-diff" "Show changes between any two files on disk.

(fn A B)" t nil) (autoload 'magit-show-commit "magit-diff" "Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

(fn REV &optional ARGS FILES MODULE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-diff" '("magit-"))) (autoload 'magit-ediff "magit-ediff" nil) (autoload 'magit-ediff-resolve "magit-ediff" "Resolve outstanding conflicts in FILE using Ediff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

(fn FILE)" t nil) (autoload 'magit-ediff-stage "magit-ediff" "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-compare "magit-ediff" "Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range).

(fn REVA REVB FILEA FILEB)" t nil) (autoload 'magit-ediff-dwim "magit-ediff" "Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run." t nil) (autoload 'magit-ediff-show-staged "magit-ediff" "Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-unstaged "magit-ediff" "Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-working-tree "magit-ediff" "Show changes between `HEAD' and working tree using Ediff.
FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-commit "magit-ediff" "Show changes introduced by COMMIT using Ediff.

(fn COMMIT)" t nil) (autoload 'magit-ediff-show-stash "magit-ediff" "Show changes introduced by STASH using Ediff.
`magit-ediff-show-stash-with-index' controls whether a
three-buffer Ediff is used in order to distinguish changes in the
stash that were staged.

(fn STASH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-ediff" '("magit-ediff-"))) (autoload 'magit-run-git-gui "magit-extras" "Run `git gui' for the current git repository." t nil) (autoload 'magit-run-git-gui-blame "magit-extras" "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

(fn COMMIT FILENAME &optional LINENUM)" t nil) (autoload 'magit-run-gitk "magit-extras" "Run `gitk' in the current repository." t nil) (autoload 'magit-run-gitk-branches "magit-extras" "Run `gitk --branches' in the current repository." t nil) (autoload 'magit-run-gitk-all "magit-extras" "Run `gitk --all' in the current repository." t nil) (autoload 'ido-enter-magit-status "magit-extras" "Drop into `magit-status' from file switching.

This command does not work in Emacs 26.1.
See https://github.com/magit/magit/issues/3634
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd \"C-x g\") \\='ido-enter-magit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (define-key ido-common-completion-map
    (kbd \"C-x g\") \\='ido-enter-magit-status)" t nil) (autoload 'magit-project-status "magit-extras" "Run `magit-status' in the current project's root." t nil) (autoload 'magit-dired-jump "magit-extras" "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'.

(fn &optional OTHER-WINDOW)" t nil) (autoload 'magit-dired-log "magit-extras" "Show log for all marked files, or the current file.

(fn &optional FOLLOW)" t nil) (autoload 'magit-do-async-shell-command "magit-extras" "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point.

(fn FILE)" t nil) (autoload 'magit-previous-line "magit-extras" "Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t nil) (function-put 'magit-previous-line 'interactive-only '"use `forward-line' with negative argument instead.") (autoload 'magit-next-line "magit-extras" "Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t nil) (function-put 'magit-next-line 'interactive-only 'forward-line) (autoload 'magit-clean "magit-extras" "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

(git clean -f -d [-x|-X])

(fn &optional ARG)" t nil) (autoload 'magit-add-change-log-entry "magit-extras" "Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil) (autoload 'magit-add-change-log-entry-other-window "magit-extras" "Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME)" t nil) (autoload 'magit-edit-line-commit "magit-extras" "Edit the commit that added the current line.

With a prefix argument edit the commit that removes the line,
if any.  The commit is determined using `git blame' and made
editable using `git rebase --interactive' if it is reachable
from `HEAD', or by checking out the commit (or a branch that
points at it) otherwise.

(fn &optional TYPE)" t nil) (autoload 'magit-diff-edit-hunk-commit "magit-extras" "From a hunk, edit the respective commit and visit the file.

First visit the file being modified by the hunk at the correct
location using `magit-diff-visit-file'.  This actually visits a
blob.  When point is on a diff header, not within an individual
hunk, then this visits the blob the first hunk is about.

Then invoke `magit-edit-line-commit', which uses an interactive
rebase to make the commit editable, or if that is not possible
because the commit is not reachable from `HEAD' by checking out
that commit directly.  This also causes the actual worktree file
to be visited.

Neither the blob nor the file buffer are killed when finishing
the rebase.  If that is undesirable, then it might be better to
use `magit-rebase-edit-command' instead of this command.

(fn FILE)" t nil) (autoload 'magit-reshelve-since "magit-extras" "Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  Then read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history.

(fn REV)" t nil) (autoload 'magit-pop-revision-stack "magit-extras" "Insert a representation of a revision into the current buffer.

Pop a revision from the `magit-revision-stack' and insert it into
the current buffer according to `magit-pop-revision-stack-format'.
Revisions can be put on the stack using `magit-copy-section-value'
and `magit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g. while composing a commit
message), then that repository is used.  Otherwise (e.g. while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too.

(fn REV TOPLEVEL)" t nil) (autoload 'magit-copy-section-value "magit-extras" "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `magit-revision-stack' for use
with `magit-pop-revision-stack'.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.  If a prefix argument is used and the region is within a
hunk, strip the outer diff marker column." t nil) (autoload 'magit-copy-buffer-revision "magit-extras" "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `magit-revision-stack'.

This command is mainly intended for use in `magit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Magit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `magit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'." t nil) (autoload 'magit-abort-dwim "magit-extras" "Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-extras" '("magit-"))) (autoload 'magit-fetch "magit-fetch" nil t) (autoload 'magit-fetch-from-pushremote "magit-fetch" nil t) (autoload 'magit-fetch-from-upstream "magit-fetch" nil t) (autoload 'magit-fetch-other "magit-fetch" "Fetch from another repository.

(fn REMOTE ARGS)" t nil) (autoload 'magit-fetch-branch "magit-fetch" "Fetch a BRANCH from a REMOTE.

(fn REMOTE BRANCH ARGS)" t nil) (autoload 'magit-fetch-refspec "magit-fetch" "Fetch a REFSPEC from a REMOTE.

(fn REMOTE REFSPEC ARGS)" t nil) (autoload 'magit-fetch-all "magit-fetch" "Fetch from all remotes.

(fn ARGS)" t nil) (autoload 'magit-fetch-all-prune "magit-fetch" "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote." t nil) (autoload 'magit-fetch-all-no-prune "magit-fetch" "Fetch from all remotes." t nil) (autoload 'magit-fetch-modules "magit-fetch" "Fetch all submodules.

Option `magit-fetch-modules-jobs' controls how many submodules
are being fetched in parallel.  Also fetch the super-repository,
because `git-fetch' does not support not doing that.  With a
prefix argument fetch all remotes.

(fn &optional ALL)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-fetch" '("magit-"))) (autoload 'magit-find-file "magit-files" "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-find-file-other-window "magit-files" "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-find-file-other-frame "magit-files" "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-file-dispatch "magit" nil t) (put 'global-magit-file-mode 'globalized-minor-mode t) (defvar global-magit-file-mode t "Non-nil if Global Magit-File mode is enabled.
See the `global-magit-file-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-file-mode'.") (custom-autoload 'global-magit-file-mode "magit-files" nil) (autoload 'global-magit-file-mode "magit-files" "Toggle Magit-File mode in all buffers.
With prefix ARG, enable Global Magit-File mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-File mode is enabled in all buffers where
`magit-file-mode-turn-on' would do it.
See `magit-file-mode' for more information on Magit-File mode.

(fn &optional ARG)" t nil) (autoload 'magit-blob-visit-file "magit-files" "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree." t nil) (autoload 'magit-file-checkout "magit-files" "Checkout FILE from REV.

(fn REV FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-files" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-git" '("magit-"))) (autoload 'magit-gitignore "magit-gitignore" nil t) (autoload 'magit-gitignore-in-topdir "magit-gitignore" "Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file.

(fn RULE)" t nil) (autoload 'magit-gitignore-in-subdir "magit-gitignore" "Add the Git ignore RULE to a \".gitignore\" file.
Prompted the user for a directory and add the rule to the
\".gitignore\" file in that directory.  Since such files are
tracked, they are shared with other clones of the repository.
Also stage the file.

(fn RULE DIRECTORY)" t nil) (autoload 'magit-gitignore-in-gitdir "magit-gitignore" "Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository.

(fn RULE)" t nil) (autoload 'magit-gitignore-on-system "magit-gitignore" "Add the Git ignore RULE to the file specified by `core.excludesFile'.
Rules that are defined in that file affect all local repositories.

(fn RULE)" t nil) (autoload 'magit-skip-worktree "magit-gitignore" "Call \"git update-index --skip-worktree -- FILE\".

(fn FILE)" t nil) (autoload 'magit-no-skip-worktree "magit-gitignore" "Call \"git update-index --no-skip-worktree -- FILE\".

(fn FILE)" t nil) (autoload 'magit-assume-unchanged "magit-gitignore" "Call \"git update-index --assume-unchanged -- FILE\".

(fn FILE)" t nil) (autoload 'magit-no-assume-unchanged "magit-gitignore" "Call \"git update-index --no-assume-unchanged -- FILE\".

(fn FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-gitignore" '("magit-"))) (autoload 'magit-imenu--log-prev-index-position-function "magit-imenu" "Move point to previous line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--log-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--diff-prev-index-position-function "magit-imenu" "Move point to previous file line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--diff-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--status-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--refs-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--cherry-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--submodule-prev-index-position-function "magit-imenu" "Move point to previous line in magit-submodule-list buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--submodule-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--repolist-prev-index-position-function "magit-imenu" "Move point to previous line in magit-repolist buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--repolist-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--process-prev-index-position-function "magit-imenu" "Move point to previous process in magit-process buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--process-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--rebase-prev-index-position-function "magit-imenu" "Move point to previous commit in git-rebase buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--rebase-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-imenu" '("magit-imenu--index-function"))) (autoload 'magit-log "magit-log" nil t) (autoload 'magit-log-refresh "magit-log" nil t) (autoload 'magit-log-current "magit-log" "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer.

(fn REVS &optional ARGS FILES)" t nil) (autoload 'magit-log-other "magit-log" "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates.

(fn REVS &optional ARGS FILES)" t nil) (autoload 'magit-log-head "magit-log" "Show log for `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-branches "magit-log" "Show log for all local branches and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-matching-branches "magit-log" "Show log for all branches matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t nil) (autoload 'magit-log-matching-tags "magit-log" "Show log for all tags matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t nil) (autoload 'magit-log-all-branches "magit-log" "Show log for all local and remote branches and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-all "magit-log" "Show log for all references and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-buffer-file "magit-log" "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is an active log
argument, then follow renames.  When the region is active,
restrict the log to the lines that the region touches.

(fn &optional FOLLOW BEG END)" t nil) (autoload 'magit-log-trace-definition "magit-log" "Show log for the definition at point.

(fn FILE FN REV)" t nil) (autoload 'magit-log-merged "magit-log" "Show log for the merge of COMMIT into BRANCH.

More precisely, find merge commit M that brought COMMIT into
BRANCH, and show the log of the range \"M^1..M\".  If COMMIT is
directly on BRANCH, then show approximately twenty surrounding
commits instead.

This command requires git-when-merged, which is available from
https://github.com/mhagger/git-when-merged.

(fn COMMIT BRANCH &optional ARGS FILES)" t nil) (autoload 'magit-log-move-to-parent "magit-log" "Move to the Nth parent of the current commit.

(fn &optional N)" t nil) (autoload 'magit-cherry "magit-log" "Show commits in a branch that are not merged in the upstream branch.

(fn HEAD UPSTREAM)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-log" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-margin" '("magit-"))) (autoload 'magit-merge "magit" nil t) (autoload 'magit-merge-plain "magit-merge" "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

(git merge --no-edit|--no-commit [ARGS] REV)

(fn REV &optional ARGS NOCOMMIT)" t nil) (autoload 'magit-merge-editmsg "magit-merge" "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

(git merge --edit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t nil) (autoload 'magit-merge-nocommit "magit-merge" "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

(git merge --no-commit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t nil) (autoload 'magit-merge-into "magit-merge" "Merge the current branch into BRANCH and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
branch, then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t nil) (autoload 'magit-merge-absorb "magit-merge" "Merge BRANCH into the current branch and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t nil) (autoload 'magit-merge-squash "magit-merge" "Squash commit REV into the current branch; don't create a commit.

(git merge --squash REV)

(fn REV)" t nil) (autoload 'magit-merge-preview "magit-merge" "Preview result of merging REV into the current branch.

(fn REV)" t nil) (autoload 'magit-merge-abort "magit-merge" "Abort the current merge operation.

(git merge --abort)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-merge" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-mode" '("disable-magit-save-buffers" "inhibit-magit-refresh" "magit-"))) (autoload 'magit-notes "magit" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-notes" '("magit-notes-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-obsolete" '("magit--magit-popup-warning"))) (autoload 'magit-patch "magit-patch" nil t) (autoload 'magit-patch-create "magit-patch" nil t) (autoload 'magit-patch-apply "magit-patch" nil t) (autoload 'magit-patch-save "magit-patch" "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used.

(fn FILE &optional ARG)" t nil) (autoload 'magit-request-pull "magit-patch" "Request upstream to pull from your public repository.

URL is the url of your publicly accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit.

(fn URL START END)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-patch" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-process" '("magit-" "tramp-sh-handle-"))) (autoload 'magit-pull "magit-pull" nil t) (autoload 'magit-pull-from-pushremote "magit-pull" nil t) (autoload 'magit-pull-from-upstream "magit-pull" nil t) (autoload 'magit-pull-branch "magit-pull" "Pull from a branch read in the minibuffer.

(fn SOURCE ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-pull" '("magit-pull-"))) (autoload 'magit-push "magit-push" nil t) (autoload 'magit-push-current-to-pushremote "magit-push" nil t) (autoload 'magit-push-current-to-upstream "magit-push" nil t) (autoload 'magit-push-current "magit-push" "Push the current branch to a branch read in the minibuffer.

(fn TARGET ARGS)" t nil) (autoload 'magit-push-other "magit-push" "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer.

(fn SOURCE TARGET ARGS)" t nil) (autoload 'magit-push-refspecs "magit-push" "Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used.

(fn REMOTE REFSPECS ARGS)" t nil) (autoload 'magit-push-matching "magit-push" "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation.

(fn REMOTE &optional ARGS)" t nil) (autoload 'magit-push-tags "magit-push" "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default.

(fn REMOTE &optional ARGS)" t nil) (autoload 'magit-push-tag "magit-push" "Push a tag to another repository.

(fn TAG REMOTE &optional ARGS)" t nil) (autoload 'magit-push-notes-ref "magit-push" "Push a notes ref to another repository.

(fn REF REMOTE &optional ARGS)" t nil) (autoload 'magit-push-implicitly "magit-push" "Push somewhere without using an explicit refspec.

This command simply runs \"git push -v [ARGS]\".  ARGS are the
arguments specified in the popup buffer.  No explicit refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

The function `magit-push-implicitly--desc' attempts to predict
what this command will do.  The value it returns is displayed in
the popup buffer.

(fn ARGS)" t nil) (autoload 'magit-push-to-remote "magit-push" "Push to REMOTE without using an explicit refspec.
The REMOTE is read in the minibuffer.

This command simply runs \"git push -v [ARGS] REMOTE\".  ARGS
are the arguments specified in the popup buffer.  No refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

(fn REMOTE ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-push" '("magit-"))) (autoload 'magit-reflog-current "magit-reflog" "Display the reflog of the current branch.
If `HEAD' is detached, then show the reflog for that instead." t nil) (autoload 'magit-reflog-other "magit-reflog" "Display the reflog of a branch or another ref.

(fn REF)" t nil) (autoload 'magit-reflog-head "magit-reflog" "Display the `HEAD' reflog." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-reflog" '("magit-reflog-"))) (autoload 'magit-show-refs "magit-refs" nil t) (autoload 'magit-show-refs-head "magit-refs" "List and compare references in a dedicated buffer.
Compared with `HEAD'.

(fn &optional ARGS)" t nil) (autoload 'magit-show-refs-current "magit-refs" "List and compare references in a dedicated buffer.
Compare with the current branch or `HEAD' if it is detached.

(fn &optional ARGS)" t nil) (autoload 'magit-show-refs-other "magit-refs" "List and compare references in a dedicated buffer.
Compared with a branch read from the user.

(fn &optional REF ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-refs" '("magit-"))) (autoload 'magit-remote "magit-remote" nil t) (autoload 'magit-remote-add "magit-remote" "Add a remote named REMOTE and fetch it.

(fn REMOTE URL &optional ARGS)" t nil) (autoload 'magit-remote-rename "magit-remote" "Rename the remote named OLD to NEW.

(fn OLD NEW)" t nil) (autoload 'magit-remote-remove "magit-remote" "Delete the remote named REMOTE.

(fn REMOTE)" t nil) (autoload 'magit-remote-prune "magit-remote" "Remove stale remote-tracking branches for REMOTE.

(fn REMOTE)" t nil) (autoload 'magit-remote-prune-refspecs "magit-remote" "Remove stale refspecs for REMOTE.

A refspec is stale if there no longer exists at least one branch
on the remote that would be fetched due to that refspec.  A stale
refspec is problematic because its existence causes Git to refuse
to fetch according to the remaining non-stale refspecs.

If only stale refspecs remain, then offer to either delete the
remote or to replace the stale refspecs with the default refspec.

Also remove the remote-tracking branches that were created due to
the now stale refspecs.  Other stale branches are not removed.

(fn REMOTE)" t nil) (autoload 'magit-remote-set-head "magit-remote" "Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that.

(fn REMOTE &optional BRANCH)" t nil) (autoload 'magit-remote-unset-head "magit-remote" "Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\".

(fn REMOTE)" t nil) (autoload 'magit-remote-configure "magit-remote" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-remote" '("magit-"))) (autoload 'magit-list-repositories "magit-repos" "Display a list of repositories.

Use the options `magit-repository-directories' to control which
repositories are displayed." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-repos" '("magit-"))) (autoload 'magit-reset "magit" nil t) (autoload 'magit-reset-mixed "magit-reset" "Reset the `HEAD' and index to COMMIT, but not the working tree.

(git reset --mixed COMMIT)

(fn COMMIT)" t nil) (autoload 'magit-reset-soft "magit-reset" "Reset the `HEAD' to COMMIT, but not the index and working tree.

(git reset --soft REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-hard "magit-reset" "Reset the `HEAD', index, and working tree to COMMIT.

(git reset --hard REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-keep "magit-reset" "Reset the `HEAD' and index to COMMIT, while keeping uncommitted changes.

(git reset --keep REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-index "magit-reset" "Reset the index to COMMIT.
Keep the `HEAD' and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.

(git reset COMMIT .)

(fn COMMIT)" t nil) (autoload 'magit-reset-worktree "magit-reset" "Reset the worktree to COMMIT.
Keep the `HEAD' and index as-is.

(fn COMMIT)" t nil) (autoload 'magit-reset-quickly "magit-reset" "Reset the `HEAD' and index to COMMIT, and possibly the working tree.
With a prefix argument reset the working tree otherwise don't.

(git reset --mixed|--hard COMMIT)

(fn COMMIT &optional HARD)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-reset" '("magit-reset-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-section" '("magit-"))) (autoload 'magit-sequencer-continue "magit-sequence" "Resume the current cherry-pick or revert sequence." t nil) (autoload 'magit-sequencer-skip "magit-sequence" "Skip the stopped at commit during a cherry-pick or revert sequence." t nil) (autoload 'magit-sequencer-abort "magit-sequence" "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started." t nil) (autoload 'magit-cherry-pick "magit-sequence" nil t) (autoload 'magit-cherry-copy "magit-sequence" "Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting.

(fn COMMITS &optional ARGS)" t nil) (autoload 'magit-cherry-apply "magit-sequence" "Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting.

(fn COMMITS &optional ARGS)" t nil) (autoload 'magit-cherry-harvest "magit-sequence" "Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH &optional ARGS)" t nil) (autoload 'magit-cherry-donate "magit-sequence" "Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH &optional ARGS)" t nil) (autoload 'magit-cherry-spinout "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-cherry-spinoff "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-revert "magit-sequence" nil t) (autoload 'magit-revert-and-commit "magit-sequence" "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t nil) (autoload 'magit-revert-no-commit "magit-sequence" "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t nil) (autoload 'magit-am "magit-sequence" nil t) (autoload 'magit-am-apply-patches "magit-sequence" "Apply the patches FILES.

(fn &optional FILES ARGS)" t nil) (autoload 'magit-am-apply-maildir "magit-sequence" "Apply the patches from MAILDIR.

(fn &optional MAILDIR ARGS)" t nil) (autoload 'magit-am-continue "magit-sequence" "Resume the current patch applying sequence." t nil) (autoload 'magit-am-skip "magit-sequence" "Skip the stopped at patch during a patch applying sequence." t nil) (autoload 'magit-am-abort "magit-sequence" "Abort the current patch applying sequence.
This discards all changes made since the sequence started." t nil) (autoload 'magit-rebase "magit-sequence" nil t) (autoload 'magit-rebase-onto-pushremote "magit-sequence" nil t) (autoload 'magit-rebase-onto-upstream "magit-sequence" nil t) (autoload 'magit-rebase-branch "magit-sequence" "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased.

(fn TARGET ARGS)" t nil) (autoload 'magit-rebase-subset "magit-sequence" "Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits.

(fn NEWBASE START ARGS)" t nil) (autoload 'magit-rebase-interactive "magit-sequence" "Start an interactive rebase sequence.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-autosquash "magit-sequence" "Combine squash and fixup commits with their intended targets.

(fn ARGS)" t nil) (autoload 'magit-rebase-edit-commit "magit-sequence" "Edit a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-reword-commit "magit-sequence" "Reword a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-remove-commit "magit-sequence" "Remove a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-continue "magit-sequence" "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is.

(fn &optional NOEDIT)" t nil) (autoload 'magit-rebase-skip "magit-sequence" "Skip the current commit and restart the current rebase operation." t nil) (autoload 'magit-rebase-edit "magit-sequence" "Edit the todo list of the current rebase operation." t nil) (autoload 'magit-rebase-abort "magit-sequence" "Abort the current rebase operation, restoring the original branch." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-sequence" '("magit-"))) (autoload 'magit-stash "magit-stash" nil t) (autoload 'magit-stash-both "magit-stash" "Create a stash of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-index "magit-stash" "Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect.

(fn MESSAGE)" t nil) (autoload 'magit-stash-worktree "magit-stash" "Create a stash of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-keep-index "magit-stash" "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-snapshot-both "magit-stash" "Create a snapshot of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-snapshot-index "magit-stash" "Create a snapshot of the index only.
Unstaged and untracked changes are not stashed." t nil) (autoload 'magit-snapshot-worktree "magit-stash" "Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-apply "magit-stash" "Apply a stash to the working tree.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index.

(fn STASH)" t nil) (autoload 'magit-stash-drop "magit-stash" "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.

(fn STASH)" t nil) (autoload 'magit-stash-clear "magit-stash" "Remove all stashes saved in REF's reflog by deleting REF.

(fn REF)" t nil) (autoload 'magit-stash-branch "magit-stash" "Create and checkout a new BRANCH from STASH.

(fn STASH BRANCH)" t nil) (autoload 'magit-stash-branch-here "magit-stash" "Create and checkout a new BRANCH and apply STASH.
The branch is created using `magit-branch-and-checkout', using the
current branch or `HEAD' as the start-point.

(fn STASH BRANCH)" t nil) (autoload 'magit-stash-format-patch "magit-stash" "Create a patch from STASH

(fn STASH)" t nil) (autoload 'magit-stash-list "magit-stash" "List all stashes in a buffer." t nil) (autoload 'magit-stash-show "magit-stash" "Show all diffs of a stash in a buffer.

(fn STASH &optional ARGS FILES)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-stash" '("magit-"))) (autoload 'magit-init "magit-status" "Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

(fn DIRECTORY)" t nil) (autoload 'magit-status "magit-status" "Show the status of the current Git repository in a buffer.

If the current directory isn't located within a Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `magit-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `magit-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `magit-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments.

(fn &optional DIRECTORY CACHE)" t nil) (defalias 'magit 'magit-status "An alias for `magit-status' for better discoverability.

Instead of invoking this alias for `magit-status' using
\"M-x magit RET\", you should bind a key to `magit-status'
and read the info node `(magit)Getting Started', which
also contains other useful hints.") (autoload 'magit-status-here "magit-status" "Like `magit-status' but with non-nil `magit-status-goto-file-position'." t nil) (autoload 'magit-status-setup-buffer "magit-status" "

(fn &optional DIRECTORY)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-status" '("magit-"))) (autoload 'magit-submodule "magit-submodule" nil t) (autoload 'magit-submodule-add "magit-submodule" nil t) (autoload 'magit-submodule-read-name-for-path "magit-submodule" "

(fn PATH &optional PREFER-SHORT)" nil nil) (autoload 'magit-submodule-register "magit-submodule" nil t) (autoload 'magit-submodule-populate "magit-submodule" nil t) (autoload 'magit-submodule-update "magit-submodule" nil t) (autoload 'magit-submodule-synchronize "magit-submodule" nil t) (autoload 'magit-submodule-unpopulate "magit-submodule" nil t) (autoload 'magit-submodule-remove "magit-submodule" "Unregister MODULES and remove their working directories.

For safety reasons, do not remove the gitdirs and if a module has
uncommitted changes, then do not remove it at all.  If a module's
gitdir is located inside the working directory, then move it into
the gitdir of the superproject first.

With the \"--force\" argument offer to remove dirty working
directories and with a prefix argument offer to delete gitdirs.
Both actions are very dangerous and have to be confirmed.  There
are additional safety precautions in place, so you might be able
to recover from making a mistake here, but don't count on it.

(fn MODULES ARGS TRASH-GITDIRS)" t nil) (autoload 'magit-insert-modules "magit-submodule" "Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-module-sections-nested' controls
whether they are wrapped in an additional section." nil nil) (autoload 'magit-insert-modules-overview "magit-submodule" "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash." nil nil) (autoload 'magit-insert-modules-unpulled-from-upstream "magit-submodule" "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpulled-from-pushremote "magit-submodule" "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpushed-to-upstream "magit-submodule" "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpushed-to-pushremote "magit-submodule" "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-list-submodules "magit-submodule" "Display a list of the current repository's submodules." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-submodule" '("magit-"))) (autoload 'magit-subtree "magit-subtree" nil t) (autoload 'magit-subtree-import "magit-subtree" nil t) (autoload 'magit-subtree-export "magit-subtree" nil t) (autoload 'magit-subtree-add "magit-subtree" "Add REF from REPOSITORY as a new subtree at PREFIX.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-add-commit "magit-subtree" "Add COMMIT as a new subtree at PREFIX.

(fn PREFIX COMMIT ARGS)" t nil) (autoload 'magit-subtree-merge "magit-subtree" "Merge COMMIT into the PREFIX subtree.

(fn PREFIX COMMIT ARGS)" t nil) (autoload 'magit-subtree-pull "magit-subtree" "Pull REF from REPOSITORY into the PREFIX subtree.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-push "magit-subtree" "Extract the history of the subtree PREFIX and push it to REF on REPOSITORY.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-split "magit-subtree" "Extract the history of the subtree PREFIX.

(fn PREFIX COMMIT ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-subtree" '("magit-"))) (autoload 'magit-tag "magit" nil t) (autoload 'magit-tag-create "magit-tag" "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

(git tag [--annotate] NAME REV)

(fn NAME REV &optional ARGS)" t nil) (autoload 'magit-tag-delete "magit-tag" "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

(git tag -d TAGS)

(fn TAGS)" t nil) (autoload 'magit-tag-prune "magit-tag" "Offer to delete tags missing locally from REMOTE, and vice versa.

(fn TAGS REMOTE-TAGS REMOTE)" t nil) (autoload 'magit-tag-release "magit-tag" "Create a release tag.

Assume that release tags match `magit-release-tag-regexp'.

First prompt for the name of the new tag using the highest
existing tag as initial input and leaving it to the user to
increment the desired part of the version string.

If `--annotate' is enabled, then prompt for the message of the
new tag.  Base the proposed tag message on the message of the
highest tag, provided that that contains the corresponding
version string and substituting the new version string for that.
Otherwise propose something like \"Foo-Bar 1.2.3\", given, for
example, a TAG \"v1.2.3\" and a repository located at something
like \"/path/to/foo-bar\".

(fn TAG MSG &optional ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-tag" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-transient" '("magit-"))) (autoload 'magit-emacs-Q-command "magit-utils" "Show a shell command that runs an uncustomized Emacs with only Magit loaded.
See info node `(magit)Debugging Tools' for more information." t nil) (autoload 'Info-follow-nearest-node--magit-gitman "magit-utils" "

(fn FN &optional FORK)" nil nil) (advice-add 'Info-follow-nearest-node :around 'Info-follow-nearest-node--magit-gitman) (autoload 'org-man-export--magit-gitman "magit-utils" "

(fn FN LINK DESCRIPTION FORMAT)" nil nil) (advice-add 'org-man-export :around 'org-man-export--magit-gitman) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-utils" '("magit-"))) (put 'magit-wip-after-save-mode 'globalized-minor-mode t) (defvar magit-wip-after-save-mode nil "Non-nil if Magit-Wip-After-Save mode is enabled.
See the `magit-wip-after-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-after-save-mode'.") (custom-autoload 'magit-wip-after-save-mode "magit-wip" nil) (autoload 'magit-wip-after-save-mode "magit-wip" "Toggle Magit-Wip-After-Save-Local mode in all buffers.
With prefix ARG, enable Magit-Wip-After-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-After-Save-Local mode is enabled in all buffers where
`magit-wip-after-save-local-mode-turn-on' would do it.
See `magit-wip-after-save-local-mode' for more information on Magit-Wip-After-Save-Local mode.

(fn &optional ARG)" t nil) (defvar magit-wip-after-apply-mode nil "Non-nil if Magit-Wip-After-Apply mode is enabled.
See the `magit-wip-after-apply-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-after-apply-mode "magit-wip" nil) (autoload 'magit-wip-after-apply-mode "magit-wip" "Commit to work-in-progress refs.

If called interactively, enable Magit-Wip-After-Apply mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index.

(fn &optional ARG)" t nil) (defvar magit-wip-before-change-mode nil "Non-nil if Magit-Wip-Before-Change mode is enabled.
See the `magit-wip-before-change-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-before-change-mode "magit-wip" nil) (autoload 'magit-wip-before-change-mode "magit-wip" "Commit to work-in-progress refs before certain destructive changes.

If called interactively, enable Magit-Wip-Before-Change mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed.

(fn &optional ARG)" t nil) (autoload 'magit-wip-commit-initial-backup "magit-wip" "Before saving, commit current file to a worktree wip ref.

The user has to add this function to `before-save-hook'.

Commit the current state of the visited file before saving the
current buffer to that file.  This backs up the same version of
the file as `backup-buffer' would, but stores the backup in the
worktree wip ref, which is also used by the various Magit Wip
modes, instead of in a backup file as `backup-buffer' would.

This function ignores the variables that affect `backup-buffer'
and can be used along-side that function, which is recommended
because this function only backs up files that are tracked in
a Git repository." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-wip" '("magit-"))) (autoload 'magit-worktree "magit-worktree" nil t) (autoload 'magit-worktree-checkout "magit-worktree" "Checkout BRANCH in a new worktree at PATH.

(fn PATH BRANCH)" t nil) (autoload 'magit-worktree-branch "magit-worktree" "Create a new BRANCH and check it out in a new worktree at PATH.

(fn PATH BRANCH START-POINT &optional FORCE)" t nil) (autoload 'magit-worktree-move "magit-worktree" "Move WORKTREE to PATH.

(fn WORKTREE PATH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-worktree" '("magit-"))) (provide 'magit-autoloads)) "git-gutter" ((git-gutter git-gutter-autoloads) (autoload 'git-gutter:linum-setup "git-gutter" "Setup for linum-mode." nil nil) (autoload 'git-gutter-mode "git-gutter" "Git-Gutter mode

If called interactively, enable Git-Gutter mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-git-gutter-mode 'globalized-minor-mode t) (defvar global-git-gutter-mode nil "Non-nil if Global Git-Gutter mode is enabled.
See the `global-git-gutter-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-gutter-mode'.") (custom-autoload 'global-git-gutter-mode "git-gutter" nil) (autoload 'global-git-gutter-mode "git-gutter" "Toggle Git-Gutter mode in all buffers.
With prefix ARG, enable Global Git-Gutter mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Git-Gutter mode is enabled in all buffers where
`git-gutter--turn-on' would do it.
See `git-gutter-mode' for more information on Git-Gutter mode.

(fn &optional ARG)" t nil) (autoload 'git-gutter "git-gutter" "Show diff information in gutter" t nil) (autoload 'git-gutter:toggle "git-gutter" "Toggle to show diff information." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-gutter" '("git-gutter"))) (provide 'git-gutter-autoloads)) "rich-minority" ((rich-minority-autoloads rich-minority) (autoload 'rm--mode-list-as-string-list "rich-minority" "Return `minor-mode-list' as a simple list of strings." nil nil) (defvar rich-minority-mode nil "Non-nil if Rich minority mode is enabled.
See the `rich-minority-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `rich-minority-mode'.") (custom-autoload 'rich-minority-mode "rich-minority" nil) (autoload 'rich-minority-mode "rich-minority" "Toggle Rich minority mode on or off.

If called interactively, enable Rich minority mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{rich-minority-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rich-minority" '("rm-"))) (provide 'rich-minority-autoloads)) "smart-mode-line" ((smart-mode-line smart-mode-line-autoloads smart-mode-line-respectful-theme smart-mode-line-dark-theme smart-mode-line-light-theme) (when load-file-name (let ((dir (file-name-as-directory (file-name-directory load-file-name)))) (add-to-list 'custom-theme-load-path dir) (when (file-directory-p (file-name-as-directory (concat dir "themes"))) (add-to-list 'custom-theme-load-path (file-name-as-directory (concat dir "themes")))))) (autoload 'sml/setup "smart-mode-line" "Setup the mode-line to be smart and sexy.

ARG is ignored. Just call this function in your init file, and
the mode-line will be setup.

(fn &optional ARG)" t nil) (defalias 'smart-mode-line-enable #'sml/setup) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-mode-line" '("sml/"))) (when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-mode-line-dark-theme" '("smart-mode-line-dark"))) (when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-mode-line-light-theme" '("smart-mode-line-light"))) (when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-mode-line-respectful-theme" '("smart-mode-line-respectful"))) (provide 'smart-mode-line-autoloads)) "multiple-cursors" ((mc-separate-operations multiple-cursors-pkg mc-mark-pop mc-mark-more mc-cycle-cursors multiple-cursors-autoloads multiple-cursors-core multiple-cursors rectangular-region-mode mc-edit-lines mc-hide-unmatched-lines-mode) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-cycle-cursors" '("mc/"))) (autoload 'mc/edit-lines "mc-edit-lines" "Add one cursor to each line of the active region.
Starts from mark and moves in straight down or up towards the
line point is on.

What is done with lines which are not long enough is governed by
`mc/edit-lines-empty-lines'.  The prefix argument ARG can be used
to override this.  If ARG is a symbol (when called from Lisp),
that symbol is used instead of `mc/edit-lines-empty-lines'.
Otherwise, if ARG negative, short lines will be ignored.  Any
other non-nil value will cause short lines to be padded.

(fn &optional ARG)" t nil) (autoload 'mc/edit-ends-of-lines "mc-edit-lines" "Add one cursor to the end of each line in the active region." t nil) (autoload 'mc/edit-beginnings-of-lines "mc-edit-lines" "Add one cursor to the beginning of each line in the active region." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-edit-lines" '("mc/edit-lines-empty-lines"))) (autoload 'mc-hide-unmatched-lines-mode "mc-hide-unmatched-lines-mode" "Minor mode when enabled hides all lines where no cursors (and
also hum/lines-to-expand below and above) To make use of this
mode press \"C-'\" while multiple-cursor-mode is active. You can
still edit lines while you are in mc-hide-unmatched-lines
mode. To leave this mode press <return> or \"C-g\"

If called interactively, enable Mc-Hide-Unmatched-Lines mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-hide-unmatched-lines-mode" '("hum/"))) (autoload 'mc/mark-next-like-this "mc-mark-more" "Find and mark the next part of the buffer matching the currently active region
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-next-like-this-word "mc-mark-more" "Find and mark the next part of the buffer matching the currently active region
If no region is active, mark the word at the point and find the next match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-next-word-like-this "mc-mark-more" "Find and mark the next word of the buffer matching the currently active region
The matching region must be a whole word to be a match
If no region is active, mark the symbol at the point and find the next match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-next-symbol-like-this "mc-mark-more" "Find and mark the next symbol of the buffer matching the currently active region
The matching region must be a whole symbol to be a match
If no region is active, mark the symbol at the point and find the next match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-previous-like-this "mc-mark-more" "Find and mark the previous part of the buffer matching the currently active region
If no region is active add a cursor on the previous line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-previous-like-this-word "mc-mark-more" "Find and mark the previous part of the buffer matching the currently active region
If no region is active, mark the word at the point and find the previous match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark previous.

(fn ARG)" t nil) (autoload 'mc/mark-previous-word-like-this "mc-mark-more" "Find and mark the previous part of the buffer matching the currently active region
The matching region must be a whole word to be a match
If no region is active add a cursor on the previous line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-previous-symbol-like-this "mc-mark-more" "Find and mark the previous part of the buffer matching the currently active region
The matching region must be a whole symbol to be a match
If no region is active add a cursor on the previous line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-next-lines "mc-mark-more" "

(fn ARG)" t nil) (autoload 'mc/mark-previous-lines "mc-mark-more" "

(fn ARG)" t nil) (autoload 'mc/unmark-next-like-this "mc-mark-more" "Deselect next part of the buffer matching the currently active region." t nil) (autoload 'mc/unmark-previous-like-this "mc-mark-more" "Deselect prev part of the buffer matching the currently active region." t nil) (autoload 'mc/skip-to-next-like-this "mc-mark-more" "Skip the current one and select the next part of the buffer matching the currently active region." t nil) (autoload 'mc/skip-to-previous-like-this "mc-mark-more" "Skip the current one and select the prev part of the buffer matching the currently active region." t nil) (autoload 'mc/mark-all-like-this "mc-mark-more" "Find and mark all the parts of the buffer matching the currently active region" t nil) (autoload 'mc/mark-all-words-like-this "mc-mark-more" nil t nil) (autoload 'mc/mark-all-symbols-like-this "mc-mark-more" nil t nil) (autoload 'mc/mark-all-in-region "mc-mark-more" "Find and mark all the parts in the region matching the given search

(fn BEG END &optional SEARCH)" t nil) (autoload 'mc/mark-all-in-region-regexp "mc-mark-more" "Find and mark all the parts in the region matching the given regexp.

(fn BEG END)" t nil) (autoload 'mc/mark-more-like-this-extended "mc-mark-more" "Like mark-more-like-this, but then lets you adjust with arrows key.
The adjustments work like this:

   <up>    Mark previous like this and set direction to 'up
   <down>  Mark next like this and set direction to 'down

If direction is 'up:

   <left>  Skip past the cursor furthest up
   <right> Remove the cursor furthest up

If direction is 'down:

   <left>  Remove the cursor furthest down
   <right> Skip past the cursor furthest down

The bindings for these commands can be changed. See `mc/mark-more-like-this-extended-keymap'." t nil) (autoload 'mc/mark-all-like-this-dwim "mc-mark-more" "Tries to guess what you want to mark all of.
Can be pressed multiple times to increase selection.

With prefix, it behaves the same as original `mc/mark-all-like-this'

(fn ARG)" t nil) (autoload 'mc/mark-all-dwim "mc-mark-more" "Tries even harder to guess what you want to mark all of.

If the region is active and spans multiple lines, it will behave
as if `mc/mark-all-in-region'. With the prefix ARG, it will call
`mc/edit-lines' instead.

If the region is inactive or on a single line, it will behave like
`mc/mark-all-like-this-dwim'.

(fn ARG)" t nil) (autoload 'mc/mark-all-like-this-in-defun "mc-mark-more" "Mark all like this in defun." t nil) (autoload 'mc/mark-all-words-like-this-in-defun "mc-mark-more" "Mark all words like this in defun." t nil) (autoload 'mc/mark-all-symbols-like-this-in-defun "mc-mark-more" "Mark all symbols like this in defun." t nil) (autoload 'mc/toggle-cursor-on-click "mc-mark-more" "Add a cursor where you click, or remove a fake cursor that is
already there.

(fn EVENT)" t nil) (defalias 'mc/add-cursor-on-click 'mc/toggle-cursor-on-click) (autoload 'mc/mark-sgml-tag-pair "mc-mark-more" "Mark the tag we're in and its pair for renaming." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-mark-more" '("mc--" "mc/"))) (autoload 'mc/mark-pop "mc-mark-pop" "Add a cursor at the current point, pop off mark ring and jump
to the popped mark." t nil) (autoload 'mc/insert-numbers "mc-separate-operations" "Insert increasing numbers for each cursor, starting at
`mc/insert-numbers-default' or ARG.

(fn ARG)" t nil) (autoload 'mc/insert-letters "mc-separate-operations" "Insert increasing letters for each cursor, starting at 0 or ARG.
     Where letter[0]=a letter[2]=c letter[26]=aa

(fn ARG)" t nil) (autoload 'mc/reverse-regions "mc-separate-operations" nil t nil) (autoload 'mc/sort-regions "mc-separate-operations" nil t nil) (autoload 'mc/vertical-align "mc-separate-operations" "Aligns all cursors vertically with a given CHARACTER to the one with the
highest column number (the rightest).
Might not behave as intended if more than one cursors are on the same line.

(fn CHARACTER)" t nil) (autoload 'mc/vertical-align-with-space "mc-separate-operations" "Aligns all cursors with whitespace like `mc/vertical-align' does" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-separate-operations" '("mc--" "mc/insert-numbers-default"))) (autoload 'multiple-cursors-mode "multiple-cursors-core" "Mode while multiple cursors are active.

If called interactively, enable Multiple-Cursors mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multiple-cursors-core" '("activate-cursor-for-undo" "deactivate-cursor-after-undo" "multiple-cursors-mode" "unsupported-cmd"))) (autoload 'set-rectangular-region-anchor "rectangular-region-mode" "Anchors the rectangular region at point.

Think of this one as `set-mark' except you're marking a rectangular region. It is
an exceedingly quick way of adding multiple cursors to multiple lines." t nil) (autoload 'rectangular-region-mode "rectangular-region-mode" "A mode for creating a rectangular region to edit

If called interactively, enable Rectangular-Region mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rectangular-region-mode" '("rectangular-region-mode" "rrm/"))) (provide 'multiple-cursors-autoloads)) "treemacs-projectile" ((treemacs-projectile treemacs-projectile-autoloads) (autoload 'treemacs-projectile "treemacs-projectile" "Add one of `projectile-known-projects' to the treemacs workspace.
With a prefix ARG was for the name of the project instead of using the name of
the project's root directory.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-projectile" '("treemacs-"))) (provide 'treemacs-projectile-autoloads)) "treemacs-magit" ((treemacs-magit-autoloads treemacs-magit) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-magit" '("treemacs-magit--"))) (provide 'treemacs-magit-autoloads)) "which-key" ((which-key which-key-autoloads) (defvar which-key-mode nil "Non-nil if Which-Key mode is enabled.
See the `which-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-mode'.") (custom-autoload 'which-key-mode "which-key" nil) (autoload 'which-key-mode "which-key" "Toggle which-key-mode.

If called interactively, enable Which-Key mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'which-key-setup-side-window-right "which-key" "Apply suggested settings for side-window that opens on right." t nil) (autoload 'which-key-setup-side-window-right-bottom "which-key" "Apply suggested settings for side-window that opens on right
if there is space and the bottom otherwise." t nil) (autoload 'which-key-setup-side-window-bottom "which-key" "Apply suggested settings for side-window that opens on
bottom." t nil) (autoload 'which-key-setup-minibuffer "which-key" "Apply suggested settings for minibuffer.
Do not use this setup if you use the paging commands. Instead use
`which-key-setup-side-window-bottom', which is nearly identical
but more functional." t nil) (autoload 'which-key-add-keymap-based-replacements "which-key" "Replace the description of KEY using REPLACEMENT in KEYMAP.
KEY should take a format suitable for use in
`kbd'. REPLACEMENT is the string to use to describe the
command associated with KEY in the KEYMAP. You may also use a
cons cell of the form (STRING . COMMAND) for each REPLACEMENT,
where STRING is the replacement string and COMMAND is a symbol
corresponding to the intended command to be replaced. In the
latter case, which-key will verify the intended command before
performing the replacement. COMMAND should be nil if the binding
corresponds to a key prefix. For example,

(which-key-add-keymap-based-replacements global-map
  \"C-x w\" \"Save as\")

and

(which-key-add-keymap-based-replacements global-map
  \"C-x w\" '(\"Save as\" . write-file))

both have the same effect for the \"C-x C-w\" key binding, but
the latter causes which-key to verify that the key sequence is
actually bound to write-file before performing the replacement.

(fn KEYMAP KEY REPLACEMENT &rest MORE)" nil nil) (autoload 'which-key-add-key-based-replacements "which-key" "Replace the description of KEY-SEQUENCE with REPLACEMENT.
KEY-SEQUENCE is a string suitable for use in `kbd'. REPLACEMENT
may either be a string, as in

(which-key-add-key-based-replacements \"C-x 1\" \"maximize\")

a cons of two strings as in

(which-key-add-key-based-replacements \"C-x 8\"
                                        '(\"unicode\" . \"Unicode keys\"))

or a function that takes a (KEY . BINDING) cons and returns a
replacement.

In the second case, the second string is used to provide a longer
name for the keys under a prefix.

MORE allows you to specifcy additional KEY REPLACEMENT pairs.  All
replacements are added to
`which-key-key-based-description-replacement-alist'.

(fn KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil) (autoload 'which-key-add-major-mode-key-based-replacements "which-key" "Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply.

(fn MODE KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil) (autoload 'which-key-reload-key-sequence "which-key" "Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'. If nil, KEY-SEQ defaults to
`which-key--current-key-list'. Any prefix arguments that were
used are reapplied to the new key sequence.

(fn &optional KEY-SEQ)" nil nil) (autoload 'which-key-show-standard-help "which-key" "Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'.

(fn &optional _)" t nil) (autoload 'which-key-show-next-page-no-cycle "which-key" "Show next page of keys unless on the last page, in which case
call `which-key-show-standard-help'." t nil) (autoload 'which-key-show-previous-page-no-cycle "which-key" "Show previous page of keys unless on the first page, in which
case do nothing." t nil) (autoload 'which-key-show-next-page-cycle "which-key" "Show the next page of keys, cycling from end to beginning
after last page.

(fn &optional _)" t nil) (autoload 'which-key-show-previous-page-cycle "which-key" "Show the previous page of keys, cycling from beginning to end
after first page.

(fn &optional _)" t nil) (autoload 'which-key-show-top-level "which-key" "Show top-level bindings.

(fn &optional _)" t nil) (autoload 'which-key-show-major-mode "which-key" "Show top-level bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. 

(fn &optional ALL)" t nil) (autoload 'which-key-show-full-major-mode "which-key" "Show all bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. " t nil) (autoload 'which-key-dump-bindings "which-key" "Dump bindings from PREFIX into buffer named BUFFER-NAME.

PREFIX should be a string suitable for `kbd'.

(fn PREFIX BUFFER-NAME)" t nil) (autoload 'which-key-undo-key "which-key" "Undo last keypress and force which-key update.

(fn &optional _)" t nil) (autoload 'which-key-C-h-dispatch "which-key" "Dispatch C-h commands by looking up key in
`which-key-C-h-map'. This command is always accessible (from any
prefix) if `which-key-use-C-h-commands' is non nil." t nil) (autoload 'which-key-show-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively from all available keymaps.

If NO-PAGING is non-nil, which-key will not intercept subsequent
keypresses for the paging functionality.

(fn KEYMAP &optional NO-PAGING)" t nil) (autoload 'which-key-show-full-keymap "which-key" "Show all bindings in KEYMAP using which-key. KEYMAP is
selected interactively from all available keymaps.

(fn KEYMAP)" t nil) (autoload 'which-key-show-minor-mode-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively by mode in `minor-mode-map-alist'.

(fn &optional ALL)" t nil) (autoload 'which-key-show-full-minor-mode-keymap "which-key" "Show all bindings in KEYMAP using which-key. KEYMAP
is selected interactively by mode in `minor-mode-map-alist'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "which-key" '("which-key-"))) (provide 'which-key-autoloads)) "free-keys" ((free-keys free-keys-autoloads) (autoload 'free-keys "free-keys" "Display free keys in current buffer.

A free key is a key that has no associated key-binding as
determined by function `key-binding'.

By default, keys on `free-keys-keys' list with no prefix sequence
are considered, possibly together with modifier keys from
`free-keys-modifiers'.  You can change the prefix sequence by
hitting 'p' in the *Free keys* buffer.  Prefix is supplied in
format recognized by `kbd', for example \"C-x\".

(fn &optional PREFIX BUFFER)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "free-keys" '("free-keys-"))) (provide 'free-keys-autoloads)) "winum" ((winum winum-autoloads) (defvar winum-mode nil "Non-nil if Winum mode is enabled.
See the `winum-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `winum-mode'.") (custom-autoload 'winum-mode "winum" nil) (autoload 'winum-mode "winum" "A minor mode that allows for managing windows based on window numbers.

If called interactively, enable Winum mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-0-or-10 "winum" "Jump to window 0 if assigned or 10 if exists.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-0 "winum" "Jump to window 0.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-1 "winum" "Jump to window 1.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-2 "winum" "Jump to window 2.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-3 "winum" "Jump to window 3.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-4 "winum" "Jump to window 4.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-5 "winum" "Jump to window 5.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-6 "winum" "Jump to window 6.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-7 "winum" "Jump to window 7.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-8 "winum" "Jump to window 8.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-9 "winum" "Jump to window 9.
If prefix ARG is given, delete the window instead of selecting it.

(fn &optional ARG)" t nil) (autoload 'winum-select-window-by-number "winum" "Select or delete window which number is specified by ARG.
If the number is negative, delete the window instead of selecting it.
There are several ways to provide the number:
- if called from elisp with an argument, use it.
- if called interactively with a numeric prefix argument, use it.
- if prefix argument is the negative argument, delete window 0.
- if prefix argument is the default prefix argument, delete current window.
- if called interactively and no valid argument is provided, read from
  minibuffer.

(fn &optional ARG)" t nil) (autoload 'winum-set-keymap-prefix "winum" "Set key bindings prefix for `winum-keymap' based on `winum-base-map'.
This function overrides the value of `winum-keymap', so you
should call it before customization of `winum-keymap' and/or
after customization of `winum-base-map'.
PREFIX must be a key sequence, like the ones returned by `kbd'.

(fn PREFIX)" nil nil) (autoload 'winum-get-window-by-number "winum" "Return window numbered N if exists, nil otherwise.

(fn N)" nil nil) (autoload 'winum-get-number-string "winum" "Get the current or specified window's current number as a propertized string.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned.

(fn &optional WINDOW)" nil nil) (autoload 'winum-get-number "winum" "Get the current or specified window's current number.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned.

(fn &optional WINDOW)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "winum" '("winum-"))) (provide 'winum-autoloads)) "beacon" ((beacon-autoloads beacon) (autoload 'beacon-blink "beacon" "Blink the beacon at the position of the cursor.
Unlike `beacon-blink-automated', the beacon will blink
unconditionally (even if `beacon-mode' is disabled), and this can
be invoked as a user command or called from lisp code." t nil) (defvar beacon-mode nil "Non-nil if Beacon mode is enabled.
See the `beacon-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `beacon-mode'.") (custom-autoload 'beacon-mode "beacon" nil) (autoload 'beacon-mode "beacon" "Toggle Beacon mode on or off.

If called interactively, enable Beacon mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\\{beacon-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "beacon" '("beacon-"))) (provide 'beacon-autoloads)) "dimmer" ((dimmer dimmer-autoloads) (autoload 'dimmer-configure-company-box "dimmer" "Convenience setting for company-box users.
This predicate prevents dimming the buffer you are editing when
company-box pops up a list of completion." nil nil) (autoload 'dimmer-configure-helm "dimmer" "Convenience settings for helm users." nil nil) (autoload 'dimmer-configure-gnus "dimmer" "Convenience settings for gnus users." nil nil) (autoload 'dimmer-configure-hydra "dimmer" "Convenience settings for hydra users." nil nil) (autoload 'dimmer-configure-magit "dimmer" "Convenience settings for magit users." nil nil) (autoload 'dimmer-configure-org "dimmer" "Convenience settings for org users." nil nil) (autoload 'dimmer-configure-posframe "dimmer" "Convenience settings for packages depending on posframe.

Note, packages that use posframe aren't required to be consistent
about how they name their buffers, but many of them tend to
include the words \"posframe\" and \"buffer\" in the buffer's
name.  Examples include:

  - \" *ivy-posframe-buffer*\"
  - \" *company-posframe-buffer*\"
  - \" *flycheck-posframe-buffer*\"
  - \" *ddskk-posframe-buffer*\"

If this setting doesn't work for you, you still have the option
of adding another regular expression to catch more things, or
in some cases you can customize the other package and ensure it
uses a buffer name that fits this pattern." nil nil) (autoload 'dimmer-configure-which-key "dimmer" "Convenience settings for which-key-users." nil nil) (defvar dimmer-mode nil "Non-nil if Dimmer mode is enabled.
See the `dimmer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dimmer-mode'.") (custom-autoload 'dimmer-mode "dimmer" nil) (autoload 'dimmer-mode "dimmer" "visually highlight the selected buffer

If called interactively, enable Dimmer mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'dimmer-activate 'dimmer-mode) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dimmer" '("dimmer-"))) (provide 'dimmer-autoloads)) "buffer-move" ((buffer-move buffer-move-autoloads) (autoload 'buf-move-up "buffer-move" "Swap the current buffer and the buffer above the split.
   If there is no split, ie now window above the current one, an
   error is signaled." t nil) (autoload 'buf-move-down "buffer-move" "Swap the current buffer and the buffer under the split.
   If there is no split, ie now window under the current one, an
   error is signaled." t nil) (autoload 'buf-move-left "buffer-move" "Swap the current buffer and the buffer on the left of the split.
   If there is no split, ie now window on the left of the current
   one, an error is signaled." t nil) (autoload 'buf-move-right "buffer-move" "Swap the current buffer and the buffer on the right of the split.
   If there is no split, ie now window on the right of the current
   one, an error is signaled." t nil) (autoload 'buf-move "buffer-move" "Begin moving the current buffer to different windows.

Use the arrow keys to move in the desired direction.  Pressing
any other key exits this function." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buffer-move" '("buf"))) (provide 'buffer-move-autoloads)) "yasnippet" ((yasnippet yasnippet-autoloads) (autoload 'yas-minor-mode "yasnippet" "Toggle YASnippet mode.

If called interactively, enable Yas minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{yas-minor-mode-map}

(fn &optional ARG)" t nil) (put 'yas-global-mode 'globalized-minor-mode t) (defvar yas-global-mode nil "Non-nil if Yas-Global mode is enabled.
See the `yas-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.") (custom-autoload 'yas-global-mode "yasnippet" nil) (autoload 'yas-global-mode "yasnippet" "Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas minor mode is enabled in all buffers where
`yas-minor-mode-on' would do it.
See `yas-minor-mode' for more information on Yas minor mode.

(fn &optional ARG)" t nil) (autoload 'snippet-mode "yasnippet" "A mode for editing yasnippets" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yasnippet" '("help-snippet-def" "snippet-mode-map" "yas"))) (provide 'yasnippet-autoloads)) "general" ((\.dirs-local general-autoloads general) (autoload 'general-define-key "general" "The primary key definition function provided by general.el.

Define MAPS, optionally using DEFINER, in the keymap(s) corresponding to STATES
and KEYMAPS.

MAPS consists of paired keys (vectors or strings; also see
`general-implicit-kbd') and definitions (those mentioned in `define-key''s
docstring and general.el's \"extended\" definitions). All pairs (when not
ignored) will be recorded and can be later displayed with
`general-describe-keybindings'.

If DEFINER is specified, a custom key definer will be used to bind MAPS. See
general.el's documentation/README for more information.

Unlike with normal key definitions functions, the keymaps in KEYMAPS should be
quoted (this allows using the keymap name for other purposes, e.g. deferring
keybindings if the keymap symbol is not bound, optionally inferring the
corresponding major mode for a symbol by removing \"-map\" for :which-key,
easily storing the keymap name for use with `general-describe-keybindings',
etc.). Note that general.el provides other key definer macros that do not
require quoting keymaps.

STATES corresponds to the evil state(s) to bind the keys in. Non-evil users
should not set STATES. When STATES is non-nil, `evil-define-key*' will be
used (the evil auxiliary keymaps corresponding STATES and KEYMAPS will be used);
otherwise `define-key' will be used (unless DEFINER is specified). KEYMAPS
defaults to 'global. There is also 'local, which create buffer-local
keybindings for both evil and non-evil keybindings. There are other special,
user-alterable \"shorthand\" symbols for keymaps and states (see
`general-keymap-aliases' and `general-state-aliases').

Note that STATES and KEYMAPS can either be lists or single symbols. If any
keymap does not exist, those keybindings will be deferred until the keymap does
exist, so using `eval-after-load' is not necessary with this function.

PREFIX corresponds to a key to prefix keys in MAPS with and defaults to none. To
bind/unbind a key specified with PREFIX, \"\" can be specified as a key in
MAPS (e.g. ...:prefix \"SPC\" \"\" nil... will unbind space).

The keywords in this paragraph are only useful for evil users. If
NON-NORMAL-PREFIX is specified, this prefix will be used instead of PREFIX for
states in `general-non-normal-states' (e.g. the emacs and insert states). This
argument will only have an effect if one of these states is in STATES or if
corresponding global keymap (e.g. `evil-insert-state-map') is in KEYMAPS.
Alternatively, GLOBAL-PREFIX can be used with PREFIX and/or NON-NORMAL-PREFIX to
bind keys in all states under the specified prefix. Like with NON-NORMAL-PREFIX,
GLOBAL-PREFIX will prevent PREFIX from applying to `general-non-normal-states'.
INFIX can be used to append a string to all of the specified prefixes. This is
potentially useful when you are using GLOBAL-PREFIX and/or NON-NORMAL-PREFIX so
that you can sandwich keys in between all the prefixes and the specified keys in
MAPS. This may be particularly useful if you are using default prefixes in a
wrapper function/macro so that you can add to them without needing to re-specify
all of them. If none of the other prefix keyword arguments are specified, INFIX
will have no effect.

If PREFIX-COMMAND or PREFIX-MAP is specified, a prefix command and/or keymap
will be created. PREFIX-NAME can be additionally specified to set the keymap
menu name/prompt. If PREFIX-COMMAND is specified, `define-prefix-command' will
be used. Otherwise, only a prefix keymap will be created. Previously created
prefix commands/keymaps will never be redefined/cleared. All prefixes (including
the INFIX key, if specified) will then be bound to PREFIX-COMMAND or PREFIX-MAP.
If the user did not specify any PREFIX or manually specify any KEYMAPS, general
will bind all MAPS in the prefix keymap corresponding to either PREFIX-MAP or
PREFIX-COMMAND instead of in the default keymap.

PREDICATE corresponds to a predicate to check to determine whether a definition
should be active (e.g. \":predicate '(eobp)\"). Definitions created with a
predicate will only be active when the predicate is true. When the predicate is
false, key lookup will continue to search for a match in lower-precedence
keymaps.

In addition to the normal definitions supported by `define-key', general.el also
provides \"extended\" definitions, which are plists containing the normal
definition as well as other keywords. For example, PREDICATE can be specified
globally or locally in an extended definition. New global (~general-define-key~)
and local (extended definition) keywords can be added by the user. See
`general-extended-def-keywords' and general.el's documentation/README for more
information.

PACKAGE is the global version of the extended definition keyword that specifies
the package a keymap is defined in (used for \"autoloading\" keymaps)

PROPERTIES, REPEAT, and JUMP are the global versions of the extended definition
keywords used for adding evil command properties to commands.

MAJOR-MODES, WK-MATCH-KEYS, WK-MATCH-BINDINGS, and WK-FULL-KEYS are the
corresponding global versions of which-key extended definition keywords. They
will only have an effect for extended definitions that specify :which-key or
:wk. See the section on extended definitions in the general.el
documentation/README for more information.

LISPY-PLIST and WORF-PLIST are the global versions of extended definition
keywords that are used for each corresponding custom DEFINER.

(fn &rest MAPS &key DEFINER (STATES general-default-states) (KEYMAPS general-default-keymaps KEYMAPS-SPECIFIED-P) (PREFIX general-default-prefix) (NON-NORMAL-PREFIX general-default-non-normal-prefix) (GLOBAL-PREFIX general-default-global-prefix) INFIX PREFIX-COMMAND PREFIX-MAP PREFIX-NAME PREDICATE PACKAGE PROPERTIES REPEAT JUMP MAJOR-MODES (WK-MATCH-KEYS t) (WK-MATCH-BINDING t) (WK-FULL-KEYS t) LISPY-PLIST WORF-PLIST &allow-other-keys)" nil nil) (autoload 'general-emacs-define-key "general" "A wrapper for `general-define-key' that is similar to `define-key'.
It has a positional argument for KEYMAPS (that will not be overridden by a later
:keymaps argument). Besides this, it acts the same as `general-define-key', and
ARGS can contain keyword arguments in addition to keybindings. This can
basically act as a drop-in replacement for `define-key', and unlike with
`general-define-key', KEYMAPS does not need to be quoted.

(fn KEYMAPS &rest ARGS)" nil t) (function-put 'general-emacs-define-key 'lisp-indent-function '1) (autoload 'general-evil-define-key "general" "A wrapper for `general-define-key' that is similar to `evil-define-key'.
It has positional arguments for STATES and KEYMAPS (that will not be overridden
by a later :keymaps or :states argument). Besides this, it acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings. This can basically act as a drop-in replacement for
`evil-define-key', and unlike with `general-define-key', KEYMAPS does not need
to be quoted.

(fn STATES KEYMAPS &rest ARGS)" nil t) (function-put 'general-evil-define-key 'lisp-indent-function '2) (autoload 'general-def "general" "General definer that takes a variable number of positional arguments in ARGS.
This macro will act as `general-define-key', `general-emacs-define-key', or
`general-evil-define-key' based on how many of the initial arguments do not
correspond to keybindings. All quoted and non-quoted lists and symbols before
the first string, vector, or keyword are considered to be positional arguments.
This means that you cannot use a function or variable for a key that starts
immediately after the positional arguments. If you need to do this, you should
use one of the definers that `general-def' dispatches to or explicitly separate
the positional arguments from the maps with a bogus keyword pair like
\":start-maps t\"

(fn &rest ARGS)" nil t) (function-put 'general-def 'lisp-indent-function 'defun) (autoload 'general-create-definer "general" "A helper macro to create wrappers for `general-def'.
This can be used to create key definers that will use a certain keymap, evil
state, prefix key, etc. by default. NAME is the wrapper name and DEFAULTS are
the default arguments. WRAPPING can also be optionally specified to use a
different definer than `general-def'. It should not be quoted.

(fn NAME &rest DEFAULTS &key WRAPPING &allow-other-keys)" nil t) (function-put 'general-create-definer 'lisp-indent-function 'defun) (autoload 'general-defs "general" "A wrapper that splits into multiple `general-def's.
Each consecutive grouping of positional argument followed by keyword/argument
pairs (having only one or the other is fine) marks the start of a new section.
Each section corresponds to one use of `general-def'. This means that settings
only apply to the keybindings that directly follow.

Since positional arguments can appear at any point, unqouted symbols are always
considered to be positional arguments (e.g. a keymap). This means that variables
can never be used for keys with `general-defs'. Variables can still be used for
definitions or as arguments to keywords.

(fn &rest ARGS)" nil t) (function-put 'general-defs 'lisp-indent-function 'defun) (autoload 'general-unbind "general" "A wrapper for `general-def' to unbind multiple keys simultaneously.
Insert after all keys in ARGS before passing ARGS to `general-def.' \":with
 #'func\" can optionally specified to use a custom function instead (e.g.
 `ignore').

(fn &rest ARGS)" nil t) (function-put 'general-unbind 'lisp-indent-function 'defun) (autoload 'general-describe-keybindings "general" "Show all keys that have been bound with general in an org buffer.
Any local keybindings will be shown first followed by global keybindings.
With a non-nil prefix ARG only show bindings in active maps.

(fn &optional ARG)" t nil) (autoload 'general-key "general" "Act as KEY's definition in the current context.
This uses an extended menu item's capability of dynamically computing a
definition. It is recommended over `general-simulate-key' wherever possible. See
the docstring of `general-simulate-key' and the readme for information about the
benefits and downsides of `general-key'.

KEY should be a string given in `kbd' notation and should correspond to a single
definition (as opposed to a sequence of commands). When STATE is specified, look
up KEY with STATE as the current evil state. When specified, DOCSTRING will be
the menu item's name/description.

Let can be used to bind variables around key lookup. For example:
(general-key \"some key\"
  :let ((some-var some-val)))

SETUP and TEARDOWN can be used to run certain functions before and after key
lookup. For example, something similar to using :state 'emacs would be:
(general-key \"some key\"
  :setup (evil-local-mode -1)
  :teardown (evil-local-mode))

ACCEPT-DEFAULT, NO-REMAP, and POSITION are passed to `key-binding'.

(fn KEY &key STATE DOCSTRING LET SETUP TEARDOWN ACCEPT-DEFAULT NO-REMAP POSITION)" nil t) (function-put 'general-key 'lisp-indent-function '1) (autoload 'general-simulate-keys "general" "Deprecated. Please use `general-simulate-key' instead.

(fn KEYS &optional STATE KEYMAP (LOOKUP t) DOCSTRING NAME)" nil t) (autoload 'general-simulate-key "general" "Create and return a command that simulates KEYS in STATE and KEYMAP.

`general-key' should be prefered over this whenever possible as it is simpler
and has saner functionality in many cases because it does not rely on
`unread-command-events' (e.g. \"C-h k\" will show the docstring of the command
to be simulated ; see the readme for more information). The main downsides of
`general-key' are that it cannot simulate a command followed by keys or
subsequent commands, and which-key does not currently work well with it when
simulating a prefix key/incomplete key sequence.

KEYS should be a string given in `kbd' notation. It can also be a list of a
single command followed by a string of the key(s) to simulate after calling that
command. STATE should only be specified by evil users and should be a quoted
evil state. KEYMAP should not be quoted. Both STATE and KEYMAP aliases are
supported (but they have to be set when the macro is expanded). When neither
STATE or KEYMAP are specified, the key(s) will be simulated in the current
context.

If NAME is specified, it will replace the automatically generated function name.
NAME should not be quoted. If DOCSTRING is specified, it will replace the
automatically generated docstring.

Normally the generated function will look up KEY in the correct context to try
to match a command. To prevent this lookup, LOOKUP can be specified as nil.
Generally, you will want to keep LOOKUP non-nil because this will allow checking
the evil repeat property of matched commands to determine whether or not they
should be recorded. See the docstring for `general--simulate-keys' for more
information about LOOKUP.

When a WHICH-KEY description is specified, it will replace the command name in
the which-key popup.

When a command name is specified and that command has been remapped (i.e. [remap
command] is currently bound), the remapped version will be used instead of the
original command unless REMAP is specified as nil (it is true by default).

The advantages of this over a keyboard macro are as follows:
- Prefix arguments are supported
- The user can control the context in which the keys are simulated
- The user can simulate both a named command and keys
- The user can simulate an incomplete key sequence (e.g. for a keymap)

(fn KEYS &key STATE KEYMAP NAME DOCSTRING (LOOKUP t) WHICH-KEY (REMAP t))" nil t) (function-put 'general-simulate-key 'lisp-indent-function 'defun) (autoload 'general-key-dispatch "general" "Create and return a command that runs FALLBACK-COMMAND or a command in MAPS.
MAPS consists of <key> <command> pairs. If a key in MAPS is matched, the
corresponding command will be run. Otherwise FALLBACK-COMMAND will be run with
the unmatched keys. So, for example, if \"ab\" was pressed, and \"ab\" is not
one of the key sequences from MAPS, the FALLBACK-COMMAND will be run followed by
the simulated keypresses of \"ab\". Prefix arguments will still work regardless
of which command is run. This is useful for binding under non-prefix keys. For
example, this can be used to redefine a sequence like \"cw\" or \"cow\" in evil
but still have \"c\" work as `evil-change'. If TIMEOUT is specified,
FALLBACK-COMMAND will also be run in the case that the user does not press the
next key within the TIMEOUT (e.g. 0.5).

NAME and DOCSTRING are optional keyword arguments. They can be used to replace
the automatically generated name and docstring for the created function. By
default, `cl-gensym' is used to prevent name clashes (e.g. allows the user to
create multiple different commands using `self-insert-command' as the
FALLBACK-COMMAND without explicitly specifying NAME to manually prevent
clashes).

When INHERIT-KEYMAP is specified, all the keybindings from that keymap will be
inherited in MAPS.

When a WHICH-KEY description is specified, it will replace the command name in
the which-key popup.

When command to be executed has been remapped (i.e. [remap command] is currently
bound), the remapped version will be used instead of the original command unless
REMAP is specified as nil (it is true by default).

(fn FALLBACK-COMMAND &rest MAPS &key TIMEOUT INHERIT-KEYMAP NAME DOCSTRING WHICH-KEY (REMAP t) &allow-other-keys)" nil t) (function-put 'general-key-dispatch 'lisp-indent-function '1) (autoload 'general-predicate-dispatch "general" "

(fn FALLBACK-DEF &rest DEFS &key DOCSTRING &allow-other-keys)" nil t) (function-put 'general-predicate-dispatch 'lisp-indent-function '1) (autoload 'general-translate-key "general" "Translate keys in the keymap(s) corresponding to STATES and KEYMAPS.
STATES should be the name of an evil state, a list of states, or nil. KEYMAPS
should be a symbol corresponding to the keymap to make the translations in or a
list of keymap names. Keymap and state aliases are supported (as well as 'local
and 'global for KEYMAPS).

MAPS corresponds to a list of translations (key replacement pairs). For example,
specifying \"a\" \"b\" will bind \"a\" to \"b\"'s definition in the keymap.
Specifying nil as a replacement will unbind a key.

If DESTRUCTIVE is non-nil, the keymap will be destructively altered without
creating a backup. If DESTRUCTIVE is nil, store a backup of the keymap on the
initial invocation, and for future invocations always look up keys in the
original/backup keymap. On the other hand, if DESTRUCTIVE is non-nil, calling
this function multiple times with \"a\" \"b\" \"b\" \"a\", for example, would
continue to swap and unswap the definitions of these keys. This means that when
DESTRUCTIVE is non-nil, all related swaps/cycles should be done in the same
invocation.

If both MAPS and DESCTRUCTIVE are nil, only create the backup keymap.

(fn STATES KEYMAPS &rest MAPS &key DESTRUCTIVE &allow-other-keys)" nil nil) (function-put 'general-translate-key 'lisp-indent-function 'defun) (autoload 'general-swap-key "general" "Wrapper around `general-translate-key' for swapping keys.
STATES, KEYMAPS, and ARGS are passed to `general-translate-key'. ARGS should
consist of key swaps (e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\" \"a\"
with `general-translate-key') and optionally keyword arguments for
`general-translate-key'.

(fn STATES KEYMAPS &rest ARGS)" nil t) (function-put 'general-swap-key 'lisp-indent-function 'defun) (autoload 'general-auto-unbind-keys "general" "Advise `define-key' to automatically unbind keys when necessary.
This will prevent errors when a sub-sequence of a key is already bound (e.g. the
user attempts to bind \"SPC a\" when \"SPC\" is bound, resulting in a \"Key
sequnce starts with non-prefix key\" error). When UNDO is non-nil, remove
advice.

(fn &optional UNDO)" nil nil) (autoload 'general-add-hook "general" "A drop-in replacement for `add-hook'.
Unlike `add-hook', HOOKS and FUNCTIONS can be single items or lists. APPEND and
LOCAL are passed directly to `add-hook'. When TRANSIENT is non-nil, each
function will remove itself from the hook it is in after it is run once. If
TRANSIENT is a function, call it on the return value in order to determine
whether to remove a function from the hook. For example, if TRANSIENT is
#'identity, remove each function only if it returns non-nil. TRANSIENT could
alternatively check something external and ignore the function's return value.

(fn HOOKS FUNCTIONS &optional APPEND LOCAL TRANSIENT)" nil nil) (autoload 'general-remove-hook "general" "A drop-in replacement for `remove-hook'.
Unlike `remove-hook', HOOKS and FUNCTIONS can be single items or lists. LOCAL is
passed directly to `remove-hook'.

(fn HOOKS FUNCTIONS &optional LOCAL)" nil nil) (autoload 'general-advice-add "general" "A drop-in replacement for `advice-add'.
SYMBOLS, WHERE, FUNCTIONS, and PROPS correspond to the arguments for
`advice-add'. Unlike `advice-add', SYMBOLS and FUNCTIONS can be single items or
lists. When TRANSIENT is non-nil, each function will remove itself as advice
after it is run once. If TRANSIENT is a function, call it on the return value in
order to determine whether to remove a function as advice. For example, if
TRANSIENT is #'identity, remove each function only if it returns non-nil.
TRANSIENT could alternatively check something external and ignore the function's
return value.

(fn SYMBOLS WHERE FUNCTIONS &optional PROPS TRANSIENT)" nil nil) (autoload 'general-add-advice "general") (autoload 'general-advice-remove "general" "A drop-in replacement for `advice-remove'.
Unlike `advice-remove', SYMBOLS and FUNCTIONS can be single items or lists.

(fn SYMBOLS FUNCTIONS)" nil nil) (autoload 'general-remove-advice "general") (autoload 'general-evil-setup "general" "Set up some basic equivalents for vim mapping functions.
This creates global key definition functions for the evil states.
Specifying SHORT-NAMES as non-nil will create non-prefixed function
aliases such as `nmap' for `general-nmap'.

(fn &optional SHORT-NAMES _)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "general" '("general-"))) (provide 'general-autoloads)) "dumb-jump" ((dumb-jump-autoloads dumb-jump) (defvar dumb-jump-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-M-g") 'dumb-jump-go) (define-key map (kbd "C-M-p") 'dumb-jump-back) (define-key map (kbd "C-M-q") 'dumb-jump-quick-look) map)) (autoload 'dumb-jump-back "dumb-jump" "Jump back to where the last jump was done." t nil) (autoload 'dumb-jump-quick-look "dumb-jump" "Run dumb-jump-go in quick look mode.  That is, show a tooltip of where it would jump instead." t nil) (autoload 'dumb-jump-go-other-window "dumb-jump" "Like 'dumb-jump-go' but use 'find-file-other-window' instead of 'find-file'." t nil) (autoload 'dumb-jump-go-current-window "dumb-jump" "Like dumb-jump-go but always use 'find-file'." t nil) (autoload 'dumb-jump-go-prefer-external "dumb-jump" "Like dumb-jump-go but prefer external matches from the current file." t nil) (autoload 'dumb-jump-go-prompt "dumb-jump" "Like dumb-jump-go but prompts for function instead of using under point" t nil) (autoload 'dumb-jump-go-prefer-external-other-window "dumb-jump" "Like dumb-jump-go-prefer-external but use 'find-file-other-window' instead of 'find-file'." t nil) (autoload 'dumb-jump-go "dumb-jump" "Go to the function/variable declaration for thing at point.
When USE-TOOLTIP is t a tooltip jump preview will show instead.
When PREFER-EXTERNAL is t it will sort external matches before
current file.

(fn &optional USE-TOOLTIP PREFER-EXTERNAL PROMPT)" t nil) (defvar dumb-jump-mode nil "Non-nil if Dumb-Jump mode is enabled.
See the `dumb-jump-mode' command
for a description of this minor mode.") (custom-autoload 'dumb-jump-mode "dumb-jump" nil) (autoload 'dumb-jump-mode "dumb-jump" "Minor mode for jumping to variable and function definitions

If called interactively, enable Dumb-Jump mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'dumb-jump-xref-activate "dumb-jump" "Function to activate xref backend.
Add this function to `xref-backend-functions' to dumb jump to be
activiated, whenever it finds a project. It is recommended to add
it to the end, so that it only gets activated when no better
option is found." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dumb-jump" '("dumb-jump-"))) (provide 'dumb-jump-autoloads)) "vterm" ((vterm-autoloads vterm) (autoload 'vterm-module-compile "vterm" "Compile vterm-module." t nil) (autoload 'vterm-next-error-function "vterm" "Advance to the next error message and visit the file where the error was.
This is the value of `next-error-function' in Compilation
buffers.  Prefix arg N says how many error messages to move
forwards (or backwards, if negative).

Optional argument RESET clears all the errors.

(fn N &optional RESET)" t nil) (autoload 'vterm "vterm" "Create a new vterm.

If called with an argument BUFFER-NAME, the name of the new buffer will
be set to BUFFER-NAME, otherwise it will be `vterm'

(fn &optional BUFFER-NAME)" t nil) (autoload 'vterm-other-window "vterm" "Create a new vterm in another window.

If called with an argument BUFFER-NAME, the name of the new buffer will
be set to BUFFER-NAME, otherwise it will be `vterm'

(fn &optional BUFFER-NAME)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vterm" '("vterm-"))) (provide 'vterm-autoloads)) "jsonrpc" ((jsonrpc-autoloads jsonrpc) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jsonrpc" '("jrpc-default-request-timeout" "jsonrpc-"))) (provide 'jsonrpc-autoloads)) "eldoc" ((eldoc eldoc-autoloads) (defvar eldoc-minor-mode-string (purecopy " ElDoc") "String to display in mode line when ElDoc Mode is enabled; nil for none.") (custom-autoload 'eldoc-minor-mode-string "eldoc" t) (autoload 'eldoc-mode "eldoc" "Toggle echo area display of Lisp objects at point (ElDoc mode).

If called interactively, enable Eldoc mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

ElDoc mode is a buffer-local minor mode.  When enabled, the echo
area displays information about a function or variable in the
text where point is.  If point is on a documented variable, it
displays the first line of that variable's doc string.  Otherwise
it displays the argument list of the function called in the
expression point is on.

(fn &optional ARG)" t nil) (put 'global-eldoc-mode 'globalized-minor-mode t) (defvar global-eldoc-mode t "Non-nil if Global Eldoc mode is enabled.
See the `global-eldoc-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-eldoc-mode'.") (custom-autoload 'global-eldoc-mode "eldoc" nil) (autoload 'global-eldoc-mode "eldoc" "Toggle Eldoc mode in all buffers.
With prefix ARG, enable Global Eldoc mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Eldoc mode is enabled in all buffers where
`turn-on-eldoc-mode' would do it.
See `eldoc-mode' for more information on Eldoc mode.

(fn &optional ARG)" t nil) (autoload 'turn-on-eldoc-mode "eldoc" "Turn on `eldoc-mode' if the buffer has ElDoc support enabled.
See `eldoc-documentation-strategy' for more detail." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eldoc" '("eldoc"))) (provide 'eldoc-autoloads)) "flymake" ((flymake flymake-autoloads) (autoload 'flymake-log "flymake" "Log, at level LEVEL, the message MSG formatted with ARGS.
LEVEL is passed to `display-warning', which is used to display
the warning.  If this form is included in a byte-compiled file,
the generated warning contains an indication of the file that
generated it.

(fn LEVEL MSG &rest ARGS)" nil t) (autoload 'flymake-make-diagnostic "flymake" "Make a Flymake diagnostic for BUFFER's region from BEG to END.
TYPE is a diagnostic symbol and TEXT is string describing the
problem detected in this region.  DATA is any object that the
caller wishes to attach to the created diagnostic for later
retrieval.

OVERLAY-PROPERTIES is an alist of properties attached to the
created diagnostic, overriding the default properties and any
properties of `flymake-overlay-control' of the diagnostic's
type.

(fn BUFFER BEG END TYPE TEXT &optional DATA OVERLAY-PROPERTIES)" nil nil) (autoload 'flymake-diagnostics "flymake" "Get Flymake diagnostics in region determined by BEG and END.

If neither BEG or END is supplied, use the whole buffer,
otherwise if BEG is non-nil and END is nil, consider only
diagnostics at BEG.

(fn &optional BEG END)" nil nil) (autoload 'flymake-diag-region "flymake" "Compute BUFFER's region (BEG . END) corresponding to LINE and COL.
If COL is nil, return a region just for LINE.  Return nil if the
region is invalid.

(fn BUFFER LINE &optional COL)" nil nil) (autoload 'flymake-mode "flymake" "Toggle Flymake mode on or off.

If called interactively, enable Flymake mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

Flymake is an Emacs minor mode for on-the-fly syntax checking.
Flymake collects diagnostic information from multiple sources,
called backends, and visually annotates the buffer with the
results.

Flymake performs these checks while the user is editing.
The customization variables `flymake-start-on-flymake-mode',
`flymake-no-changes-timeout' determine the exact circumstances
whereupon Flymake decides to initiate a check of the buffer.

The commands `flymake-goto-next-error' and
`flymake-goto-prev-error' can be used to navigate among Flymake
diagnostics annotated in the buffer.

The visual appearance of each type of diagnostic can be changed
by setting properties `flymake-overlay-control', `flymake-bitmap'
and `flymake-severity' on the symbols of diagnostic types (like
`:error', `:warning' and `:note').

Activation or deactivation of backends used by Flymake in each
buffer happens via the special hook
`flymake-diagnostic-functions'.

Some backends may take longer than others to respond or complete,
and some may decide to disable themselves if they are not
suitable for the current buffer.  The commands
`flymake-running-backends', `flymake-disabled-backends' and
`flymake-reporting-backends' summarize the situation, as does the
special *Flymake log* buffer.

(fn &optional ARG)" t nil) (autoload 'flymake-mode-on "flymake" "Turn Flymake mode on." nil nil) (autoload 'flymake-mode-off "flymake" "Turn Flymake mode off." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flymake" '("flymake-"))) (provide 'flymake-autoloads)) "xref" ((xref xref-autoloads) (autoload 'xref-find-backend "xref" nil nil nil) (autoload 'xref-pop-marker-stack "xref" "Pop back to where \\[xref-find-definitions] was last invoked." t nil) (autoload 'xref-marker-stack-empty-p "xref" "Return t if the marker stack is empty; nil otherwise." nil nil) (autoload 'xref-find-definitions "xref" "Find the definition of the identifier at point.
With prefix argument or when there's no identifier at point,
prompt for it.

If sufficient information is available to determine a unique
definition for IDENTIFIER, display it in the selected window.
Otherwise, display the list of the possible definitions in a
buffer where the user can select from the list.

(fn IDENTIFIER)" t nil) (autoload 'xref-find-definitions-other-window "xref" "Like `xref-find-definitions' but switch to the other window.

(fn IDENTIFIER)" t nil) (autoload 'xref-find-definitions-other-frame "xref" "Like `xref-find-definitions' but switch to the other frame.

(fn IDENTIFIER)" t nil) (autoload 'xref-find-references "xref" "Find references to the identifier at point.
This command might prompt for the identifier as needed, perhaps
offering the symbol at point as the default.
With prefix argument, or if `xref-prompt-for-identifier' is t,
always prompt for the identifier.  If `xref-prompt-for-identifier'
is nil, prompt only if there's no usable symbol at point.

(fn IDENTIFIER)" t nil) (autoload 'xref-find-definitions-at-mouse "xref" "Find the definition of identifier at or around mouse click.
This command is intended to be bound to a mouse event.

(fn EVENT)" t nil) (autoload 'xref-find-apropos "xref" "Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'.

(fn PATTERN)" t nil) (define-key esc-map "." #'xref-find-definitions) (define-key esc-map "," #'xref-pop-marker-stack) (define-key esc-map "?" #'xref-find-references) (define-key esc-map [67108910] #'xref-find-apropos) (define-key ctl-x-4-map "." #'xref-find-definitions-other-window) (define-key ctl-x-5-map "." #'xref-find-definitions-other-frame) (autoload 'xref-references-in-directory "xref" "Find all references to SYMBOL in directory DIR.
Return a list of xref values.

This function uses the Semantic Symbol Reference API, see
`semantic-symref-tool-alist' for details on which tools are used,
and when.

(fn SYMBOL DIR)" nil nil) (autoload 'xref-matches-in-directory "xref" "Find all matches for REGEXP in directory DIR.
Return a list of xref values.
Only files matching some of FILES and none of IGNORES are searched.
FILES is a string with glob patterns separated by spaces.
IGNORES is a list of glob patterns for files to ignore.

(fn REGEXP FILES DIR IGNORES)" nil nil) (autoload 'xref-matches-in-files "xref" "Find all matches for REGEXP in FILES.
Return a list of xref values.
FILES must be a list of absolute file names.

(fn REGEXP FILES)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xref" '("xref-"))) (provide 'xref-autoloads)) "project" ((project project-autoloads) (autoload 'project-current "project" "Return the project instance in DIRECTORY, defaulting to `default-directory'.

When no project is found in that directory, the result depends on
the value of MAYBE-PROMPT: if it is nil or omitted, return nil,
else ask the user for a directory in which to look for the
project, and if no project is found there, return a \"transient\"
project instance.

The \"transient\" project instance is a special kind of value
which denotes a project rooted in that directory and includes all
the files under the directory except for those that should be
ignored (per `project-ignores').

See the doc string of `project-find-functions' for the general form
of the project instance object.

(fn &optional MAYBE-PROMPT DIRECTORY)" nil nil) (defvar project-prefix-map (let ((map (make-sparse-keymap))) (define-key map "f" 'project-find-file) (define-key map "F" 'project-or-external-find-file) (define-key map "b" 'project-switch-to-buffer) (define-key map "s" 'project-shell) (define-key map "d" 'project-dired) (define-key map "v" 'project-vc-dir) (define-key map "c" 'project-compile) (define-key map "e" 'project-eshell) (define-key map "k" 'project-kill-buffers) (define-key map "p" 'project-switch-project) (define-key map "g" 'project-find-regexp) (define-key map "G" 'project-or-external-find-regexp) (define-key map "r" 'project-query-replace-regexp) map) "Keymap for project commands.") (define-key ctl-x-map "p" project-prefix-map) (autoload 'project-other-window-command "project" "Run project command, displaying resultant buffer in another window.

The following commands are available:

\\{project-prefix-map}
\\{project-other-window-map}" t nil) (define-key ctl-x-4-map "p" #'project-other-window-command) (autoload 'project-other-frame-command "project" "Run project command, displaying resultant buffer in another frame.

The following commands are available:

\\{project-prefix-map}
\\{project-other-frame-map}" t nil) (define-key ctl-x-5-map "p" #'project-other-frame-command) (autoload 'project-other-tab-command "project" "Run project command, displaying resultant buffer in a new tab.

The following commands are available:

\\{project-prefix-map}" t nil) (define-key tab-prefix-map "p" #'project-other-tab-command) (autoload 'project-find-regexp "project" "Find all matches for REGEXP in the current project's roots.
With \\[universal-argument] prefix, you can specify the directory
to search in, and the file name pattern to search for.  The
pattern may use abbreviations defined in `grep-files-aliases',
e.g. entering `ch' is equivalent to `*.[ch]'.  As whitespace
triggers completion when entering a pattern, including it
requires quoting, e.g. `\\[quoted-insert]<space>'.

(fn REGEXP)" t nil) (autoload 'project-or-external-find-regexp "project" "Find all matches for REGEXP in the project roots or external roots.
With \\[universal-argument] prefix, you can specify the file name
pattern to search for.

(fn REGEXP)" t nil) (autoload 'project-find-file "project" "Visit a file (with completion) in the current project.
The completion default is the filename at point, if one is
recognized." t nil) (autoload 'project-or-external-find-file "project" "Visit a file (with completion) in the current project or external roots.
The completion default is the filename at point, if one is
recognized." t nil) (autoload 'project-dired "project" "Start Dired in the current project's root." t nil) (autoload 'project-vc-dir "project" "Run VC-Dir in the current project's root." t nil) (autoload 'project-shell "project" "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists." t nil) (autoload 'project-eshell "project" "Start Eshell in the current project's root directory.
If a buffer already exists for running Eshell in the project's root,
switch to it.  Otherwise, create a new Eshell buffer.
With \\[universal-argument] prefix arg, create a new Eshell buffer even
if one already exists." t nil) (autoload 'project-search "project" "Search for REGEXP in all the files of the project.
Stops when a match is found.
To continue searching for the next match, use the
command \\[fileloop-continue].

(fn REGEXP)" t nil) (autoload 'project-query-replace-regexp "project" "Query-replace REGEXP in all the files of the project.
Stops when a match is found and prompts for whether to replace it.
If you exit the query-replace, you can later continue the query-replace
loop using the command \\[fileloop-continue].

(fn FROM TO)" t nil) (autoload 'project-compile "project" "Run `compile' in the project root.
Arguments the same as in `compile'.

(fn COMMAND &optional COMINT)" t nil) (autoload 'project-switch-to-buffer "project" "Display buffer BUFFER-OR-NAME in the selected window.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

(fn BUFFER-OR-NAME)" t nil) (autoload 'project-display-buffer "project" "Display BUFFER-OR-NAME in some window, without selecting it.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer' as a subroutine, which see
for how it is determined where the buffer will be displayed.

(fn BUFFER-OR-NAME)" t nil) (autoload 'project-display-buffer-other-frame "project" "Display BUFFER-OR-NAME preferably in another frame.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer-other-frame' as a subroutine,
which see for how it is determined where the buffer will be
displayed.

(fn BUFFER-OR-NAME)" t nil) (autoload 'project-kill-buffers "project" "Kill the buffers belonging to the current project.
Two buffers belong to the same project if their project
instances, as reported by `project-current' in each buffer, are
identical.  Only the buffers that match a condition in
`project-kill-buffer-conditions' will be killed.  If NO-CONFIRM
is non-nil, the command will not ask the user for confirmation.
NO-CONFIRM is always nil when the command is invoked
interactivly.

(fn &optional NO-CONFIRM)" t nil) (autoload 'project-remember-project "project" "Add project PR to the front of the project list.
Save the result in `project-list-file' if the list of projects has changed.

(fn PR)" nil nil) (autoload 'project-known-project-roots "project" "Return the list of root directories of all known projects." nil nil) (defvar project-switch-commands '((102 "Find file" project-find-file) (103 "Find regexp" project-find-regexp) (100 "Dired" project-dired) (118 "VC-Dir" project-vc-dir) (101 "Eshell" project-eshell)) "Alist mapping keys to project switching menu entries.
Used by `project-switch-project' to construct a dispatch menu of
commands available upon \"switching\" to another project.

Each element is of the form (KEY LABEL COMMAND), where COMMAND is the
command to run when KEY is pressed.  LABEL is used to distinguish
the menu entries in the dispatch menu.") (autoload 'project-switch-project "project" "\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "project" '("project-"))) (provide 'project-autoloads)) "eglot" ((eglot-autoloads eglot) (autoload 'eglot "eglot" "Manage a project with a Language Server Protocol (LSP) server.

The LSP server of CLASS started (or contacted) via CONTACT.  If
this operation is successful, current *and future* file buffers
of MANAGED-MAJOR-MODE inside PROJECT automatically become
\"managed\" by the LSP server, meaning information about their
contents is exchanged periodically to provide enhanced
code-analysis via `xref-find-definitions', `flymake-mode',
`eldoc-mode', `completion-at-point', among others.

Interactively, the command attempts to guess MANAGED-MAJOR-MODE
from current buffer, CLASS and CONTACT from
`eglot-server-programs' and PROJECT from `project-current'.  If
it can't guess, the user is prompted.  With a single
\\[universal-argument] prefix arg, it always prompt for COMMAND.
With two \\[universal-argument] prefix args, also prompts for
MANAGED-MAJOR-MODE.

PROJECT is a project instance as returned by `project-current'.

CLASS is a subclass of symbol `eglot-lsp-server'.

CONTACT specifies how to contact the server.  It is a
keyword-value plist used to initialize CLASS or a plain list as
described in `eglot-server-programs', which see.

INTERACTIVE is t if called interactively.

(fn MANAGED-MAJOR-MODE PROJECT CLASS CONTACT &optional INTERACTIVE)" t nil) (autoload 'eglot-ensure "eglot" "Start Eglot session for current buffer if there isn't one." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eglot" '("eglot-"))) (provide 'eglot-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 1 "melpa" nil "gnu-elpa-mirror" nil "emacsmirror-mirror" nil "straight" nil "evil" nil "undo-tree" nil "goto-chg" nil "cl-lib" nil "evil-surround" nil "evil-indent-textobject" nil "evil-org" nil "doom-themes" nil "org-roam" nil "dash" nil "f" nil "s" nil "org" (org :type git :repo "https://code.orgmode.org/bzg/org-mode.git" :local-repo "org") "emacsql" nil "emacsql-sqlite" nil "use-package" nil "bind-key" nil "diminish" nil "avy" nil "browse-kill-ring" nil "company" nil "company-box" nil "dash-functional" nil "frame-local" nil "dired-subtree" nil "dired-hacks-utils" nil "dired-narrow" nil "vscode-icon" nil "dired-sidebar" nil "diredfl" nil "dired-git-info" nil "elisp-slime-nav" nil "macrostep" nil "paredit" nil "highlight-parentheses" nil "eshell-git-prompt" nil "eyebrowse" nil "flycheck" nil "pkg-info" nil "epl" nil "let-alist" nil "seq" nil "evil-leader" nil "helm" nil "async" nil "popup" nil "helm-core" nil "helm-descbinds" nil "helm-swoop" nil "helm-projectile" nil "projectile" nil "helm-rg" nil "helm-xref" nil "helm-ls-git" nil "ivy" nil "swiper" nil "counsel" nil "smex" nil "hydra" nil "lv" nil "lsp-mode" nil "ht" nil "spinner" nil "markdown-mode" nil "ccls" nil "lsp-ui" nil "company-lsp" nil "dap-mode" nil "bui" nil "lsp-treemacs" nil "treemacs" nil "ace-window" nil "pfuture" nil "posframe" nil "magit" nil "git-commit" nil "transient" nil "with-editor" nil "git-gutter" nil "smart-mode-line" nil "rich-minority" nil "multiple-cursors" nil "flyspell" nil "treemacs-projectile" nil "treemacs-magit" nil "which-key" nil "free-keys" nil "winum" nil "windmove" nil "beacon" nil "dimmer" nil "buffer-move" nil "winner" nil "yasnippet" nil "general" nil "dumb-jump" nil "vterm" nil "eglot" nil "jsonrpc" nil "flymake" nil "eldoc" nil "project" nil "xref" nil)) melpa #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "emacsmirror-mirror" nil "straight" nil "evil" (evil :type git :flavor melpa :files (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el") "evil-pkg.el") :host github :repo "emacs-evil/evil") "undo-tree" nil "goto-chg" (goto-chg :type git :flavor melpa :host github :repo "emacs-evil/goto-chg") "cl-lib" nil "evil-surround" (evil-surround :type git :flavor melpa :host github :repo "emacs-evil/evil-surround") "evil-indent-textobject" (evil-indent-textobject :type git :flavor melpa :host github :repo "cofi/evil-indent-textobject") "evil-org" (evil-org :type git :flavor melpa :host github :repo "Somelauw/evil-org-mode") "doom-themes" (doom-themes :type git :flavor melpa :files (:defaults "themes/*.el" "doom-themes-pkg.el") :host github :repo "hlissner/emacs-doom-themes") "org-roam" (org-roam :type git :flavor melpa :host github :repo "org-roam/org-roam") "dash" (dash :type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el") "f" (f :type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el") "s" (s :type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el") "emacsql" (emacsql :type git :flavor melpa :files ("emacsql.el" "emacsql-compiler.el" "emacsql-system.el" "README.md" "emacsql-pkg.el") :host github :repo "skeeto/emacsql") "emacsql-sqlite" (emacsql-sqlite :type git :flavor melpa :files ("emacsql-sqlite.el" "sqlite" "emacsql-sqlite-pkg.el") :host github :repo "skeeto/emacsql") "use-package" (use-package :type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package") "bind-key" (bind-key :type git :flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :host github :repo "jwiegley/use-package") "diminish" (diminish :type git :flavor melpa :host github :repo "myrjola/diminish.el") "avy" (avy :type git :flavor melpa :host github :repo "abo-abo/avy") "browse-kill-ring" (browse-kill-ring :type git :flavor melpa :host github :repo "browse-kill-ring/browse-kill-ring") "company" (company :type git :flavor melpa :host github :repo "company-mode/company-mode") "company-box" (company-box :type git :flavor melpa :files (:defaults "images" "company-box-pkg.el") :host github :repo "sebastiencs/company-box") "dash-functional" (dash-functional :type git :flavor melpa :files ("dash-functional.el" "dash-functional-pkg.el") :host github :repo "magnars/dash.el") "frame-local" (frame-local :type git :flavor melpa :host github :repo "sebastiencs/frame-local") "dired-subtree" (dired-subtree :type git :flavor melpa :files ("dired-subtree.el" "dired-subtree-pkg.el") :host github :repo "Fuco1/dired-hacks") "dired-hacks-utils" (dired-hacks-utils :type git :flavor melpa :files ("dired-hacks-utils.el" "dired-hacks-utils-pkg.el") :host github :repo "Fuco1/dired-hacks") "dired-narrow" (dired-narrow :type git :flavor melpa :files ("dired-narrow.el" "dired-narrow-pkg.el") :host github :repo "Fuco1/dired-hacks") "vscode-icon" (vscode-icon :type git :flavor melpa :files (:defaults "icons" "vscode-icon-pkg.el") :host github :repo "jojojames/vscode-icon-emacs") "dired-sidebar" (dired-sidebar :type git :flavor melpa :host github :repo "jojojames/dired-sidebar") "diredfl" (diredfl :type git :flavor melpa :host github :repo "purcell/diredfl") "dired-git-info" nil "elisp-slime-nav" (elisp-slime-nav :type git :flavor melpa :host github :repo "purcell/elisp-slime-nav") "macrostep" (macrostep :type git :flavor melpa :host github :repo "joddie/macrostep") "paredit" (paredit :type git :flavor melpa :files ("paredit.el" "paredit-pkg.el") :repo "https://mumble.net/~campbell/git/paredit.git") "highlight-parentheses" (highlight-parentheses :type git :flavor melpa :host github :repo "tsdh/highlight-parentheses.el") "eshell-git-prompt" (eshell-git-prompt :type git :flavor melpa :host github :repo "xuchunyang/eshell-git-prompt") "eyebrowse" (eyebrowse :type git :flavor melpa :repo "https://depp.brause.cc/eyebrowse.git") "flycheck" (flycheck :type git :flavor melpa :host github :repo "flycheck/flycheck") "pkg-info" (pkg-info :type git :flavor melpa :host github :repo "emacsorphanage/pkg-info") "epl" (epl :type git :flavor melpa :host github :repo "cask/epl") "let-alist" nil "seq" nil "evil-leader" (evil-leader :type git :flavor melpa :host github :repo "cofi/evil-leader") "helm" (helm :type git :flavor melpa :files ("*.el" "emacs-helm.sh" (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") "helm-pkg.el") :host github :repo "emacs-helm/helm") "async" (async :type git :flavor melpa :host github :repo "jwiegley/emacs-async") "popup" (popup :type git :flavor melpa :files ("popup.el" "popup-pkg.el") :host github :repo "auto-complete/popup-el") "helm-core" (helm-core :type git :flavor melpa :files ("helm-core-pkg.el" "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") :host github :repo "emacs-helm/helm") "helm-descbinds" (helm-descbinds :type git :flavor melpa :host github :repo "emacs-helm/helm-descbinds") "helm-swoop" (helm-swoop :type git :flavor melpa :host github :repo "emacsorphanage/helm-swoop") "helm-projectile" (helm-projectile :type git :flavor melpa :host github :repo "bbatsov/helm-projectile") "projectile" (projectile :type git :flavor melpa :files ("projectile.el" "projectile-pkg.el") :host github :repo "bbatsov/projectile") "helm-rg" (helm-rg :type git :flavor melpa :host github :repo "cosmicexplorer/helm-rg") "helm-xref" (helm-xref :type git :flavor melpa :host github :repo "brotzeit/helm-xref") "helm-ls-git" (helm-ls-git :type git :flavor melpa :host github :repo "emacs-helm/helm-ls-git") "ivy" (ivy :type git :flavor melpa :files (:defaults (:exclude "swiper.el" "counsel.el" "ivy-hydra.el") "doc/ivy-help.org" "ivy-pkg.el") :host github :repo "abo-abo/swiper") "swiper" (swiper :type git :flavor melpa :files ("swiper.el" "swiper-pkg.el") :host github :repo "abo-abo/swiper") "counsel" (counsel :type git :flavor melpa :files ("counsel.el" "counsel-pkg.el") :host github :repo "abo-abo/swiper") "smex" (smex :type git :flavor melpa :host github :repo "nonsequitur/smex") "hydra" (hydra :type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra") "lv" (lv :type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra") "lsp-mode" (lsp-mode :type git :flavor melpa :host github :repo "emacs-lsp/lsp-mode") "ht" (ht :type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el") "spinner" nil "markdown-mode" (markdown-mode :type git :flavor melpa :host github :repo "jrblevin/markdown-mode") "ccls" (ccls :type git :flavor melpa :host github :repo "MaskRay/emacs-ccls") "lsp-ui" (lsp-ui :type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "lsp-ui-pkg.el") :host github :repo "emacs-lsp/lsp-ui") "company-lsp" (company-lsp :type git :flavor melpa :host github :repo "tigersoldier/company-lsp") "dap-mode" (dap-mode :type git :flavor melpa :files (:defaults "icons" "dap-mode-pkg.el") :host github :repo "emacs-lsp/dap-mode") "bui" (bui :type git :flavor melpa :host github :repo "alezost/bui.el") "lsp-treemacs" (lsp-treemacs :type git :flavor melpa :files (:defaults "icons" "lsp-treemacs-pkg.el") :host github :repo "emacs-lsp/lsp-treemacs") "treemacs" (treemacs :type git :flavor melpa :files (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el" "src/scripts/treemacs*.py" (:exclude "src/extra/*") "treemacs-pkg.el") :host github :repo "Alexander-Miller/treemacs") "ace-window" (ace-window :type git :flavor melpa :host github :repo "abo-abo/ace-window") "pfuture" (pfuture :type git :flavor melpa :host github :repo "Alexander-Miller/pfuture") "posframe" (posframe :type git :flavor melpa :host github :repo "tumashu/posframe") "magit" (magit :type git :flavor melpa :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "Documentation/magit.texi" "Documentation/AUTHORS.md" "LICENSE" (:exclude "lisp/magit-libgit.el") "magit-pkg.el") :host github :repo "magit/magit") "git-commit" (git-commit :type git :flavor melpa :files ("lisp/git-commit.el" "git-commit-pkg.el") :host github :repo "magit/magit") "transient" (transient :type git :flavor melpa :files ("lisp/*.el" "docs/transient.texi" "transient-pkg.el") :host github :repo "magit/transient") "with-editor" (with-editor :type git :flavor melpa :host github :repo "magit/with-editor") "git-gutter" (git-gutter :type git :flavor melpa :host github :repo "emacsorphanage/git-gutter") "smart-mode-line" (smart-mode-line :type git :flavor melpa :host github :repo "Malabarba/smart-mode-line") "rich-minority" (rich-minority :type git :flavor melpa :host github :repo "Malabarba/rich-minority") "multiple-cursors" (multiple-cursors :type git :flavor melpa :host github :repo "magnars/multiple-cursors.el") "flyspell" nil "treemacs-projectile" (treemacs-projectile :type git :flavor melpa :files ("src/extra/treemacs-projectile.el" "treemacs-projectile-pkg.el") :host github :repo "Alexander-Miller/treemacs") "treemacs-magit" (treemacs-magit :type git :flavor melpa :files ("src/extra/treemacs-magit.el" "treemacs-magit-pkg.el") :host github :repo "Alexander-Miller/treemacs") "which-key" (which-key :type git :flavor melpa :host github :repo "justbur/emacs-which-key") "free-keys" (free-keys :type git :flavor melpa :host github :repo "Fuco1/free-keys") "winum" (winum :type git :flavor melpa :host github :repo "deb0ch/emacs-winum") "windmove" nil "beacon" (beacon :type git :flavor melpa :host github :repo "Malabarba/beacon") "dimmer" (dimmer :type git :flavor melpa :host github :repo "gonewest818/dimmer.el") "buffer-move" (buffer-move :type git :flavor melpa :host github :repo "lukhas/buffer-move") "winner" nil "yasnippet" (yasnippet :type git :flavor melpa :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :host github :repo "joaotavora/yasnippet") "general" (general :type git :flavor melpa :host github :repo "noctuid/general.el") "dumb-jump" (dumb-jump :type git :flavor melpa :host github :repo "jacktasia/dumb-jump") "vterm" (vterm :type git :flavor melpa :files ("*" (:exclude ".dir-locals.el" ".gitignore" ".clang-format" ".travis.yml") "vterm-pkg.el") :host github :repo "akermu/emacs-libvterm") "eglot" (eglot :type git :flavor melpa :host github :repo "joaotavora/eglot") "jsonrpc" nil "flymake" nil "eldoc" nil "project" nil "xref" nil)) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "emacsmirror-mirror" nil "straight" nil "undo-tree" (undo-tree :type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git"))) "cl-lib" nil "dired-git-info" (dired-git-info :type git :host github :repo "emacs-straight/dired-git-info" :files ("*" (:exclude ".git"))) "let-alist" nil "seq" nil "spinner" (spinner :type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git"))) "flyspell" nil "windmove" nil "winner" nil "jsonrpc" nil "flymake" nil "eldoc" nil "project" nil "xref" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "cl-lib" nil "let-alist" (let-alist :type git :host github :repo "emacsmirror/let-alist") "seq" nil "flyspell" nil "windmove" nil "winner" nil "jsonrpc" (jsonrpc :type git :host github :repo "emacsmirror/jsonrpc") "flymake" (flymake :type git :host github :repo "emacsmirror/flymake") "eldoc" (eldoc :type git :host github :repo "emacsmirror/eldoc") "project" (project :type git :host github :repo "emacsmirror/project") "xref" (xref :type git :host github :repo "emacsmirror/xref")))))

("org-elpa" "melpa" "gnu-elpa-mirror" "emacsmirror-mirror" "straight" "emacs" "use-package" "bind-key" "diminish" "avy" "cl-lib" "browse-kill-ring" "company" "company-box" "dash" "dash-functional" "frame-local" "dired-subtree" "dired-hacks-utils" "dired-narrow" "vscode-icon" "dired-sidebar" "diredfl" "dired-git-info" "elisp-slime-nav" "macrostep" "paredit" "highlight-parentheses" "eshell-git-prompt" "eyebrowse" "flycheck" "pkg-info" "epl" "let-alist" "seq" "evil" "undo-tree" "goto-chg" "evil-leader" "evil-surround" "evil-indent-textobject" "helm" "async" "popup" "helm-core" "helm-descbinds" "helm-swoop" "helm-projectile" "projectile" "helm-rg" "helm-xref" "helm-ls-git" "ivy" "swiper" "counsel" "smex" "hydra" "lv" "ccls" "lsp-mode" "f" "s" "ht" "spinner" "markdown-mode" "lsp-ui" "company-lsp" "dap-mode" "bui" "lsp-treemacs" "treemacs" "ace-window" "pfuture" "posframe" "magit" "git-commit" "transient" "with-editor" "git-gutter" "smart-mode-line" "rich-minority" "multiple-cursors" "flyspell" "doom-themes" "treemacs-projectile" "treemacs-magit" "vterm" "which-key" "free-keys" "winum" "windmove" "beacon" "dimmer" "buffer-move" "winner" "yasnippet" "general" "dumb-jump")

t
