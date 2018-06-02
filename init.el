;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(html
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     auto-completion
     bbdb
     bibtex
     common-lisp
     emacs-lisp
     (elfeed :variables rmh-elfeed-org-files (list "~/.spacemacs.d/tommyfeed.org"))
     git
     gnus
     latex
     markdown
     (mu4e :variables mu4e-installation-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e")
     (org :variables org-want-todo-bindings t)
     osx
     spell-checking
     syntax-checking)
   ;; Additional layers
   ;; dotspacemacs-additional-packages '(org2ctex)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages
   '(org-bullets
     org-projectile
     persp-mode
     evil-unimpaired
     google-translate
     auctex-latexmk
     leuven-theme)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   ; dotspacemacs-delete-orphan-packages t
   ))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; https connection
   dotspacemacs-elpa-https nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'official
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         spacemacs-light
                         spacemacs-dark
                         monokai)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '(;"Source Code Pro"
                               "Inconsolata"
                               :size 24
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 10
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
   ;; User initialization goes here
   (setq solarized-scale-org-headlines nil)
   (setq solarized-use-variable-pitch nil)
)

(defun dotspacemacs/user-init ()
  (setq ispell-program-name "/usr/local/bin/aspell")
)

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (add-to-list 'load-path "~/.spacemacs.d/lisp") ; 添加初始化文件路径
  (require 'init-GUI)
  (require 'init-cal)
  (require 'init-bbdb)
  (require 'init-evil)
  (require 'init-mu4e)
  (require 'init-org)
  (require 'init-orgTommy)
  (require 'init-auctex)
  (require 'init-helm-bibtex)

  (setq magit-push-always-verify nil) ; magit 每次 push 不再询问
  (setq powerline-default-separator 'nil) ; 设置 powerline 分割线
  (setq purpose-mode nil)
  (spaceline-compile) ; 更新 spaceline 设置
  (setq ispell-personal-dictionary "~/org/.aspell.en.pws") ; aspell dict 位置
  (setq org-html-head-extra nil) ; 导出 html 时采用自定义 CSS

  (org-agenda nil " ") ; 启动后显示 org agenda
)
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (solarized-theme restart-emacs helm magit slime imenu-list which-key yasnippet-snippets ws-butler winum web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit symon string-inflection spaceline-all-the-icons smeargle slime-company slim-mode scss-mode sass-mode reveal-in-osx-finder rainbow-delimiters pug-mode pcre2el pbcopy password-generator paradox overseer osx-trash osx-dictionary orgit org-ref org-present org-pomodoro org-mime org-download org-brain open-junk-file neotree nameless mu4e-maildirs-extension mu4e-alert move-text monokai-theme mmm-mode markdown-toc magit-svn magit-gitflow macrostep lorem-ipsum link-hint launchctl indent-guide impatient-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-mu helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-commit ghub gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies editorconfig dumb-jump diminish counsel-projectile company-web company-statistics company-auctex common-lisp-snippets column-enforce-mode clean-aindent-mode centered-cursor-mode bbdb auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
