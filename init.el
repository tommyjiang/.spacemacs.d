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
   '(
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     auto-completion
     bibtex
     (chrome :variables chrome-exec-path "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
     colors
     (elfeed :variables rmh-elfeed-org-files (list "~/org/tommyfeed.org"))
     emacs-lisp
     epub
     git
     html
     latex
     markdown
     (org :variables org-want-todo-bindings t)
     spell-checking
     syntax-checking
     theming)
   ;; Additional layers
   dotspacemacs-additional-packages
   '(bbdb
     websocket
     solarized-theme
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages
   '(org-bullets
     org-superstar
     org-projectile
     org-rich-yank
     evil-unimpaired
     forge
     google-translate
     auctex-latexmk
     window-purpose
     evil-lisp-state
     smartparens
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
   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         spacemacs-light
                         spacemacs-dark
                         monokai)
   dotspacemacs-mode-line-theme '(spacemacs :separator-scale 1.5)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Inconsolata"
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
   dotspacemacs-line-numbers '(:enabled-for-modes prog-mode
                               :relative t)
   )
   ;; User initialization goes here
   (setq solarized-scale-org-headlines nil)
   (setq solarized-use-variable-pitch nil)
)

(defun dotspacemacs/user-init ()
  ; 忽略 cl 警告
  (setq byte-compile-warnings '(cl-functions))
  ; Org capture 自动转为 insert state
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ; ispell bin 路径和字典
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-dictionary "american")

  ; melpa 源地址
  ;(setq configuration-layer-elpa-archives
  ;  '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ;    ("nongnu"   . "https://elpa.nongnu.org/nongnu/")
  ;    ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;    ))
)

(defun dotspacemacs/user-config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."
  (add-to-list 'load-path "~/.spacemacs.d/lisp") ; 添加初始化文件路径
  (load-theme 'solarized-light t)
  (require 'init-GUI)
  (require 'init-cal)
  (require 'init-bbdb)
  (require 'init-evil)
  (require 'init-org)
  (require 'init-orgTommy)
  (require 'init-auctex)
  (require 'init-helm-bibtex)
  (require 'init-misc)
  (require 'init-topspace)

  (require 'helm-ls-git)

  (setenv "PATH" (concat (getenv "PATH") "/usr/local/texlive/2022/bin/x86_64-darwin/"))
  (setq exec-path (append exec-path '("/usr/local/texlive/2022/bin/x86_64-darwin/")))

  (setq org-agenda-start-with-log-mode nil) ; org agenda 不显示 log
  (setq debug-on-error t)
  (setq package-check-signature nil)
  (org-agenda nil ",") ; 启动后显示 org agenda
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
   '(yasnippet-snippets ws-butler writeroom-mode winum which-key websocket web-mode web-beautify volatile-highlights vim-powerline vi-tilde-fringe uuidgen use-package undo-tree treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toc-org term-cursor tagedit symon symbol-overlay string-edit-at-point spacemacs-whitespace-cleanup spacemacs-purpose-popwin spaceline-all-the-icons space-doc solarized-theme smeargle slim-mode scss-mode sass-mode restart-emacs request rainbow-mode rainbow-identifiers rainbow-delimiters quickrun pug-mode prettier-js pcre2el password-generator paradox overseer orgit org-rich-yank org-ref org-present org-pomodoro org-mime org-download org-contrib org-cliplink open-junk-file nov nameless multi-line monokai-theme mmm-mode markdown-toc macrostep lorem-ipsum inspector info+ indent-guide impatient-mode hybrid-mode hungry-delete holy-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-ls-git helm-git-grep helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-bibtex helm-ag golden-ratio gnuplot gmail-message-mode gitignore-templates git-timemachine git-modes git-messenger git-link gh-md fuzzy font-lock+ flyspell-correct-helm flymd flycheck-pos-tip flycheck-package flycheck-elsa flx-ido fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-tutor evil-textobj-line evil-tex evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-evilified-state evil-escape evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu emr emmet-mode elisp-slime-nav elisp-def elfeed-org elfeed-goodies editorconfig edit-server dumb-jump drag-stuff dotenv-mode dired-quick-sort diminish devdocs define-word company-web company-reftex company-math company-auctex column-enforce-mode color-identifiers-mode clean-aindent-mode centered-cursor-mode bind-map bbdb auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ace-link ace-jump-helm-line ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "nil" :slant normal :weight normal :height 241 :width normal))))
 '(calendar-weekend-header ((t (:foreground "#859900"))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#cb4b16"))))
 '(company-tooltip-scrollbar-track ((t (:background "wheat"))))
 '(company-tooltip-selection ((t (:foreground "#073642" :background "#268bd2"))))
 '(eval-sexp-fu-flash ((t (:background "#268bd2"))))
 '(evil-ex-substitute-replacement ((t (:foreground "#d33682" :underline t))))
 '(helm-buffer-directory ((t (:inherit default :foreground "#cb4b16"))))
 '(helm-ff-file-extension ((t (:inherit default :foreground "#cb4b16"))))
 '(helm-match ((t (:inherit default :foreground "#268bd2"))))
 '(info-double-quoted-name ((t (:foreground "#268bd2"))))
 '(org-agenda-calendar-event ((t (:foreground "#268bd2"))))
 '(org-agenda-date ((t (:box nil))))
 '(org-agenda-date-today ((t (:weight bold :box nil))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :weight bold :box nil))))
 '(org-agenda-structure ((t (:box nil))))
 '(org-block-begin-line ((t (:italic t :underline nil))))
 '(org-block-end-line ((t (:italic t :overline nil))))
 '(org-checkbox ((t (:foreground "#d33682" :box nil))))
 '(org-document-info ((t (:foreground "#93a1a1"))))
 '(org-document-title ((t (:foreground "#93a1a1"))))
 '(org-link ((t (:foreground "#d33682"))))
 '(org-pomodoro-mode-line ((t (:foreground "#cb4b16"))))
 '(org-pomodoro-mode-line-break ((t (:foreground "#268bd2"))))
 '(org-verbatim ((t (:foreground "#d33682" :inherit fixed-pitch)))))
)
