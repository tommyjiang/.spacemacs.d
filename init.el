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
     python
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     auto-completion
     bibtex
     (chrome :variables chrome-exec-path "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
     colors
     common-lisp
     (elfeed :variables rmh-elfeed-org-files (list "~/org/tommyfeed.org"))
     emacs-lisp
     (epub :variables nov-text-width 80)
     git
     gnus
     html
     latex
     markdown
     (mu4e :variables mu4e-installation-path "/usr/local/Cellar/mu/1.0_1/share/emacs/site-lisp/mu/mu4e")
     (org :variables org-want-todo-bindings t)
     osx
     spell-checking
     syntax-checking
     theming)
   ;; Additional layers
   dotspacemacs-additional-packages
   '(org2ctex
     bbdb)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages
   '(org-bullets
     org-projectile
     persp-mode
     evil-unimpaired
     google-translate
     auctex-latexmk
     window-purpose
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
   dotspacemacs-themes '(solarized-dark
                         solarized-light
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
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq configuration-layer--elpa-archives
    '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
      ("org-cn"   . "http://elpa.emacs-china.org/org/")
      ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))
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
  (require 'init-misc)
  (require 'init-org2ctex)

  (setq org-agenda-start-with-log-mode t) ; org agenda 显示 log
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
    (org2ctex helm all-the-icons yasnippet-snippets ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package treemacs-projectile treemacs-evil toc-org tagedit symon string-inflection spaceline-all-the-icons solarized-theme smeargle slime-company slim-mode scss-mode sass-mode reveal-in-osx-finder restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters pug-mode prettier-js pcre2el password-generator paradox overseer osx-trash osx-dictionary orgit org-ref org-present org-pomodoro org-mime org-download org-brain open-junk-file nov nameless mu4e-maildirs-extension mu4e-alert move-text monokai-theme mmm-mode markdown-toc magit-svn magit-gitflow lorem-ipsum link-hint launchctl indent-guide impatient-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org-rifle helm-mu helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag golden-ratio gnuplot gmail-message-mode gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy font-lock+ flyspell-correct-helm flymd flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies editorconfig edit-server dumb-jump dotenv-mode doom-modeline diminish counsel-projectile company-web company-statistics company-auctex common-lisp-snippets column-enforce-mode color-identifiers-mode clean-aindent-mode centered-cursor-mode bbdb auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(org2ctex-latex-classes
   (quote
    (("ctexart" "\\documentclass[12pt, fontset=adobe, UTF8, a4paper, oneside]{ctexart}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("ctexrep" "\\documentclass[fontset=adobe,UTF8,a4paper,zihao=-4]{ctexrep}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("ctexbook" "\\documentclass[fontset=adobe,UTF8,a4paper,zihao=-4]{ctexbook}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass[presentation]{beamer}
\\usepackage[fontset=adobe,UTF8,a4paper,zihao=-4]{ctex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org2ctex-latex-packages-alist
   (list "
%%% 默认使用的latex宏包 %%%
\\usepackage{fancyhdr} % 设置页眉页脚宏包
\\usepackage{geometry} % 设置页边距宏包
\\usepackage{xcolor} % 颜色宏包
\\usepackage{enumitem} % 枚举设置宏包
\\usepackage{tikz} % 画图宏包
% 宏包设置
% 页眉页脚样式
\\pagestyle{fancy} % 页面样式采用fancyhdr宏包中的fancy
\\fancyhf{} % 去掉页眉
\\cfoot{\\thepage} % 页脚中间显示页码
\\renewcommand{\\headrulewidth}{0pt} % 去掉页眉的横线
% 页边距设置
\\geometry{top = 2.54cm, bottom = 2.54cm, left = 3.18cm, right = 3.18cm}
% 清华紫
\\definecolor{THU}{RGB}{111, 23, 135}
% 交叉引用宏包
\\hypersetup{colorlinks=true,linkcolor=THU,citecolor=THU}"))
 '(package-selected-packages
   (quote
    (yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode company-anaconda anaconda-mode pythonic web-mode tagedit smeargle slime-company slime slim-mode scss-mode sass-mode reveal-in-osx-finder rainbow-mode rainbow-identifiers pug-mode pbcopy osx-trash osx-dictionary orgit org2ctex org-ref pdf-tools key-chord ivy tablist org-present org-pomodoro org-mime org-download mu4e-maildirs-extension mu4e-alert ht alert log4e gntp mmm-mode markdown-toc magit-gitflow magit-popup launchctl htmlize helm-gitignore helm-css-scss helm-company helm-c-yasnippet helm-bibtex parsebib haml-mode gnuplot gmail-message-mode ham-mode markdown-mode html-to-markdown gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-helm flyspell-correct flymd flycheck-pos-tip pos-tip flycheck evil-magit magit transient git-commit with-editor emmet-mode elfeed-web simple-httpd elfeed-org elfeed-goodies ace-jump-mode noflet elfeed edit-server company-web web-completion-data company-statistics company-auctex company common-lisp-snippets color-identifiers-mode biblio biblio-core bbdb auto-yasnippet yasnippet auto-dictionary auctex ac-ispell auto-complete solarized-theme ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-weekend-header ((t (:foreground "#859900"))))
 '(company-tooltip-selection ((t (:foreground "#073642" :background "#268bd2"))))
 '(eval-sexp-fu-flash ((t (:background "#268bd2"))))
 '(evil-ex-substitute-replacement ((t (:foreground "#d33682" :underline t))))
 '(helm-buffer-directory ((t (:inherit default :foreground "#cb4b16"))))
 '(helm-match ((t (:inherit default :foreground "#268bd2"))))
 '(helm-mu-contacts-name-face ((t (:inherit default))))
 '(info-double-quoted-name ((t (:foreground "#268bd2"))))
 '(mu4e-highlight-face ((t (:foreground "#268bd2"))))
 '(mu4e-modeline-face ((t (:inherit default :background "#eee8d5"))))
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))))
 '(spaceline-python-venv ((t ((quote mu4e-modeline-face))))))
