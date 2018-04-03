; LaTeX mode 采用 AUCTeX
(mapc (lambda (mode)
         (add-hook 'LaTeX-mode-hook mode))
         (list 'auto-fill-mode
               'LaTeX-math-mode
               'turn-on-reftex
               'linum-mode))

; AUCTeX 设置
(add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq TeX-auto-untabify t     ;
                      TeX-engine 'xetex       ; 默认采用 xetex
                      TeX-show-compilation nil) ; 不显示编译信息
                (TeX-global-PDF-mode t)       ; 打开 PDF 模式
                (setq TeX-save-query nil)
                (imenu-add-menubar-index)
                (setq TeX-command-default "LaTeX")
                ; (setq TeX-master "master")
                (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)))

; Mint 命令
(add-hook 'LaTeX-mode-hook
  (lambda ()
    (add-to-list 'TeX-command-list
      '("Mint" "%`xelatex -shell-escape %(mode)%' %t" TeX-run-TeX nil t))))

; latex 命令
(add-hook 'LaTeX-mode-hook
  (lambda ()
    (add-to-list 'TeX-command-list
      '("mk" "latexmk -pdf -xelatex %(mode)%' %t" TeX-run-TeX nil t))))

; 双向搜索
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method-active 'synctex)
(setq TeX-source-correlate-start-server t)
(setq TeX-view-program-selection '((output-pdf "Skim")))
(setq TeX-view-program-list
    '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o")))
(server-start)

; 不放大缩小字号
(setq font-latex-fontify-sectioning 'color) ; do not fontify section title
(setq font-latex-fontify-script nil) ; do not fontify script

(provide 'init-auctex)
