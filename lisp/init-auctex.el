; LaTeX mode hook
(mapc (lambda (mode)
         (add-hook 'LaTeX-mode-hook mode))
         (list 'auto-fill-mode
               'LaTeX-math-mode
               'turn-on-reftex
               'linum-mode))

(add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq TeX-auto-untabify t     ; remove all tabs before saving
                      TeX-engine 'xetex       ; use xelatex default
                      TeX-show-compilation nil) ; display compilation windows
                (TeX-global-PDF-mode t)       ; enable PDF mode
                (setq TeX-save-query nil)
                (imenu-add-menubar-index)
                (setq TeX-command-default "LaTeX")
                (setq TeX-master "master")
                (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)))

; Disable auto newline in LaTeX mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (set-fill-column 99999)))

(add-hook 'LaTeX-mode-hook
  (lambda ()
    (add-to-list 'TeX-command-list 
      '("Mint" "%`xelatex -shell-escape %(mode)%' %t" TeX-run-TeX nil t))))

(add-hook 'LaTeX-mode-hook
  (lambda ()
    (add-to-list 'TeX-command-list 
      '("mk" "latexmk -pdf -xelatex %(mode)%' %t" TeX-run-TeX nil t))))

; double search
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method-active 'synctex)
(setq TeX-source-correlate-start-server t)
(setq TeX-view-program-selection '((output-pdf "Skim")))
(setq TeX-view-program-list 
    '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o")))
(server-start)

; normal font size
(setq font-latex-fontify-sectioning 1.0) ; do not fontify section title
(setq font-latex-fontify-script nil) ; do not fontify script

(provide 'init-auctex)
