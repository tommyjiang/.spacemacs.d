; helm-bibtex settings
(setq bibtex-completion-bibliography '("~/org/paper.bib"
                                       "~/org/tutorial.bib"))
(setq bibtex-completion-library-path '("~/Documents/Library.papers3/Library.papers3/Files" 
                                       "~/Downloads/Machine Learning/Books/General" 
                                       "~/Downloads/Machine Learning/Tutorials"))

; Org export
(setq org-latex-pdf-process (list "latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

; Replaced version of open pdf
(defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (car (bibtex-completion-find-pdf key))))
      (if (file-exists-p pdf-file)
          (org-open-file pdf-file)
        (message "No PDF found for %s" key))))

(advice-add 'org-ref-open-pdf-at-point :override #'my/org-ref-open-pdf-at-point)

(provide 'init-helm-bibtex)
