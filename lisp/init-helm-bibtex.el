; helm-bibtex settings
(setq bibtex-completion-bibliography '("~/CV-DL-OD-Interview/refs.bib"))

(defun list-dirs-recursively (dir &optional include-symlinks)
  "Return list of all subdirectories recursively. Returns absolute paths.
Optionally call recursively on symlinks."
  (let ((result nil)
        (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (file-name-all-completions "" dir))
      (when (and (directory-name-p file) (not (member file '("./" "../"))))
        (setq result (nconc result (list (expand-file-name file dir))))
        (let* ((leaf (substring file 0 (1- (length file))))
               (full-file (expand-file-name leaf dir)))
          ;; Don't follow symlinks to other directories.
          (unless (and (file-symlink-p full-file) (not include-symlinks))
            (setq result
                  (nconc result (list-dirs-recursively full-file)))))
        ))
    result))

(setq bibtex-completion-library-path (list-dirs-recursively "~/Downloads/Machine Learning"))

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

(setq bibtex-completion-cite-prompt-for-optional-arguments nil)

(provide 'init-helm-bibtex)
