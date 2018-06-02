; 设置英文字体
(set-face-attribute
  'default nil :font "Inconsolata 24")

; 设置中文字体
(if (display-graphic-p)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
    charset
    (font-spec :family "Hiragino Sans GB W3"))))

; Solarized
(setq solarized-scale-org-headlines nil)
(custom-theme-set-faces
 'solarized-light
 ; org mode
 '(org-agenda-date-today
   ((t (:foreground "#268db2" :box nil))))
 '(org-agenda-structure
   ((t (:box nil))))
 '(org-agenda-date
   ((t (:box nil))))
 '(org-verbatim
   ((t (:foreground "#d33682" :inherit fixed-pitch))))
 '(org-checkbox
   ((t (:foreground "#d33682" :box nil))))
 '(font-latex-sectioning-0-face
   ((t (:height 1.0))))
 '(font-latex-sectioning-1-face
   ((t (:height 1.0))))
 '(font-latex-sectioning-2-face
   ((t (:height 1.0))))
 '(font-latex-sectioning-3-face
   ((t (:height 1.0))))
 '(font-latex-sectioning-4-face
   ((t (:height 1.0))))
 '(font-latex-sectioning-5-face
   ((t (:height 1.0))))
 '(org-document-title
   ((t (:foreground "#93a1a1"))))
 '(org-document-info
   ((t (:foreground "#93a1a1"))))
 ; mu4e
 ; '(mu4e-highlight-face
 ;   ((t (:inherit default :foreground "#268db2"))))
 '(mu4e-modeline-face
   ((t (:inherit default :background ))))
 )

(setq theming-modifications '(
  (solarized-light (mu4e-highlight-face :foreground "#268db2")
                   ))
)

(spacemacs/update-theme)

; 启动后全屏
(toggle-frame-maximized)

(provide 'init-GUI)
