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
 )

; 启动后全屏
(toggle-frame-maximized)

(provide 'init-GUI)
