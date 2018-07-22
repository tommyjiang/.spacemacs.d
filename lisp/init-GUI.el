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

(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)

(custom-theme-set-faces
 'solarized-light
 ; org mode
 '(org-agenda-date
   ((t (:box nil))))
 '(org-agenda-date-today
   ((t (:foreground "#268bd2" :weight bold :box nil))))
 '(org-agenda-date-weekend
   ((t (:inherit org-agenda-date :foreground "#F47983" :weight bold))))
 '(org-agenda-structure
   ((t (:box nil))))
 '(org-agenda-calendar-event
   ((t (:foreground "#268bd2"))))
 '(org-verbatim
   ((t (:foreground "#d33682" :inherit fixed-pitch))))
 '(org-checkbox
   ((t (:foreground "#d33682" :box nil))))
 '(org-document-title
   ((t (:foreground "#93a1a1"))))
 '(org-document-info
   ((t (:foreground "#93a1a1"))))
)

(setq theming-modifications
      '((solarized-light
         (calendar-weekday-header :foreground "#cb4b16")
         (eval-sexp-fu-flash :background "#268bd2")
         ; company
         (company-tooltip-selection :foreground "#073642" :background "#268bd2")
         ; evil
         (evil-ex-substitute-replacement :foreground "#d33682" :underline t)
         ; helm
         (helm-buffer-directory :inherit default :foreground "#cb4b16")
         (helm-match :inherit default :foreground "#268bd2")
         ; helm-mu
         (helm-mu-contacts-name-face :inherit default)
         ; info
         (info-double-quoted-name :foreground "#268bd2")
         ; mu4e
         (mu4e-highlight-face :foreground "#268bd2")
         (mu4e-modeline-face :inherit default :background "#eee8d5")
         ; spaceline
         (spaceline-python-venv 'mu4e-modeline-face)
         )))

(spacemacs/update-theme)

; 启动后全屏
(toggle-frame-maximized)

(provide 'init-GUI)
