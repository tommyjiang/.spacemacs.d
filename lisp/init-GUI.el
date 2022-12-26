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

; 调整 epub mode(nov mode) 行高
(defun set-bigger-spacing ()
  (setq-local default-text-properties '(line-spacing 0.25 line-height 1.25)))
(add-hook 'nov-mode-hook 'set-bigger-spacing)

(custom-theme-set-faces
 'solarized-light
 ; org mode
 '(org-agenda-date
   ((t (:box nil))))
 '(org-agenda-date-today
   ((t (:foreground "#268bd2" :weight bold :box nil))))
 '(org-agenda-date-weekend
   ((t (:inherit org-agenda-date :foreground "#f47983" :weight bold))))
 '(org-agenda-structure
   ((t (:box nil))))
 '(org-agenda-calendar-event
   ((t (:foreground "#268bd2"))))
 '(org-block-begin-line
   ((t (:italic t :underline nil))))
 '(org-block-end-line
   ((t (:italic t :overline nil))))
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
         (calendar-weekend-header :foreground "#859900")
         (company-tooltip-scrollbar-thumb :background "#cb4b16")
         (company-tooltip-scrollbar-track :background "wheat")
         (org-agenda-date :box nil)
         (org-agenda-date-today :weight bold :box nil)
         (org-agenda-date-weekend :inherit org-agenda-date :weight bold :box nil)
         (org-agenda-structure :box nil)
         (org-agenda-calendar-event :foreground "#268bd2")
         (org-block-begin-line :italic t :underline nil)
         (org-block-end-line :italic t :overline nil)
         (org-link :foreground "#d33682")
         (org-verbatim :foreground "#d33682" :inherit fixed-pitch)
         (org-checkbox :foreground "#d33682" :box nil)
         (org-document-title :foreground "#93a1a1")
         (org-document-info :foreground "#93a1a1")
         (org-roam-header-line :foreground "#268bd2" :bold t)
         (org-pomodoro-mode-line :foreground "#cb4b16")
         (org-pomodoro-mode-line-break :foreground "#268bd2")
         ; eval
         (eval-sexp-fu-flash :background "#268bd2")
         ; company
         (company-tooltip-selection :foreground "#073642" :background "#268bd2")
         ; evil
         (evil-ex-substitute-replacement :foreground "#d33682" :underline t)
         ; helm
         (helm-buffer-directory :inherit default :foreground "#cb4b16")
         (helm-match :inherit default :foreground "#268bd2")
         (helm-ff-file-extension :inherit default :foreground "#cb4b16")
         ; info
         (info-double-quoted-name :foreground "#268bd2")
         )))

; 更新主题
(spacemacs/update-theme)

; hl-todo face
(setq hl-todo-keyword-faces
      '(("THEM" . "#dc8cc3")
        ("PROG" . "#7cb8bb")
        ("OKAY" . "#afd8af")
        ("DONT" . "#5f7f5f")
        ("FAIL" . "#cc5353")
        ("FURTHER" . "#cc5353")
        ("DONE" . "#afd8af")
        ("FIXME" . "#cc9393")
        ("XXX"   . "#cc5353")))

; company 中英文混排右对齐
(set-char-table-range glyphless-char-display #xfeff 'zero-width)

; 启动后全屏
(toggle-frame-maximized)

(provide 'init-GUI)
