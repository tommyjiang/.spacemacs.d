; 设置英文字体
(set-face-attribute
  'default nil :font "Inconsolata 24")

; 设置中文字体
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
  charset
  (font-spec :family "Heiti SC")))

; 启动后全屏
(toggle-frame-maximized)

(provide 'init-GUI)
