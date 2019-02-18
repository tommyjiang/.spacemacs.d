; 引入 org2ctex 宏包并启用
(require 'org2ctex)
(org2ctex-toggle t)

; 设置字体
(custom-set-variables '(org2ctex-latex-fonts
  '((mainfont "Adobe 宋体 Std")
    (CJKmainfont "Adobe 宋体 Std")
    (CJKmainfont-italic "Adobe 楷体 Std")
    (CJKsansfont "Adobe 黑体 Std")
    (CJKmonofont "Adobe 仿宋 Std"))))


(provide 'init-org2ctex)
