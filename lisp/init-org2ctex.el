; 引入 org2ctex 宏包并启用
(require 'org2ctex)
(org2ctex-toggle t)

; 设置字体
(setq org2ctex-latex-fonts nil)

; 设置 ctex class 导出方法，使用 adobe 字体
(custom-set-variables '(org2ctex-latex-classes
  '(("ctexart"
     "\\documentclass[12pt, fontset=adobe, UTF8, a4paper, oneside]{ctexart}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ("ctexrep"
     "\\documentclass[fontset=adobe,UTF8,a4paper,zihao=-4]{ctexrep}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ("ctexbook"
     "\\documentclass[fontset=adobe,UTF8,a4paper,zihao=-4]{ctexbook}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ("beamer"
     "\\documentclass[presentation]{beamer}
\\usepackage[fontset=adobe,UTF8,a4paper,zihao=-4]{ctex}"
     ("\\section{%s}" . "\\section*{%s}")
	 ("\\subsection{%s}" . "\\subsection*{%s}")
	 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

; LaTeX 宏包设置
(custom-set-variables '(org2ctex-latex-packages-alist
  (list
   "
%%% 默认使用的latex宏包 %%%
\\usepackage{fancyhdr} % 设置页眉页脚宏包
\\usepackage{geometry} % 设置页边距宏包
\\usepackage{xcolor} % 颜色宏包
\\usepackage{enumitem} % 枚举设置宏包
\\usepackage{tikz} % 画图宏包
% 宏包设置
% 页眉页脚样式
\\pagestyle{fancy} % 页面样式采用fancyhdr宏包中的fancy
\\fancyhf{} % 去掉页眉
\\cfoot{\\thepage} % 页脚中间显示页码
\\renewcommand{\\headrulewidth}{0pt} % 去掉页眉的横线
% 页边距设置
\\geometry{top = 2.54cm, bottom = 2.54cm, left = 3.18cm, right = 3.18cm}
% 清华紫
\\definecolor{THU}{RGB}{111, 23, 135}
% 交叉引用宏包
\\hypersetup{colorlinks=true,linkcolor=THU,citecolor=THU}")))

(setq org-latex-toc-command
      '"{\n
\\hypersetup{linkcolor=black} % 目录链接为黑色
\\tableofcontents % 目录 \n} \n")

(provide 'init-org2ctex)
