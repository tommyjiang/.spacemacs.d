; magit
(setq magit-push-always-verify nil) ; magit 每次 push 不再询问
(setq magit-diff-refine-hunk (quote all))  ; 每行显示具体的 diff
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)  ; 保存后更新 magit-status

; powerline
(setq powerline-default-separator 'nil) ; 设置 powerline 分割线
(spaceline-compile) ; 更新 spaceline 设置

; ispell
(setq ispell-personal-dictionary "~/org/.aspell.en.pws") ; aspell dict 位置

; rainbow-mode
(add-hook 'prog-mode-hook 'rainbow-mode)

; markdown
(setq markdown-toc-header-toc-title "**目录**")

; python interpreter
(setq python-shell-interpreter 'python3)

(provide 'init-misc)
