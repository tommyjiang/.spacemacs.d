; 设置一些 mode 中使用 emacs mode
(evil-set-initial-state 'calendar-mode 'emacs)
; (evil-set-initial-state 'inferior-ess-mode 'emacs)
; (evil-set-initial-state 'ess-watch-mode 'emacs)
(evil-set-initial-state 'text-mode 'insert)
(evil-set-initial-state 'magit-status-mode 'emacs)

; Org capture 自动转为 insert state 
(add-hook 'org-capture-mode-hook 'evil-insert-state) ; org capture

; Org mode 中回车的设置
(define-key evil-normal-state-map (kbd "RET") 'org-return)

; 将 jj 映射为 ESC
(define-key evil-insert-state-map "j" #'tommy/maybe-exit)

(evil-define-command tommy/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

(provide 'init-evil)
