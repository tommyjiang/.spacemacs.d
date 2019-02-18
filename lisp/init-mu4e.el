; 后端
(setq mu4e-get-mail-command "offlineimap")

; 基本配置
(setq mu4e-maildir "~/mail"
      mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder "/Sent Messages"
      mu4e-refile-folder "/&UXZO1mWHTvZZOQ-.&W1hoYw-"
      mu4e-trash-folder "/Deleted Messages"
      mu4e-update-interval nil
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addressed t
)

; 邮箱快捷键配置
(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/Sent Messages" . ?s)
        ("/Archive" . ?a)
        ("/Junk" . ?j)
        ("/Deleted Messages" . ?d)
        ("/Drafts" . ?D)
        )
)

; 邮箱账户配置
(setq user-mail-address "187355802@qq.com"
      user-full-name "江浩"
      mu4e-compose-signature
      (concat
       "祝好，\n"
       "江浩")
      mu4e-compose-signature-auto-include t
      )

; 发邮件配置
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.qq.com"
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 587)

; 附件存储目录
(setq mu4e-attachment-dir "~/Downloads")

; IMAP 服务器同步间隔，单位为 s
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 300)

; 新邮件提醒
(setq mu4e-enable-notifications t)
(with-eval-after-load 'mu4e-alert
  (when (eq system-type 'gnu/linux) (mu4e-alert-set-default-style 'notifications)) ; Linux 桌面提醒
  (when (eq system-type 'darwin) (mu4e-alert-set-default-style 'notifier)) ; Mac OS 桌面提醒
)

; 日期格式
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
(setq mu4e-view-date-format "%Y-%m-%d %A %H:%M")

; 显示内容
(setq mu4e-headers-fields
  '((:date . 25)
    (:flags . 6)
    (:from . 22)
    (:subject . nil))
)

; Modeline 显示
(setq mu4e-enable-mode-line t)
(mu4e-alert-enable-mode-line-display)

; org-mu4e 配置
; https://emacs-china.org/t/topic/498/9
(defun mu4e-toggle-org-mode ()
  (interactive)
  (cond
   ((eq major-mode 'mu4e-view-mode) (mu4e-org-mode))
   ((eq major-mode 'mu4e-org-mode) (mu4e-view-mode))
   ((eq major-mode 'mu4e-compose-mode) (org-mu4e-compose-org-mode))
   ((eq major-mode 'org-mu4e-compose-org-mode) (mu4e-compose-mode))))

(with-eval-after-load 'mu4e-view
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-view-mode
    "to" 'mu4e-toggle-org-mode))

(with-eval-after-load 'mu4e-utils
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-org-mode
    "to" 'mu4e-toggle-org-mode))

(with-eval-after-load 'mu4e-compose
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode "to" 'mu4e-toggle-org-mode))

(with-eval-after-load 'org-mu4e
  (setq org-mu4e-convert-to-html t)
  (spacemacs/set-leader-keys-for-major-mode 'org-mu4e-compose-org-mode "to" 'mu4e-toggle-org-mode)
  (defun org~mu4e-mime-convert-to-html ()
    "Convert the current body to html."
    (unless (fboundp 'org-export-string-as)
      (mu4e-error "require function 'org-export-string-as not found."))
    (let* ((begin
            (save-excursion
              (goto-char (point-min))
              (search-forward mail-header-separator)))
           (end (point-max))
           (raw-body (buffer-substring begin end))
           (tmp-file (make-temp-name (expand-file-name "mail"
                                                       temporary-file-directory)))
           (org-export-skip-text-before-1st-heading nil)
           (org-export-htmlize-output-type 'inline-css)
           (org-export-preserve-breaks t)
           (org-export-with-LaTeX-fragments
            (if (executable-find "dvipng") 'dvipng
              (mu4e-message "Cannot find dvipng, ignore inline LaTeX") nil))
           (html-and-images
            (org~mu4e-mime-replace-images
             (org-export-string-as raw-body 'html nil)
             tmp-file))
           (html-images (cdr html-and-images))
           (html (car html-and-images)))
      (delete-region begin end)
      (save-excursion
        (goto-char begin)
        (newline)
        (insert (org~mu4e-mime-multipart
                 raw-body html (mapconcat 'identity html-images "\n")))))))

; 只显示直接发送邮件的联系人
(setq helm-mu-contacts-personal t)

; 直接执行标记，不再询问
(setq mu4e-headers-leave-behavior 'apply)

; 只显示最近 7 天的发件人
(setq mu4e-compose-complete-only-after
      (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 7))))

; 利用 helm-mu 进行查找
(define-key mu4e-main-mode-map "s" 'helm-mu)
(define-key mu4e-headers-mode-map "s" 'helm-mu)
(define-key mu4e-view-mode-map "s" 'helm-mu)

(provide 'init-mu4e)
