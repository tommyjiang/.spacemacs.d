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

(provide 'init-mu4e)
