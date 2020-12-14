; Org-mode settings by Tommy

; Do not confirm deletion in org agenda
(setq org-agenda-confirm-kill nil)

; Pad leading zero in org agenda
(setq org-agenda-time-leading-zero t)

; Display agenda when starting Emacs
(add-hook 'after-init-hook (lambda () (org-agenda nil " ")))

; Enable hl-todo mode in orgmode
(add-hook 'org-mode-hook (lambda () (hl-todo-mode 1) nil))

; Set org deadline faces
(setq org-agenda-deadline-faces
  '((1.0  . org-level-1)
    (0.75 . org-level-2)
    (0.5  . org-level-3)
    (0.25 . org-level-4)
    (0.0  . org-level-4)))

; Set anniversary/holiday faces in org agenda
(defface anniversary-lock-face '((t (:foreground "#9900FF" :bold t :slant normal))) "anniversary-lock-face")
(defface holiday-lock-face '((t (:foreground "#FF2121" :bold t :slant normal))) "holiday-lock-face")
(defface event-duration-lock-face '((t (:foreground "#D33682"))) "event-duration-lock-face")

(defun tommy-set-org-agenda-faces ()
  "Set font lock faces in Org Agenda"
  (interactive)
  (highlight-regexp "纪念日:.*" 'anniversary-lock-face)
  (highlight-regexp "节日.*" 'holiday-lock-face)
  (highlight-regexp ".*\([0-9]+\/[0-9]+\):.*" 'event-duration-lock-face)
)

(add-hook 'org-agenda-finalize-hook 'tommy-set-org-agenda-faces)

; Set org mode time stamp formats
(setq org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
(setq org-time-stamp-custom-formats  '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))

; Show current clock time
(setq org-clock-modeline-total 'current)

; Set system-time-locale
(set-locale-environment "zh_CN.utf-8")

; Set deadline warning days
(setq org-deadline-warning-days 30)

; Deadline headers
(setq org-agenda-deadline-leaders '("今天截止:" "还有 %02d 天:" "%2d 天以前:"))
(setq org-agenda-scheduled-leaders '("已安排:" "计划已过期 %d 天:"))

; Birthday display format
(setq org-bbdb-anniversary-format-alist
  '(("birthday" .
     (lambda (name years suffix)
       (concat "[[bbdb:" name "][" name " ("
               (format "%s" years)
               "岁生日" ")]]")))))

; org agenda views
(setq org-agenda-sorting-strategy
      (quote ((agenda time-up deadline-up habit-up category-keep)
              (todo category-up effort-up))))

; Using / instead of - in org-read-date
(defun my-read-date ()
"Parse date for capturing ledger entries via org mode"
(replace-regexp-in-string "-" "/" (org-read-date)))

; Org Agenda date and time in Chinese
(require 'org-agenda)

(defalias 'org-agenda-format-date-aligned 'tommy-org-agenda-format-date-aligned)

(defun tommy-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
  This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
    (day (cadr date))
    (day-of-week (calendar-day-of-week date))
    (month (car date))
    (monthname (calendar-month-name month))
    (year (nth 2 date))
    (iso-week (org-days-to-iso-week
      (calendar-absolute-from-gregorian date)))
      (weekyear (cond ((and (= month 1) (>= iso-week 52))
        (1- year))
        ((and (= month 12) (<= iso-week 1))
        (1+ year))
        (t year)))
    (weekstring (if (= day-of-week 1)
      (format " W%02d" iso-week)
        ""))
    (chinese-dayname '("星期日" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六")))
    (format "%4d年%02d月%02d日 第%02d周 %s"
      year month day iso-week (nth day-of-week chinese-dayname))))

; Look ahead for 3 days
(setq org-agenda-span 3)

; replace \emsp in org clock report
(defun tommy-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "\\"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "__")))
      (concat str "__ "))))

(advice-add 'org-clocktable-indent-string :override #'tommy-org-clocktable-indent-string)

(defface phone-number-face '((t (:foreground "red"))) t)

; org completion with helm
(setq org-completion-use-ido nil)

(when (and (boundp 'org-completion-handler)
           (require 'helm nil t))
  (defun org-helm-completion-handler
      (prompt collection &optional predicate require-match
              initial-input hist def inherit-input-method)
    (helm-comp-read prompt
                    collection
                    ;; the character \ is filtered out by default ;(
                    :fc-transformer nil
                    :test predicate
                    :must-match require-match
                    :initial-input initial-input
                    :history hist
                    :default def))

  (setq org-completion-handler 'org-helm-completion-handler))

; Restore window after quitting the agenda
(setq org-agenda-restore-windows-after-quit t)

; checkbox 对号标志
(defun tommy/org-html-checkbox (checkbox)
  "Format CHECKBOX into HTML."
  (case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
                 (off "<span class=\"checkbox\">&#x2610;</span>") ; checkbox (not checked)
                 (trans "<code>[-]</code>")
                 (t "")))

(defadvice org-html-checkbox (around tommy activate)
  (setq ad-return-value (tommy/org-html-checkbox (ad-get-arg 0))))

; 导出 html 时去掉 extra header
(setq org-html-head-extra nil)

(setq org-html-postamble nil)

; org LaTeX preview scale
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))

; org mode auto-fill mode
(add-hook 'org-mode-hook 'auto-fill-mode)

; set org-tag-column
(setq org-tags-column -100)

(provide 'init-orgTommy)
