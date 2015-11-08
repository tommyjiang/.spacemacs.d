; Org-mode settings by Tommy

; org-mode crypt append system path
(setq exec-path (append exec-path '("/usr/local/bin")))

; Display agenda when starting Emacs
(add-hook 'after-init-hook (lambda () (org-agenda nil " ")))

; Set org mode time stamp format
;(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
; Set org deadline faces
; (setq org-agenda-deadline-faces
;   '((1.0  . org-level-1)
;     (0.75 . org-level-2)
;     (0.5  . org-level-3)
;     (0.25 . org-level-4)
;     (0.0  . org-level-4)))

; (defface phone-number-lock-face '((t (:foreground "FF0000"))) t)
; (defface language-lock-face '((t (:foreground "00FF00"))) t)
; (defface emacs-vim-lock-face '((t (:foreground "9900FF"))) t)
; 
; (global-hi-lock-mode 1)
; (setq hi-lock-file-patterns-policy #'(lambda (dummy) t))
; 
; (defun bigboss-highlight ()
;   (interactive)
;   (highlight-regexp "0[0-9]\\{2\\}-[0-9]\\{8\\}" 'phone-number-lock-face)
;   (highlight-regexp "Lisp\\|Scheme" 'language-lock-face)
;   (highlight-regexp "神之编辑器\\|编辑器之神" 'emacs-vim-lock-face)
; )
; 
; (add-hook 'org-mode-hook 'bigboss-highlight)

; Set anniversary/holiday faces in org agenda
(defface anniversary-lock-face '((t (:foreground "#9900FF" :bold t))) t)
(defface holiday-lock-face '((t (:foreground "#FF2121" :bold t))) t)
(defface event-duration-lock-face '((t (:foreground "#FF1493"))) t)

(defun tommy-set-org-agenda-faces ()
  "Set font lock faces in Org Agenda"
  (interactive)
  (highlight-regexp "纪念日:.*" "anniversary-lock-face")
  (highlight-regexp "节日.*" "holiday-lock-face")
  (highlight-regexp ".*\([0-9]+\/[0-9]+\):.*" "event-duration-lock-face")
)

(add-hook 'org-finalize-agenda-hook 'tommy-set-org-agenda-faces)

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
(setq org-agenda-deadline-leaders '("今天截止:" "还有 %2d 天:" "%2d 天以前:"))
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
    (format "%4d年%d月%d日 第%2d周 %s"
      year month day iso-week (nth day-of-week chinese-dayname))))

; Look ahead for 5 days
(setq org-agenda-span 5)

; Org Agenda faces
(custom-set-faces
  '(org-agenda-date-today ((t (:inherit org-agenda-date :weight bold))) t)
  ;'(org-scheduled-today ((t (:foreground "00FF00"))) t)
  '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "#F47983" :weight bold))) t)
  ;'(org-agenda-date ((t (:foreground "#4B5CC4"))) t)
  )

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

(setq org-agenda-confirm-kill nil)

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

(provide 'init-orgTommy)
