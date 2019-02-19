; 设置经纬度
(setq calendar-latitude 40.01)
(setq calendar-longitude 116.3)
(setq calendar-location-time "Beijing, China")

; 设定每周一为新一周起始日期
(setq calendar-week-start-day 1)

; 计算周数
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-warning-face))

; 显示周数
(setq calendar-intermonth-header
      (propertize "周"
                  'font-lock-face 'font-lock-warning-face))

; 周几名称显示
(setq calendar-week-start-day 1
      calendar-day-name-array ["日" "一" "二" "三"
                               "四" "五" "六"])

; 周末用 keyword 颜色显示
(defadvice calendar-generate-month
  (after highlight-weekend-days (month year indent) activate)
  "Highlight weekend days"
  (dotimes (i 31)
    (let ((date (list month (1+ i) year)))
      (if (or (= (calendar-day-of-week date) 0)
              (= (calendar-day-of-week date) 6))
          (calendar-mark-visible-date date 'font-lock-keyword-face)))))

; 设置中国节日
(require 'cal-china-x)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq calendar-holidays cal-china-x-important-holidays)

(provide 'init-cal)
