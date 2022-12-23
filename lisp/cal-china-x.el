;;; cal-china-x.el --- Chinese localization, lunar/horoscope/zodiac info and more...

;; Copyright (C) 2006-2013, 2015 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 2.6b
;; Url: https://github.com/xwl/cal-china-x
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; This extension mainly adds the following extra features:
;;   - Chinese localization
;;   - Display holiday, lunar, horoscope, zodiac, solar term info on mode line
;;   - Define holidays using `holiday-lunar', `holiday-solar-term'
;;   - Highlight holidays based on different priorities
;;   - Add `cal-china-x-chinese-holidays', `cal-china-x-japanese-holidays'.
;;   - custom week diary(like weeks in school)
;;
;; To use, add something like the following to your .emacs:
;;     (require 'cal-china-x)
;;     (setq mark-holidays-in-calendar t)
;;     (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
;;     (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
;;     (setq calendar-holidays
;;           (append cal-china-x-important-holidays
;;                   cal-china-x-general-holidays
;;                   other-holidays))
;;
;; Note: for emacs22, please use version 1.1.

;;; History

;; This is an early derived work from `chinese-calendar.el' written by
;; Charles Wang <charleswang@peoplemail.com.cn>.

;;; Note:

;; - Display week day(the first line of each month) in chinese properly
;;   It is a bit difficult to do nice alignment since it depends on the font
;;   size of chinese characters and numbers. But some manages to do it:
;;     https://github.com/xwl/cal-china-x/issues/3

;;; Code:

(require 'calendar)
(require 'holidays)
(require 'cal-china)
(require 'cl-lib)

;;; Variables

(defconst cal-china-x-dir (if load-file-name
                              (file-name-directory load-file-name)
                            ""))

;; Bound in calendar-generate.
(defvar displayed-month)
(defvar displayed-year)

(defconst cal-china-x-celestial-stem
  ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])

(defconst cal-china-x-terrestrial-branch
  ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(defconst cal-china-x-days
  ["日" "一" "二" "三" "四" "五" "六"])

(defconst cal-china-x-month-name
  ["正月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "冬月" "腊月"])

(defconst cal-china-x-day-name
  ["初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十"
   "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九" "二十"
   "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"])

(defconst cal-china-x-nine-characters
  ["一九" "二九" "三九" "四九" "五九" "六九" "七九" "八九" "九九"])

(defvar chinese-date-diary-pattern
  `((year "年" month "月" day "日" " 星期[" ,(mapconcat 'identity cal-china-x-days "") "]")
    ,@(if (> emacs-major-version 22)
          diary-iso-date-forms
        '((month "[-/]" day "[^-/0-9]")
          (year "[-/]" month "[-/]" day "[^0-9]")
          (monthname "-" day "[^-0-9]")
          (year "-" monthname "-" day "[^0-9]")
          (dayname "\\W")))))

(defconst cal-china-x-horoscope-name
  '(((3  21) (4  19) "白羊")
    ((4  20) (5  20) "金牛")
    ((5  21) (6  21) "双子")
    ((6  22) (7  22) "巨蟹")
    ((7  23) (8  22) "狮子")
    ((8  23) (9  22) "处女")
    ((9  23) (10 23) "天秤")
    ((10 24) (11 22) "天蝎")
    ((11 23) (12 21) "射手")
    ((12 22) (1  19) "摩羯")
    ((1  20) (2  18) "水瓶")
    ((2  19) (3  20) "双鱼")))

(defconst cal-china-x-zodiac-name
  ["鼠" "牛" "虎" "兔" "龙" "蛇" "马" "羊" "猴" "鸡" "狗" "猪"]
  "The zodiac(生肖) when you were born.")

;; for ref, http://www.geocities.com/calshing/chinesecalendar.htm
(defconst cal-china-x-solar-term-name
  ["小寒" "大寒" "立春" "雨水" "惊蛰" "春分"
   "清明" "谷雨" "立夏" "小满" "芒种" "夏至"
   "小暑" "大暑" "立秋" "处暑" "白露" "秋分"
   "寒露" "霜降" "立冬" "小雪" "大雪" "冬至"]
  "24 solar terms(节气, in chinese).
\"小寒\" is the first solar term in a new year. e.g., 2007-01-06.
There is a short poem for remembering,

    春雨惊春清谷天，夏满芒夏暑相连，
    秋处露秋寒霜降，冬雪雪冬小大寒。")

(defconst cal-china-x-chinese-holidays
  '((holiday-fixed 1 1 "元旦")
    (holiday-lunar 1 1 "春节" 0)
    (holiday-lunar 1 5 "破五" 0)
    (holiday-lunar 1 15 "元宵节" 0)
    (holiday-lunar 2 2 "龙抬头" 0)
    (holiday-fixed 2 14 "情人节")
    (holiday-fixed 2 23 "奶奶生日")
    (holiday-lunar 3 3 "上巳节" 0)
    (holiday-solar-term "清明" "清明节")
    (holiday-fixed 5 1 "劳动节")
    (holiday-lunar 4 8 "佛诞" 0)
    (holiday-lunar 5 5 "端午节" 0)
    (holiday-lunar 5 27 "姥爷生日" 0)
    (holiday-lunar 7 7 "七夕" 0)
    (holiday-lunar 7 15 "中元节" 0)
    (holiday-lunar 8 15 "中秋节" 0)
    (holiday-lunar 9 9 "重阳节" 0)
    (holiday-fixed 5 4 "姥姥生日")
    (holiday-fixed 9 10 "教师节")
    (holiday-fixed 10 1 "国庆节")
    (holiday-lunar 10 15 "下元节")
    (holiday-lunar 12 8 "腊八" 0)
    (holiday-solar-term "立春" "立春")
    (holiday-solar-term "雨水" "雨水")
    (holiday-solar-term "惊蛰" "惊蛰")
    (holiday-solar-term "春分" "春分")
    (holiday-solar-term "清明" "清明")
    (holiday-solar-term "谷雨" "谷雨")
    (holiday-solar-term "立夏" "立夏")
    (holiday-solar-term "小满" "小满")
    (holiday-solar-term "芒种" "芒种")
    (holiday-solar-term "夏至" "夏至")
    (holiday-solar-term "小暑" "小暑")
    (holiday-solar-term "大暑" "大暑")
    (holiday-solar-term "立秋" "立秋")
    (holiday-solar-term "处暑" "处暑")
    (holiday-solar-term "白露" "白露")
    (holiday-solar-term "秋分" "秋分")
    (holiday-solar-term "寒露" "寒露")
    (holiday-solar-term "霜降" "霜降")
    (holiday-solar-term "立冬" "立冬")
    (holiday-solar-term "小雪" "小雪")
    (holiday-solar-term "大雪" "大雪")
    (holiday-solar-term "冬至" "冬至")
    (holiday-solar-term "小寒" "小寒")
    (holiday-solar-term "大寒" "大寒")
    (holiday-lunar 12 1 "买春联" 0)
    (holiday-lunar 12 23 "小年" 0)
    (holiday-fixed 12 24 "平安夜")
    (holiday-fixed 12 25 "圣诞节")
    (holiday-lunar 12 30 "除夕" 0)
    )
  "Pre-defined chinese public holidays.
You can add this to your `calendar-holidays'.")

;;; Interfaces

(defgroup cal-china-x nil
  "Chinese calendar extentions and more."
  :group 'calendar)

(defcustom cal-china-x-important-holidays '()
  "Highlighted by `cal-china-x-important-holiday-face'."
  :type 'symbol
  :group 'cal-china-x)

(defcustom cal-china-x-general-holidays '()
  "Highlighted by `cal-china-x-general-holiday-face'."
  :type 'symbol
  :group 'cal-china-x)

(defface cal-china-x-important-holiday-face
  '((((class color) (background light))
     :background "red")
    (((class color) (background dark))
     :background "red")
    (t
     :inverse-video t))
  "Face for indicating `cal-china-x-important-holidays'."
  :group 'cal-china-x)

(defface cal-china-x-general-holiday-face
  '((((class color) (background light))
     :background "green")
    (((class color) (background dark))
     :background "green")
    (t
     :inverse-video t))
  "Face for indicating `cal-china-x-general-holidays'."
  :group 'cal-china-x)

(defcustom cal-china-x-custom-week-start-date '()
  "The month and day of first Monday in your custom week diary.

e.g., '(9 20) means from every year, Sep 20th will be defined as
the first week.  This could be useful in some circumstances, such
as schools, where people may use some specific school diary."
  :type 'symbol
  :group 'cal-china-x)

(defcustom cal-china-x-force-chinese-week-day nil
  "Force using chinese week day, even though it may not align nicely.

Default is nil. The chinese week day will be enabled automatically if
the package 'cnfonts (old name: 'chinese-fonts-setup) is loaded."
  :type 'boolean
  :group 'cal-china-x)

;;;###autoload
(defun cal-china-x-birthday-from-chinese (lunar-month lunar-day)
  "Return next birthday date in Gregorian form.

LUNAR-MONTH and LUNAR-DAY are date number used in chinese lunar
calendar."
  (interactive "nlunar month: \nnlunar day: ")
  (let* ((current-chinese-date (calendar-chinese-from-absolute
                                (calendar-absolute-from-gregorian
                                 (calendar-current-date))))
         (cycle (car current-chinese-date))
         (year (cadr current-chinese-date))
         (birthday-gregorian-full
          (cal-china-x-birthday-from-chinese-1
           cycle year lunar-month lunar-day)))
    ;; If it is before current date, calculate next year.
    (when (calendar-date-compare (list birthday-gregorian-full)
                                 (list (calendar-current-date)))
      (setq birthday-gregorian-full
            (cal-china-x-birthday-from-chinese-1
             cycle (1+ year) lunar-month lunar-day)))
    (message "Your next birthday in gregorian is on %s"
             (calendar-date-string birthday-gregorian-full))))

(defun cal-china-x-birthday-from-chinese-1 (cycle year lunar-month lunar-day)
  (calendar-gregorian-from-absolute
   (calendar-chinese-to-absolute
    (list cycle year lunar-month lunar-day))))

;;;###autoload
(defun holiday-lunar (lunar-month lunar-day string &optional num)
  "Like `holiday-fixed', but with LUNAR-MONTH and LUNAR-DAY.

When there are multiple days(like Run Yue or 闰月, e.g.,
2006-08-30, which is 07-07 in lunar calendar, the chinese
valentine's day), we use NUM to define which day(s) as
holidays. The rules are:

NUM = 0, only the earlier day.
NUM = 1, only the later day.
NUM with other values(default), all days(maybe one or two).

emacs23 introduces a similar `holiday-chinese', a quick test
shows that it does not recognize Run Yue at all."
  (unless (integerp num)
    (setq num 2))
  (let ((holiday (holiday-lunar-1 lunar-month lunar-day string num)))
    (when (and (= lunar-day 30)         ; Some months only have 29 days.
               (equal (holiday-lunar-1 (if (= lunar-month 12) 1 (1+ lunar-month))
                                       1 string num)
                      holiday))
      (setq holiday (holiday-lunar-1 lunar-month (1- lunar-day) string num)))
    holiday))

(defun holiday-lunar-1 (lunar-month lunar-day string &optional num)
  (let* ((cn-years (calendar-chinese-year ; calendar-chinese-year counts from 12 for last year
                    (if (and (eq displayed-month 12) (eq lunar-month 12))
                        (1+ displayed-year)
                      displayed-year)))
         (ret (holiday-lunar-2 (assoc lunar-month cn-years) lunar-day string)))
    (when (and (> (length cn-years) 12) (not (zerop num)))
      (let ((run-yue '())
            (years cn-years)
            (i '()))
        (while years
          (setq i (car years)
                years (cdr years))
          (unless (integerp (car i))
            (setq run-yue i)
            (setq years nil)))
        (when (= lunar-month (floor (car run-yue)))
          (setq ret (append ret (holiday-lunar-2
                                 run-yue lunar-day string))))))
    (cond ((= num 0)
           (when (car ret) (list (car ret))))
          ((= num 1)
           (if (cadr ret) (list (cadr ret)) ret))
          (t
           ret))))

(defun holiday-lunar-2 (run-yue lunar-day string)
  (let* ((date (calendar-gregorian-from-absolute
                (+ (cadr run-yue) (1- lunar-day))))
         (holiday (holiday-fixed (car date) (cadr date) string)))
    ;; Same year?
    (when (and holiday (= (nth 2 (caar holiday)) (nth 2 date)))
      holiday)))

;;;###autoload
(defun holiday-solar-term (solar-term str)
  "A holiday(STR) on SOLAR-TERM day.
See `cal-china-x-solar-term-name' for a list of solar term names ."
  (cal-china-x-sync-solar-term displayed-year)
  (let ((terms cal-china-x-solar-term-alist)
        i date)
    (while terms
      (setq i (car terms)
            terms (cdr terms))
      (when (string= (cdr i) solar-term)
        (let ((m (caar i))
              (y (cl-caddar i)))
          ;; displayed-year, displayed-month is accurate for the centered month
          ;; only. Cross year view: '(11 12 1), '(12 1 2)
          (when (or (and (cal-china-x-cross-year-view-p)
                         (or (and (= displayed-month 12)
                                  (= m 1)
                                  (= y (1+ displayed-year)))
                             (and (= displayed-month 1)
                                  (= m 12)
                                  (= y (1- displayed-year)))))
                    (= y displayed-year))
            (setq terms '()
                  date (car i))))))
    (holiday-fixed (car date) (cadr date) str)))

(defun cal-china-x-calendar-display-form (date)
  (if (equal date '(0 0 0))
      ""
    (format "%04d年%02d月%02d日 %s"
            (calendar-extract-year date)
            (calendar-extract-month date)
            (calendar-extract-day date)
            (cal-china-x-day-name date))))

(defun cal-china-x-chinese-date-string (date)
  (let* ((cn-date (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian date)))
         (cn-year  (cadr   cn-date))
         (cn-month (cl-caddr  cn-date))
         (cn-day   (cl-cadddr cn-date)))
    (format "%s%s年%s%s%s(%s)%s%s%s"
            (calendar-chinese-sexagesimal-name cn-year)
            (aref cal-china-x-zodiac-name (% (1- cn-year) 12))
            ; (concat (calendar-chinese-sexagesimal-name cn-month) "月")
            (aref cal-china-x-month-name (1-  (floor cn-month)))
            (if (integerp cn-month) "" "(闰月)")
            ; (concat (calendar-chinese-sexagesimal-name cn-day) "日")
            (aref cal-china-x-day-name (1- cn-day))
            (cal-china-x-get-horoscope (car date) (cadr date))
            (cal-china-x-get-solar-term date)
            (cal-china-x-get-several-nines-string date)
            (cal-china-x-get-futian-string date)
            )))

(defun cal-china-x-setup ()
  (setq calendar-date-display-form
    '((cal-china-x-calendar-display-form
           (mapcar (lambda (el) (string-to-number el))
                   (list month day year)))))

  (setq diary-date-forms chinese-date-diary-pattern)

  ;; chinese month and year
  ; (setq calendar-font-lock-keywords
  ;       (append calendar-font-lock-keywords
  ;               '(("[0-9]+年\\ *[0-9]+月" . font-lock-function-name-face))))

  (setq calendar-chinese-celestial-stem cal-china-x-celestial-stem
        calendar-chinese-terrestrial-branch cal-china-x-terrestrial-branch)

  (setq calendar-month-header '(propertize (format "%d年%2d月" year month)
                                           'font-lock-face
                                           'calendar-month-header))

  (if cal-china-x-force-chinese-week-day
      (setq calendar-day-header-array cal-china-x-days)

    (eval-after-load 'chinese-fonts-setup ; older name of cnfonts, to be removed
      '(progn
         (setq calendar-day-header-array cal-china-x-days)
         ))

    (eval-after-load 'cnfonts
      '(progn
         (setq calendar-day-header-array cal-china-x-days)
         )))

  (setq calendar-mode-line-format
        (list
         (calendar-mode-line-entry 'calendar-scroll-right "previous month" "<")
         "Calendar"

         '(cal-china-x-get-holiday date)

         '(concat " " (calendar-date-string date t))

         '(cal-china-x-chinese-date-string date)

         (calendar-mode-line-entry 'calendar-scroll-left "next month" ">")))

  (add-hook 'calendar-move-hook 'calendar-update-mode-line)
  (add-hook 'calendar-initial-window-hook 'calendar-update-mode-line)

  (add-hook 'calendar-mode-hook
            (lambda ()
              (set (make-local-variable 'font-lock-defaults)
                   ;; chinese month and year
                   '((("[0-9]+年\\ *[0-9]+月" . font-lock-function-name-face)) t))
              ))

  ;(advice-add 'calendar-mark-holidays :around 'cal-china-x-mark-holidays)
  (advice-add 'mouse-set-point :after 'cal-china-x-mouse-set-point)
  )

;;; Implementations

(defun cal-china-x-day-name (date)
  "Chinese day name in a week, like `星期一'."
  (concat "星期" (aref cal-china-x-days (calendar-day-of-week date))))

(defun cal-china-x-day-short-name (num)
  "Short chinese day name in a week, like `一'. NUM is from 0..6
in a week."
  (aref cal-china-x-days num))

(defun cal-china-x-get-horoscope (month day)
  "Return horoscope(星座) on MONTH(1-12) DAY(1-31)."
  (catch 'return
    (mapc
     (lambda (el)
       (let ((start (car el))
             (end (cadr el)))
         (when (or (and (= month (car start)) (>= day (cadr start)))
                   (and (= month (car end)) (<= day (cadr end))))
           (throw 'return (cl-caddr el)))))
     cal-china-x-horoscope-name)))

(defun holiday-chinese-new-year ()
  "Date of Chinese New Year."
  (let ((m displayed-month)
        (y displayed-year))
    (calendar-increment-month m y 1)
    (if (< m 5)
        (let ((chinese-new-year
               (calendar-gregorian-from-absolute
                (cadr (assoc 1 (calendar-chinese-year y))))))
          (if (calendar-date-is-visible-p chinese-new-year)
          `((,chinese-new-year
                 ,(format "%s年春节"
                          (calendar-chinese-sexagesimal-name
                           (+ y 57))))))))))

(defun cal-china-x-solar-term-date (date solar-term)
  "Return solar-term date in Gregorian form."
  (let* ((cyear (calendar-extract-year date)))
    (car (rassoc solar-term (cal-china-x-solar-term-alist-new cyear)))))

(defun cal-china-x-winter-solstice-date (date)
  "Return winter solstice(冬至) date in Gregorian form.

If MONTH = 12, return current year's date
Else return last year's date"
  (let* ((cyear (if (= (calendar-extract-month date) 12)
                    (calendar-extract-year date)
                  (1- (calendar-extract-year date)))))
    (car (rassoc '"冬至" (cal-china-x-solar-term-alist-new cyear)))))

(defun winter-solstice-day-diff (date)
  (cal-china-x-days-diff date (cal-china-x-winter-solstice-date date)))

(defun cal-china-x-get-several-nines-string (date)
  (let ((daygap (winter-solstice-day-diff date)))
    (if (or (< daygap 0) (> daygap 80))
        ""
      (concat (aref cal-china-x-nine-characters (/ daygap 9))
              "("
              (number-to-string (1+ (% daygap 9)))
              ")"
              ))))

(defun cal-china-x-solar-term-date (date solar-term)
  "Return solar-term date in Gregorian form."
  (let* ((cyear (calendar-extract-year date)))
    (car (rassoc solar-term (cal-china-x-solar-term-alist-new cyear)))))

(defun cal-china-x-chinese-day-celestial-stem-number (date)
  "String of Chinese date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((a-date (calendar-absolute-from-gregorian date)))
    (% (+ a-date 15) 10)))

(defun cal-china-x-solar-term-celestical-stem (date solar-term)
  (cal-china-x-chinese-day-celestial-stem-number
   (cal-china-x-solar-term-date date solar-term)))

(defun cal-china-x-day-diff-from-solar-term (date solar-term) ; 庚日
  (let ((ss-stem (- 7 (cal-china-x-solar-term-celestical-stem date solar-term))))
    (if (< ss-stem 0) (+ ss-stem 10)
      ss-stem)))

(defun cal-china-x-chufu-date (date)
  (let* ((ss-date (cal-china-x-solar-term-date date "夏至"))
         (ss-year (calendar-extract-year ss-date))
         (ss-day (calendar-extract-day ss-date))
         (day-diff (+ 20 (cal-china-x-day-diff-from-solar-term date "夏至")))
         (chufu-day (- day-diff (- 30 ss-day))))
    (list 7 chufu-day ss-year)))

(defun cal-china-x-zhongfu-date (date)
  (list 7 (+ (calendar-extract-day (cal-china-x-chufu-date date)) 10)
        (calendar-extract-year date)))

(defun cal-china-x-mofu-date (date)
  (let* ((ss-date (cal-china-x-solar-term-date date "立秋"))
         (ss-year (calendar-extract-year ss-date))
         (ss-day (calendar-extract-day ss-date))
         (day-diff (cal-china-x-day-diff-from-solar-term date "立秋"))
         (mofu-day (+ day-diff ss-day)))
    (list 8 mofu-day ss-year)))

(defun cal-china-x-get-futian-string (date)
  (let* ((chufu (cal-china-x-chufu-date date))
         (zhongfu (cal-china-x-zhongfu-date date))
         (mofu (cal-china-x-mofu-date date))
         (chufu-gap (cal-china-x-days-diff date chufu))
         (zhongfu-gap (cal-china-x-days-diff date zhongfu))
         (mofu-gap (cal-china-x-days-diff date mofu))
         )
    (if (or (< chufu-gap 0) (> mofu-gap 9))
        ""
      (if (and (>= chufu-gap 0) (< zhongfu-gap 0))
          (concat "初伏("
                  (number-to-string (1+ chufu-gap))
                  ")")
        (if (and (>= zhongfu-gap 0) (< mofu-gap 0))
            (concat "中伏("
                    (number-to-string (1+ zhongfu-gap))
                    ")")
          (concat "末伏("
                  (number-to-string (1+ mofu-gap))
                  ")"))))))

(defun cal-china-x-get-solar-term (date)
  (let ((year (calendar-extract-year date)))
    (cal-china-x-sync-solar-term year)
    (or (cdr (assoc date cal-china-x-solar-term-alist)) "")))

(defun cal-china-x-solar-term-alist-new (year)
  "Return a solar-term alist for YEAR."
  ;; use cached values (china time zone +0800)
  (let ((cached-jieqi-file (expand-file-name (concat cal-china-x-dir "/jieqi.txt"))))
    (if (and (> year 1900)
             (< year 2101)
             (file-exists-p cached-jieqi-file))
        (let ((solar-term-alist '())
              (year (number-to-string year)))
          (with-temp-buffer
            (insert-file-contents cached-jieqi-file)
            (goto-char (point-min))
            (while (search-forward year nil t 1)
              (let* ((str (buffer-substring (line-beginning-position) (line-end-position)))
                     (lst (split-string str))
                     (jieqi (nth 0 lst))
                     (y (string-to-number (nth 1 lst)))
                     (m (string-to-number (nth 2 lst)))
                     (d (string-to-number (nth 3 lst))))
                (setq solar-term-alist (cons (cons (list m d y) jieqi)
                                             solar-term-alist)))))
          solar-term-alist)
      ;; calculation may have one day difference.
      (cl-loop for i from 0 upto 23

             for date = (cal-china-x-next-solar-term `(1 1 ,year))
             then (setq date (cal-china-x-next-solar-term date))

             with solar-term-alist = '()

             collect (cons date (aref cal-china-x-solar-term-name i))
             into solar-term-alist

             finally return solar-term-alist))))

(defun cal-china-x-gregorian-from-astro (a)
  (calendar-gregorian-from-absolute
   (floor (calendar-astro-to-absolute a))))

(defun cal-china-x-astro-from-gregorian (g)
  (calendar-astro-from-absolute
   (calendar-absolute-from-gregorian g)))

(defun cal-china-x-next-solar-term (date)
  "Return next solar term's data after DATE.
Each solar term is separated by 15 longtitude degrees or so, plus an
extra day appended."
  (cal-china-x-gregorian-from-astro
   (solar-date-next-longitude
    (calendar-astro-from-absolute
     (1+ (calendar-absolute-from-gregorian date))) 15)))

(defun cal-china-x-get-holiday (date)
  (when (and (boundp 'displayed-month)
             (boundp 'displayed-year))
    (let ((holidays (calendar-holiday-list))
          (str ""))
      (dolist (i holidays)
        (when (equal (car i) date)
          (setq str (concat str " " (cadr i)))))
      str)))

;; cached solar terms for two neighbour years at most.
(defvar cal-china-x-solar-term-alist nil) ; e.g., '(((1 20 2008) "春分") ...)
(defvar cal-china-x-solar-term-years nil)

(defun cal-china-x-sync-solar-term (year)
  "Sync `cal-china-x-solar-term-alist' and `cal-china-x-solar-term-years' to YEAR."
  (cond ((or (not cal-china-x-solar-term-years)
             ;; TODO: Seems calendar-update-mode-line is called too early in
             ;; calendar-mode.
             (not (boundp 'displayed-year))
             (not (boundp 'displayed-month)))
         (setq cal-china-x-solar-term-alist
               (cal-china-x-solar-term-alist-new year))
         (setq cal-china-x-solar-term-years (list year)))
        ((not (memq year cal-china-x-solar-term-years))
         (setq cal-china-x-solar-term-alist
               (append
                (cl-remove-if-not (lambda (i) (eq (cl-caddar i) displayed-year))
                                  cal-china-x-solar-term-alist)
                (cal-china-x-solar-term-alist-new year)))
         (setq cal-china-x-solar-term-years
               (cons year (cl-remove-if-not (lambda (i) (eq i displayed-year))
                                            cal-china-x-solar-term-years))))))

;; When months are: '(11 12 1), '(12 1 2)
(defun cal-china-x-cross-year-view-p ()
  (or (= displayed-month 12) (= displayed-month 1)))

;; ,----
;; | week
;; `----

(defun cal-china-x-week-of-date (date)
  "Get week number from DATE."
  (car (calendar-iso-from-absolute (calendar-absolute-from-gregorian date))))

(defun cal-china-x-custom-week-of-date (date)
  "Similar to `cal-china-x-week-of-date' but starting from `cal-china-x-custom-week-start-date'."
  (let* ((y (calendar-extract-year  date))
         (m (calendar-extract-month date))
         (d (calendar-extract-day   date))
         (start-date `(,@cal-china-x-custom-week-start-date ,y))
         (start-month (calendar-extract-month start-date))
         (start-day   (calendar-extract-day start-date)))

    (when (or (< m start-month)
              (and (= m start-month) (< d start-day)))
      (setq start-date (list (car start-date) (cadr start-date) (1- y))))

    (1+ (/ (cal-china-x-days-diff date start-date) 7))))

(defun cal-china-x-days-diff (date1 date2)
  "date1 - date2 = ?"
  (apply '- (mapcar 'calendar-absolute-from-gregorian (list date1 date2))))


;;; Modifications to Standard Functions

;; These functions(from calendar.el, cal-china.el) have been modified
;; for localization.

(defun calendar-chinese-sexagesimal-name (n)
  "The N-th name of the Chinese sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  ;; Change "%s-%s" to "%s%s", since adding the extra `-' between two Chinese
  ;; characters looks stupid.
  (format "%s%s"
          (aref calendar-chinese-celestial-stem (% (1- n) 10))
          (aref calendar-chinese-terrestrial-branch (% (1- n) 12))))

(defun cal-china-x-remove-exising-overlays (beg end &rest args)
  (remove-overlays beg end))

(defun cal-china-x-mark-holidays (orig-fun &rest args)
  "Mark prioritized holidays with different colors."
  (apply orig-fun args)

  (advice-add 'make-overlay :before 'cal-china-x-remove-exising-overlays)
  (let ((calendar-holiday-marker 'cal-china-x-general-holiday-face)
        (calendar-holidays cal-china-x-general-holidays))
    (apply orig-fun args))
  (let ((calendar-holiday-marker 'cal-china-x-important-holiday-face)
        (calendar-holidays cal-china-x-important-holidays))
    (apply orig-fun args))
  (advice-remove 'make-overlay 'cal-china-x-remove-exising-overlays))

(defun cal-china-x-mouse-set-point (after &rest args)
    (when (eq major-mode 'calendar-mode)
      (calendar-update-mode-line)))


;; setup
(cal-china-x-setup)

(provide 'cal-china-x)

;;; Local Variables: ***
;;; coding: utf-8 ***
;;; End: ***

;;; cal-china-x.el ends here
