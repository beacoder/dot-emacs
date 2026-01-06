;; init-calendar.el --- Initialize calendar configurations.-*- lexical-binding: t -*-
;;; Commentary:


;; @see https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-calendar.el

;; Chinese calendar
;; `pC' can show lunar details
(when (maybe-require-package 'cal-china-x)
  (use-package cal-china-x
    :after calendar
    :commands cal-china-x-setup
    :init (cal-china-x-setup)
    :config
    ;; `S' can show the time of sunrise and sunset on Calendar
    (setq calendar-location-name "Chengdu"
          calendar-latitude 30.67
          calendar-longitude 104.06)

    ;; Holidays
    (setq calendar-mark-holidays-flag t)

    (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
    (setq cal-china-x-general-holidays
          '((holiday-lunar 1 15 "元宵节")
            (holiday-lunar 7 7 "七夕节")
            (holiday-fixed 3 8 "妇女节")
            (holiday-fixed 3 12 "植树节")
            (holiday-fixed 5 4 "青年节")
            (holiday-fixed 6 1 "儿童节")
            (holiday-fixed 9 10 "教师节")))
    (setq holiday-other-holidays
          '((holiday-fixed 2 14 "情人节")
            (holiday-fixed 4 1 "愚人节")
            (holiday-fixed 12 25 "圣诞节")
            (holiday-float 5 0 2 "母亲节")
            (holiday-float 6 0 3 "父亲节")
            (holiday-float 11 4 4 "感恩节")))
    (setq calendar-holidays
          (append cal-china-x-important-holidays
                  cal-china-x-general-holidays
                  holiday-other-holidays))))


;; Better views of calendar
(use-package calfw
  :ensure t
  :commands calfw-open-calendar-buffer
  ;; :bind ("<C-f12>" . open-calendar)
  :init
  (use-package calfw-org
    :ensure t
    :commands (calfw-org-open-calendar calfw-org-create-source))

  (use-package calfw-ical
    :ensure t
    :commands (calfw-ical-open-calendar calfw-ical-create-source))

  (defun open-calendar ()
    "Open calendar."
    (interactive)
    (unless (ignore-errors
              (calfw-open-calendar-buffer
               :contents-sources
               (list
                (when org-agenda-files
                  (calfw-org-create-source "YellowGreen"))
                (when (bound-and-true-p centaur-ical)
                  (calfw-ical-create-source "gcal" centaur-ical "IndianRed")))))
      (calfw-open-calendar-buffer))))


(provide 'init-calendar)
;;; init-calendar.el ends here
