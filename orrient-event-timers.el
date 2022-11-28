;;; orrient-event-timers.el --- Event Timers in GW2 -*- lexical-binding: t; -*-

;; Homepage: https://github.com/purplg/orrient
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Create and organize goals for yourself in Guild Wars 2

;; --------------------
;; Configuration

;;; Code:
(require 'cl-lib)

(cl-defstruct orrient-gw2-event
  name       ; Name of the event.
  offset     ; Minutes. Offset from UTC when the first event of the day occurs.
  length     ; Minutes. How long the event lasts.
  frequency) ; Minutes. How often the event occurs.

(cl-defstruct orrient-gw2-meta
  name
  events)

(defconst orrient-schedule
  `(,(make-orrient-gw2-meta
      :name "day and night"
      :events `(,(make-orrient-gw2-event :name "dawn"  :offset  25 :length  5 :frequency 120)
                ,(make-orrient-gw2-event :name "day"   :offset  30 :length 70 :frequency 120)
                ,(make-orrient-gw2-event :name "dusk"  :offset 100 :length  5 :frequency 120)
                ,(make-orrient-gw2-event :name "night" :offset 105 :length 40 :frequency 120)))

    ,(make-orrient-gw2-meta
      :name "other meta"
      :events `(,(make-orrient-gw2-event :name "dawn 2"  :offset  25 :length  5 :frequency 120)
                ,(make-orrient-gw2-event :name "day 2"   :offset  30 :length 70 :frequency 120)
                ,(make-orrient-gw2-event :name "dusk 2"  :offset 100 :length  5 :frequency 120)
                ,(make-orrient-gw2-event :name "night 2" :offset 105 :length 40 :frequency 120))))
  "list of meta events.")

(defvar orrient-et-buffer "*orrient-event-timers*")

(defun orrient--current-time ()
  "Return current time in minutes from UTC 0."
  0)

(defun orrient--event-next-occurance (event current-time)
  (let* ((offset (orrient-gw2-event-offset event))
         (frequency (orrient-gw2-event-frequency event))
         (index (/ current-time frequency))
         (time-until (- current-time next-start)))
    (+ offset (* (if (>= time-until 0)
                     (1+ index)
                   index) frequency))))

(defun orrient--meta-next-event (meta time))

(defun orrient--draw-meta (meta)
  (insert (format "%s: \n" (orrient-gw2-meta-name meta)))
  (dolist (event (orrient-gw2-meta-events meta))
    (orrient--draw-event event)))

(defun orrient--draw-event (event)
  (insert (format "- %s\n" (orrient-gw2-event-name event))))

(defun orrient--et-render-buffer ()
  (interactive)
  (with-current-buffer (get-buffer-create orrient-et-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (meta orrient-schedule)
        (orrient--draw-meta meta)))))

(define-derived-mode orrient-et-mode special-mode "GW2 Event Timers"
  "View Guild Wars 2 Event Timers."
  :group 'orrient
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

;;; orrient-event-timers.el ends here
