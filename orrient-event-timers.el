;;; orrient-event-timers.el --- Event Timers in GW2 -*- lexical-binding: t; -*-

;; Homepage: https://github.com/purplg/orrient
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Create and organize goals for yourself in Guild Wars 2

;; --------------------
;; Configuration

;;; Code:
(require 'cl-lib)
(require 'generator)

(cl-defstruct orrient-event
  name
  offset
  length
  frequency)

(cl-defstruct orrient-meta
  name
  events)

(defvar orrient-schedule
  `(,(make-orrient-meta :name "Day and Night"
      :events `(,(make-orrient-event :name "Dawn"  :offset  25 :frequency 120 :length  5)
                ,(make-orrient-event :name "Day"   :offset  30 :frequency 120 :length 70)
                ,(make-orrient-event :name "Dusk"  :offset 100 :frequency 120 :length  5)
                ,(make-orrient-event :name "Night" :offset 105 :frequency 120 :length 40)))
    ,(make-orrient-meta :name "World Bosses"
      :events `(,(make-orrient-event :name "Admiral Taidha Covington" :offset   0 :frequency 180 :length 15)
                ,(make-orrient-event :name "Svanir Shaman Chief"      :offset  15 :frequency 120 :length 15)
                ,(make-orrient-event :name "Megadestroyer"            :offset  30 :frequency 180 :length 15)
                ,(make-orrient-event :name "Fire Elemental"           :offset  45 :frequency 120 :length 15)
                ,(make-orrient-event :name "The Shatterer"            :offset  60 :frequency 180 :length 15)
                ,(make-orrient-event :name "Great Jungle Wurm"        :offset  75 :frequency 120 :length 15)
                ,(make-orrient-event :name "Modniir Ulgoth"           :offset  90 :frequency 180 :length 15)
                ,(make-orrient-event :name "Shadow Behemoth"          :offset 105 :frequency 120 :length 15)
                ,(make-orrient-event :name "Golem Mark II"            :offset 120 :frequency 180 :length 15)
                ,(make-orrient-event :name "Claw of Jormag"           :offset 150 :frequency 180 :length 15)))
    ,(make-orrient-meta :name "Hard World Bosses"
      :events `(,(make-orrient-event :name "Tequatl the Sunless" :offset    0 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Triple Trouble"      :offset   60 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Karka Queen"         :offset  120 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Tequatl the Sunless" :offset  180 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Triple Trouble"      :offset  240 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Karka Queen"         :offset  360 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Tequatl the Sunless" :offset  420 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Triple Trouble"      :offset  480 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Karka Queen"         :offset  630 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Tequatl the Sunless" :offset  690 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Triple Trouble"      :offset  750 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Karka Queen"         :offset  900 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Tequatl the Sunless" :offset  960 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Triple Trouble"      :offset 1020 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Karka Queen"         :offset 1080 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Tequatl the Sunless" :offset 1140 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Triple Trouble"      :offset 1200 :frequency 1440 :length 30)
                ,(make-orrient-event :name "Karka Queen"         :offset 1380 :frequency 1440 :length 30)))
    ,(make-orrient-meta :name "Ley-Line Anomaly"
      :events `(,(make-orrient-event :name "Timberline Falls" :offset  20 :frequency 360 :length 20)
                ,(make-orrient-event :name "Iron Marches"     :offset 140 :frequency 360 :length 20)
                ,(make-orrient-event :name "Gendarran Fields" :offset 260 :frequency 360 :length 20)))
    ,(make-orrient-meta :name "PVP Tournaments"
      :events `(,(make-orrient-event :name "Balthazar's Brawl"  :offset   0 :frequency 720 :length 60)
                ,(make-orrient-event :name "Grenth's Game"      :offset 180 :frequency 720 :length 60)
                ,(make-orrient-event :name "Melandru's Matchup" :offset 360 :frequency 720 :length 60)
                ,(make-orrient-event :name "Lyssa's Legions"    :offset 540 :frequency 720 :length 60)))
    ,(make-orrient-meta :name "Eye of the North"
      :events `(,(make-orrient-event :name "Twisted Marionette (Public)"     :offset  0 :frequency 120 :length 20)
                ,(make-orrient-event :name "Battle For Lion's Arch (Public)" :offset 30 :frequency 120 :length 15)
                ,(make-orrient-event :name "Tower of Nightmares (Public)"    :offset 90 :frequency 120 :length 15)))
    ,(make-orrient-meta :name "Dry Top"
      :events `(,(make-orrient-event :name "Crash Site" :offset  0 :frequency 60 :length 40)
                ,(make-orrient-event :name "Sandstore"  :offset 40 :frequency 60 :length 20)))
    ,(make-orrient-meta :name "Verdant Brink"
      :events `(,(make-orrient-event :name "Night: Night and the Enemy"  :offset 105 :frequency 120 :length 25)
                ,(make-orrient-event :name "Night Bosses"                :offset  10 :frequency 120 :length 20)
                ,(make-orrient-event :name "Day: Securing Verdant Brink" :offset  30 :frequency 120 :length 75)))
    ,(make-orrient-meta :name "Auric Basin"
      :events `(,(make-orrient-event :name "Challenges" :offset 45 :frequency 120 :length 15)
                ,(make-orrient-event :name "Octovine"   :offset 60 :frequency 120 :length 20)
                ,(make-orrient-event :name "Reset"      :offset 80 :frequency 120 :length 10)
                ,(make-orrient-event :name "Pylons"     :offset 90 :frequency 120 :length 75))))
  "list of meta events.")

(defvar orrient-et-buffer "*orrient-event-timers*")

(defun orrient--current-time ()
  "Return current time in minutes from UTC 0."
  (let ((time (decode-time nil t nil)))
    (+ (* 60 (decoded-time-hour time))
       (decoded-time-minute time))))

(defun orrient--event-next-occurance (event current-time)
  (let* ((offset (orrient-event-offset event))
         (frequency (orrient-event-frequency event))
         (index (/ current-time frequency))
         (next-start (+ offset (* index frequency)))
         (time-until (- current-time next-start)))
    (when (>= time-until 0)
      (setq next-start (+ offset (* (1+ index) frequency))))
    next-start))

(defun orrient--meta-next-event (meta current-time)
  (let* ((events (orrient-meta-events meta)))
    (mapcar (lambda (event)
              (orrient--event-next-occurance event current-time))
            events)))

(iter-defun orrient--meta-iter (meta time)
  "Yields a cons of a orrient-event to it's next start time."
  (let ((events (orrient-meta-events meta)))
    (while t
      (let* ((upcoming-events (seq-map
                              (lambda (event)
                                (cons event (orrient--event-next-occurance event time)))
                              events))
            (next-event-instance (-reduce (lambda (upcoming-event-a upcoming-event-b)
                                            (if (< (cdr upcoming-event-a)
                                                   (cdr upcoming-event-b)) 
                                                upcoming-event-a
                                              upcoming-event-b))
                                          upcoming-events)))
        (iter-yield next-event-instance)
        (setq time (cdr next-event-instance))))))

(defun orrient--draw-meta (meta)
  (insert (format "%s | " (orrient-meta-name meta)))
  (let ((iter (orrient--meta-iter meta (orrient--current-time))))
    (cl-loop for i upto 5
             do (insert (format "%s, " (orrient-event-name (car (iter-next iter)))))))
  (insert "\n\n"))

(defun orrient--draw-event (event)
  (insert (format "| %s " (orrient-event-name event))))

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
