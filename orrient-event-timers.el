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

(defvar orrient-timers-buffer "*orrient-event-timers*")

(defvar orrient-timers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g r") 'orrient--timers-render-buffer)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    map)
  "Keymap for `orrient-timers-mode'.")


;; Data
(cl-defstruct orrient-timers-event
  name
  offset
  length
  frequency)

(cl-defstruct orrient-timers-meta
  name
  events)

(defvar orrient-timers-schedule
  `(,(make-orrient-timers-meta :name "Day and Night"
      :events `(,(make-orrient-timers-event :name "Dawn"  :offset  25 :frequency 120 :length  5)
                ,(make-orrient-timers-event :name "Day"   :offset  30 :frequency 120 :length 70)
                ,(make-orrient-timers-event :name "Dusk"  :offset 100 :frequency 120 :length  5)
                ,(make-orrient-timers-event :name "Night" :offset 105 :frequency 120 :length 40)))
    ,(make-orrient-timers-meta :name "World Bosses"
      :events `(,(make-orrient-timers-event :name "Admiral Taidha Covington" :offset   0 :frequency 180 :length 15)
                ,(make-orrient-timers-event :name "Svanir Shaman Chief"      :offset  15 :frequency 120 :length 15)
                ,(make-orrient-timers-event :name "Megadestroyer"            :offset  30 :frequency 180 :length 15)
                ,(make-orrient-timers-event :name "Fire Elemental"           :offset  45 :frequency 120 :length 15)
                ,(make-orrient-timers-event :name "The Shatterer"            :offset  60 :frequency 180 :length 15)
                ,(make-orrient-timers-event :name "Great Jungle Wurm"        :offset  75 :frequency 120 :length 15)
                ,(make-orrient-timers-event :name "Modniir Ulgoth"           :offset  90 :frequency 180 :length 15)
                ,(make-orrient-timers-event :name "Shadow Behemoth"          :offset 105 :frequency 120 :length 15)
                ,(make-orrient-timers-event :name "Golem Mark II"            :offset 120 :frequency 180 :length 15)
                ,(make-orrient-timers-event :name "Claw of Jormag"           :offset 150 :frequency 180 :length 15)))
    ,(make-orrient-timers-meta :name "Hard World Bosses"
      :events `(,(make-orrient-timers-event :name "Tequatl the Sunless" :offset    0 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Triple Trouble"      :offset   60 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Karka Queen"         :offset  120 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Tequatl the Sunless" :offset  180 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Triple Trouble"      :offset  240 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Karka Queen"         :offset  360 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Tequatl the Sunless" :offset  420 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Triple Trouble"      :offset  480 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Karka Queen"         :offset  630 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Tequatl the Sunless" :offset  690 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Triple Trouble"      :offset  750 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Karka Queen"         :offset  900 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Tequatl the Sunless" :offset  960 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Triple Trouble"      :offset 1020 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Karka Queen"         :offset 1080 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Tequatl the Sunless" :offset 1140 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Triple Trouble"      :offset 1200 :frequency 1440 :length 30)
                ,(make-orrient-timers-event :name "Karka Queen"         :offset 1380 :frequency 1440 :length 30)))
    ,(make-orrient-timers-meta :name "Ley-Line Anomaly"
      :events `(,(make-orrient-timers-event :name "Timberline Falls" :offset  20 :frequency 360 :length 20)
                ,(make-orrient-timers-event :name "Iron Marches"     :offset 140 :frequency 360 :length 20)
                ,(make-orrient-timers-event :name "Gendarran Fields" :offset 260 :frequency 360 :length 20)))
    ,(make-orrient-timers-meta :name "PVP Tournaments"
      :events `(,(make-orrient-timers-event :name "Balthazar's Brawl"  :offset   0 :frequency 720 :length 60)
                ,(make-orrient-timers-event :name "Grenth's Game"      :offset 180 :frequency 720 :length 60)
                ,(make-orrient-timers-event :name "Melandru's Matchup" :offset 360 :frequency 720 :length 60)
                ,(make-orrient-timers-event :name "Lyssa's Legions"    :offset 540 :frequency 720 :length 60)))
    ,(make-orrient-timers-meta :name "Eye of the North"
      :events `(,(make-orrient-timers-event :name "Twisted Marionette (Public)"     :offset  0 :frequency 120 :length 20)
                ,(make-orrient-timers-event :name "Battle For Lion's Arch (Public)" :offset 30 :frequency 120 :length 15)
                ,(make-orrient-timers-event :name "Tower of Nightmares (Public)"    :offset 90 :frequency 120 :length 15)))
    ,(make-orrient-timers-meta :name "Dry Top"
      :events `(,(make-orrient-timers-event :name "Crash Site" :offset  0 :frequency 60 :length 40)
                ,(make-orrient-timers-event :name "Sandstore"  :offset 40 :frequency 60 :length 20)))
    ,(make-orrient-timers-meta :name "Verdant Brink"
      :events `(,(make-orrient-timers-event :name "Night: Night and the Enemy"  :offset 105 :frequency 120 :length 25)
                ,(make-orrient-timers-event :name "Night Bosses"                :offset  10 :frequency 120 :length 20)
                ,(make-orrient-timers-event :name "Day: Securing Verdant Brink" :offset  30 :frequency 120 :length 75)))
    ,(make-orrient-timers-meta :name "Auric Basin"
      :events `(,(make-orrient-timers-event :name "Challenges" :offset 45 :frequency 120 :length 15)
                ,(make-orrient-timers-event :name "Octovine"   :offset 60 :frequency 120 :length 20)
                ,(make-orrient-timers-event :name "Reset"      :offset 80 :frequency 120 :length 10)
                ,(make-orrient-timers-event :name "Pylons"     :offset 90 :frequency 120 :length 75))))
  "List of meta events.")


;; Event prediction
(defun orrient--timers-current-time ()
  "Return current time in minutes from UTC 0."
  (let ((time (decode-time nil t nil)))
    (+ (* 60 (decoded-time-hour time))
       (decoded-time-minute time))))

(defun orrient--timers-event-next-occurance (event time)
  (let* ((offset (orrient-timers-event-offset event))
         (frequency (orrient-timers-event-frequency event))
         (index (/ time frequency))
         (next-start (+ offset (* index frequency)))
         (time-until (- time next-start)))
    (when (>= time-until 0)
      (setq next-start (+ offset (* (1+ index) frequency))))
    next-start))

(defun orrient--timers-meta-next-event (meta time)
  (let* ((events (orrient-timers-meta-events meta)))
    (mapcar (lambda (event)
              (orrient--timers-event-next-occurance event current-time))
            events)))

(iter-defun orrient--timers-meta-iter (meta time)
  "Yields a cons of a orrient-timers-event to it's next start time."
  (let ((events (orrient-timers-meta-events meta)))
    (while t
      (let* ((upcoming-events (seq-map
                              (lambda (event)
                                (cons event (orrient--timers-event-next-occurance event time)))
                              events))
             (next-event-instance (seq-reduce (lambda (upcoming-event-a upcoming-event-b)
                                                (if (and upcoming-event-a
                                                         (< (cdr upcoming-event-a)
                                                            (cdr upcoming-event-b)))
                                                    upcoming-event-a
                                                  upcoming-event-b))
                                             upcoming-events
                                             nil)))
        (iter-yield next-event-instance)
        (setq time (cdr next-event-instance))))))


;; Rendering
(defun orrient--timers-heading-length ()
  (let ((name-lengths (mapcar (lambda (meta)
                                (cons meta (length (orrient-timers-meta-name meta))))
                              orrient-timers-schedule)))
    (cdr (seq-reduce (lambda (left right)
                       (if (and left
                                (> (cdr left)
                                   (cdr right)))
                           left
                         right))
                     name-lengths
                     nil))))

(defun orrient--timers-draw-meta (meta)
  (insert (format "%s | " (orrient-timers-meta-name meta)))
  (let ((iter (orrient--timers-meta-iter meta (orrient--timers-current-time))))
    (cl-loop for i upto 5
             do (orrient--timers-draw-event (car (iter-next iter)))))
  (insert "\n\n"))

(defun orrient--timers-draw-event (event)
  (insert (format "%s, " (orrient-timers-event-name event))))

(defun orrient--timers-render-buffer ()
  (interactive)
  (with-current-buffer (get-buffer-create orrient-timers-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (meta orrient-timers-schedule)
        (orrient--timers-draw-meta meta)))))

(define-derived-mode orrient-timers-mode special-mode "GW2 Event Timers"
  "View Guild Wars 2 Event Timers."
  :group 'orrient
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

;;; orrient-event-timers.el ends here
