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
(require 'wid-edit)

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
  category
  events)

(defvar orrient-timers-schedule
  `(,(make-orrient-timers-meta :name "Day and Night" :category 'core-tyria
      :events `(,(make-orrient-timers-event :name "Dawn"  :offset  25 :frequency 120 :length  5)
                ,(make-orrient-timers-event :name "Day"   :offset  30 :frequency 120 :length 70)
                ,(make-orrient-timers-event :name "Dusk"  :offset 100 :frequency 120 :length  5)
                ,(make-orrient-timers-event :name "Night" :offset 105 :frequency 120 :length 40)))
    ,(make-orrient-timers-meta :name "World Bosses" :category 'core-tyria
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
    ,(make-orrient-timers-meta :name "Hard World Bosses" :category 'core-tyria
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
    ,(make-orrient-timers-meta :name "Ley-Line Anomaly" :category 'core-tyria
      :events `(,(make-orrient-timers-event :name "Timberline Falls" :offset  20 :frequency 360 :length 20)
                ,(make-orrient-timers-event :name "Iron Marches"     :offset 140 :frequency 360 :length 20)
                ,(make-orrient-timers-event :name "Gendarran Fields" :offset 260 :frequency 360 :length 20)))
    ,(make-orrient-timers-meta :name "PVP Tournaments" :category 'core-tyria
      :events `(,(make-orrient-timers-event :name "Balthazar's Brawl"  :offset   0 :frequency 720 :length 60)
                ,(make-orrient-timers-event :name "Grenth's Game"      :offset 180 :frequency 720 :length 60)
                ,(make-orrient-timers-event :name "Melandru's Matchup" :offset 360 :frequency 720 :length 60)
                ,(make-orrient-timers-event :name "Lyssa's Legions"    :offset 540 :frequency 720 :length 60)))
    ,(make-orrient-timers-meta :name "Eye of the North" :category 'living-world-1
      :events `(,(make-orrient-timers-event :name "Twisted Marionette"     :offset  0 :frequency 120 :length 20)
                ,(make-orrient-timers-event :name "Battle For Lion's Arch" :offset 30 :frequency 120 :length 15)
                ,(make-orrient-timers-event :name "Tower of Nightmares"    :offset 90 :frequency 120 :length 15)))
    ,(make-orrient-timers-meta :name "Scarlets Invasion" :category 'living-world-1
      :events `(,(make-orrient-timers-event :name "TODO" :offset  0 :frequency 120 :length 20)))
    ,(make-orrient-timers-meta :name "Dry Top" :category 'living-world-2
      :events `(,(make-orrient-timers-event :name "Crash Site" :offset  0 :frequency 60 :length 40)
                ,(make-orrient-timers-event :name "Sandstorm"  :offset 40 :frequency 60 :length 20)))
    ,(make-orrient-timers-meta :name "Verdant Brink" :category 'heart-of-thorns
      :events `(,(make-orrient-timers-event :name "Night: Night and the Enemy"  :offset 105 :frequency 120 :length 25)
                ,(make-orrient-timers-event :name "Night Bosses"                :offset  10 :frequency 120 :length 20)
                ,(make-orrient-timers-event :name "Day: Securing Verdant Brink" :offset  30 :frequency 120 :length 75)))
    ,(make-orrient-timers-meta :name "Auric Basin" :category 'heart-of-thorns
      :events `(,(make-orrient-timers-event :name "Challenges" :offset 45 :frequency 120 :length 15)
                ,(make-orrient-timers-event :name "Octovine"   :offset 60 :frequency 120 :length 20)
                ,(make-orrient-timers-event :name "Reset"      :offset 80 :frequency 120 :length 10)
                ,(make-orrient-timers-event :name "Pylons"     :offset 90 :frequency 120 :length 75)))
    ,(make-orrient-timers-meta :name "Tangled Depths" :category 'heart-of-thorns
      :events `(,(make-orrient-timers-event :name "Prep"              :offset 25 :frequency 120 :length  5)
                ,(make-orrient-timers-event :name "Chak Gerent"       :offset 30 :frequency 120 :length 20)
                ,(make-orrient-timers-event :name "Help the Outposts" :offset 50 :frequency 120 :length 95)))
    ,(make-orrient-timers-meta :name "Dragons Stand" :category 'heart-of-thorns
      :events `(,(make-orrient-timers-event :name "Dragon's Stand" :offset 90 :frequency 120 :length 120)))
    ,(make-orrient-timers-meta :name "Lake Doric" :category 'living-world-3
      :events `(,(make-orrient-timers-event :name "Noran's Homestead" :offset  30 :frequency 120 :length 30)
                ,(make-orrient-timers-event :name "Saidra's Haven"    :offset  60 :frequency 120 :length 45)
                ,(make-orrient-timers-event :name "New Loamhurst"     :offset 105 :frequency 120 :length 45)))
    ,(make-orrient-timers-meta :name "Crystal Oasis" :category 'path-of-fire
      :events `(,(make-orrient-timers-event :name "Rounds 1 to 3" :offset  5 :frequency 120 :length 10)
                ,(make-orrient-timers-event :name "Pinata/Reset"  :offset 20 :frequency 120 :length 10)))
    ,(make-orrient-timers-meta :name "Desert Highlands" :category 'path-of-fire
      :events `(,(make-orrient-timers-event :name "Buried Treasure" :offset 5 :frequency 120 :length 10))))
  "List of meta events.")

(defvar orrient--timers-heading-length nil)

(defvar orrient--timers-event-length 20)


;; Faces
(defface orrient-timers-meta
  '((t (:weight bold)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-core-tyria
  '((t (:background "#7b0418" :inherit 'orrient-timers-meta)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-1
  '((t (:background "#7b0418" :inherit 'orrient-timers-meta)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-2
  '((t (:background "#5c4a03" :inherit 'orrient-timers-meta)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-heart-of-thorns
  '((t (:background "#515c03" :inherit 'orrient-timers-meta)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-3
  '((t (:background "#4b7f40" :inherit 'orrient-timers-meta)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-path-of-fire
  '((t (:background "#7b4704" :inherit 'orrient-timers-meta)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-4
  '((t (:background "#662a77" :inherit 'orrient-timers-meta)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-icebrood-saga
  '((t (:background "#04497b" :inherit 'orrient-timers-meta)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-end-of-dragons
  '((t (:background "#0b6b75" :inherit 'orrient-timers-meta)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-event
  '((t (:inherit outline-1)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defun orrient--timers-get-category-face (category)
  (pcase category
    ('core-tyria 'orrient-timers-category-core-tyria)
    ('living-world-1 'orrient-timers-category-living-world-1)
    ('living-world-2 'orrient-timers-category-living-world-2)
    ('heart-of-thorns 'orrient-timers-category-heart-of-thorns)
    ('living-world-3 'orrient-timers-category-living-world-3)
    ('path-of-fire 'orrient-timers-category-path-of-fire)
    ('living-world-4 'orrient-timers-category-living-world-4)
    ('icebrood-saga 'orrient-timers-category-icebrood-saga)
    ('end-of-dragons 'orrient-timers-category-end-of-dragons)))



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


;; Widgets
(define-widget 'orrient-timers-event 'push-button
  ""
  :create 'orrient-timers-event-widget-create)

(define-widget 'orrient-timers-meta 'group
  ""
  :create 'orrient-timers-meta-widget-create)

(define-widget 'orrient-timers-countdown 'push-button
  ""
  :create 'orrient-timers-countdown-widget-create)

(defun orrient-timers-meta-widget-create (widget)
  (let ((meta (widget-get widget :meta)))
    ;; (widget-put widget :action (orrient-timers-open-meta meta)) ; TODO Add new buffer for more detailed information on meta
    (widget-put widget :tag (propertize (format (format "%%%ss " (orrient--timers-heading-length))
                                                (orrient-timers-meta-name meta))
                                        'face (orrient--timers-get-category-face (orrient-timers-meta-category meta))))
    (widget-put widget :format "%t %v"))

  (widget-default-create widget))

(defun orrient-timers-event-widget-create (widget)
  (let ((event (widget-get widget :value)))
    (widget-put widget :tag (string-limit (format
                                           (format "%%-%ss " orrient--timers-event-length)
                                           (orrient-timers-event-name event))
                                          orrient--timers-event-length))
    (widget-put widget :format "%[%t%]"))
  (widget-default-create widget))


;; Rendering
(defun orrient--timers-heading-length ()
  (or orrient--timers-heading-length
      (setq orrient--timers-heading-length
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
                               nil))))))

(defun orrient--timers-draw-meta (meta)
  (widget-create
   (append `(orrient-timers-meta :meta ,meta)
           (orrient--timers-upcoming-events-widgets meta)))
  (insert "\n"))

(defun orrient--timers-upcoming-events-widgets (meta)
  (let ((iter (orrient--timers-meta-iter meta (orrient--timers-current-time))))
    (cl-loop repeat 5 collect
             `(orrient-timers-event ,(car (iter-next iter))))))

(defun orrient--timers-render-buffer ()
  (interactive)
  (with-current-buffer (get-buffer-create orrient-timers-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (meta orrient-timers-schedule)
        (orrient--timers-draw-meta meta)))
    (orrient-timers-mode)))

(define-derived-mode orrient-timers-mode special-mode "GW2 Event Timers"
  "View Guild Wars 2 Event Timers."
  :group 'orrient
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

;;; orrient-event-timers.el ends here
