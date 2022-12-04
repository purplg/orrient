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
    (define-key map (kbd "C-n") 'orrient-timers-forward)
    (define-key map (kbd "C-p") 'orrient-timers-backward)
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    map)
  "Keymap for `orrient-timers-mode'.")

(defcustom orrient-timers-skip-step 5
  "Amount of time to skip when stepping forward or backwards in
time.")


;; User functions
(defun orrient-timers-forward (&optional step)
  (interactive)
  (unless step
    (setq step orrient-timers-skip-step))
  (orrient--timers-render-buffer-at-time (+ orrient-timers-time step)))

(defun orrient-timers-backward (&optional step)
  (interactive)
  (unless step
    (setq step orrient-timers-skip-step))
  (orrient--timers-render-buffer-at-time (- orrient-timers-time step)))


;; Data
(cl-defstruct
    (orrient-event
     (:constructor make-orrient-event
                   (&key name offset length frequency
                    &aux
                    (offset (+ (* 60 (pop offset))
                               (pop offset)))
                    (length (+ (* 60 (pop length))
                               (pop length)))
                    (frequency (+ (* 60 (pop frequency))
                                  (pop frequency))))))
  name
  offset
  length
  frequency)

(cl-defstruct orrient-meta
  name
  category
  events)

(defvar orrient-timers-schedule
  `(,(make-orrient-meta :name "Day and Night" :category 'core-tyria
      :events `(,(make-orrient-event :name "Dawn"  :offset '(0 25) :frequency '(2 0) :length '(0  5))
                ,(make-orrient-event :name "Day"   :offset '(0 30) :frequency '(2 0) :length '(1 10))
                ,(make-orrient-event :name "Dusk"  :offset '(1 40) :frequency '(2 0) :length '(0  5))
                ,(make-orrient-event :name "Night" :offset '(1 45) :frequency '(2 0) :length '(0 40))))
    ,(make-orrient-meta :name "World Bosses" :category 'core-tyria
      :events `(,(make-orrient-event :name "Admiral Taidha Covington" :offset '(0  0) :frequency '(3 0) :length '(0 15))
                ,(make-orrient-event :name "Svanir Shaman Chief"      :offset '(0 15) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "Megadestroyer"            :offset '(0 30) :frequency '(3 0) :length '(0 15))
                ,(make-orrient-event :name "Fire Elemental"           :offset '(0 45) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "The Shatterer"            :offset '(1  0) :frequency '(3 0) :length '(0 15))
                ,(make-orrient-event :name "Great Jungle Wurm"        :offset '(1 15) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "Modniir Ulgoth"           :offset '(1 30) :frequency '(3 0) :length '(0 15))
                ,(make-orrient-event :name "Shadow Behemoth"          :offset '(1 45) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "Golem Mark II"            :offset '(2  0) :frequency '(3 0) :length '(0 15))
                ,(make-orrient-event :name "Claw of Jormag"           :offset '(2 30) :frequency '(2 0) :length '(0 15))))
    ,(make-orrient-meta :name "Hard World Bosses" :category 'core-tyria
      :events `(,(make-orrient-event :name "Tequatl the Sunless" :offset '( 0 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Triple Trouble"      :offset '( 1 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Karka Queen"         :offset '( 2 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Tequatl the Sunless" :offset '( 3 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Triple Trouble"      :offset '( 4 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Karka Queen"         :offset '( 6 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Tequatl the Sunless" :offset '( 7 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Triple Trouble"      :offset '( 8 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Karka Queen"         :offset '(10 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Tequatl the Sunless" :offset '(11 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Triple Trouble"      :offset '(12 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Karka Queen"         :offset '(15 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Tequatl the Sunless" :offset '(16 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Triple Trouble"      :offset '(17 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Karka Queen"         :offset '(18 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Tequatl the Sunless" :offset '(19 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Triple Trouble"      :offset '(20 0) :frequency '(24 0) :length '(0 30))
                ,(make-orrient-event :name "Karka Queen"         :offset '(23 0) :frequency '(24 0) :length '(0 30))))
    ,(make-orrient-meta :name "Ley-Line Anomaly" :category 'core-tyria
      :events `(,(make-orrient-event :name "Timberline Falls" :offset '(0 20) :frequency '(6 0) :length '(0 20))
                ,(make-orrient-event :name "Iron Marches"     :offset '(2 20) :frequency '(6 0) :length '(0 20))
                ,(make-orrient-event :name "Gendarran Fields" :offset '(4 20) :frequency '(6 0) :length '(0 20))))
    ,(make-orrient-meta :name "PVP Tournaments" :category 'core-tyria
      :events `(,(make-orrient-event :name "balthazar's brawl"  :offset '(0 0) :frequency '(12 0) :length '(1 0))
                ,(make-orrient-event :name "grenth's game"      :offset '(3 0) :frequency '(12 0) :length '(1 0))
                ,(make-orrient-event :name "melandru's matchup" :offset '(6 0) :frequency '(12 0) :length '(1 0))
                ,(make-orrient-event :name "Lyssa's Legions"    :offset '(9 0) :frequency '(12 0) :length '(1 0))))
    ,(make-orrient-meta :name "Eye of the North" :category 'living-world-1
      :events `(,(make-orrient-event :name "Twisted Marionette"     :offset '(0  0) :frequency '(2 0) :length '(0 20))
                ,(make-orrient-event :name "Battle For Lion's Arch" :offset '(0 30) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "Tower of Nightmares"    :offset '(1 30) :frequency '(2 0) :length '(0 15))))
    ,(make-orrient-meta :name "Scarlets Invasion" :category 'living-world-1
      :events `(,(make-orrient-event :name "TODO" :offset '(0  0) :frequency '(2 0) :length '(0 20))))
    ,(make-orrient-meta :name "Dry Top" :category 'living-world-2
      :events `(,(make-orrient-event :name "Crash Site" :offset '(0  0) :frequency '(1 0) :length '(0 40))
                ,(make-orrient-event :name "Sandstorm"  :offset '(0 40) :frequency '(1 0) :length '(0 20))))
    ,(make-orrient-meta :name "Verdant Brink" :category 'heart-of-thorns
      :events `(,(make-orrient-event :name "Night: Night and the Enemy"  :offset '(1 45) :frequency '(2 0) :length '(0 25))
                ,(make-orrient-event :name "Night Bosses"                :offset '(0 10) :frequency '(2 0) :length '(0 20))
                ,(make-orrient-event :name "Day: Securing Verdant Brink" :offset '(0 30) :frequency '(2 0) :length '(1 15))))
    ,(make-orrient-meta :name "Auric Basin" :category 'heart-of-thorns
      :events `(,(make-orrient-event :name "Challenges" :offset '(0 45) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "Octovine"   :offset '(1  0) :frequency '(2 0) :length '(0 20))
                ,(make-orrient-event :name "Reset"      :offset '(1 20) :frequency '(2 0) :length '(0 10))
                ,(make-orrient-event :name "Pylons"     :offset '(1 30) :frequency '(2 0) :length '(1 15))))
    ,(make-orrient-meta :name "Tangled Depths" :category 'heart-of-thorns
      :events `(,(make-orrient-event :name "Prep"              :offset '(0 25) :frequency '(2 0) :length '(0  5))
                ,(make-orrient-event :name "Chak Gerent"       :offset '(0 30) :frequency '(2 0) :length '(0 20))
                ,(make-orrient-event :name "Help the Outposts" :offset '(0 50) :frequency '(2 0) :length '(1 35))))
    ,(make-orrient-meta :name "Dragons Stand" :category 'heart-of-thorns
      :events `(,(make-orrient-event :name "Dragon's Stand" :offset '(0 90) :frequency '(2 0) :length '(2 0))))
    ,(make-orrient-meta :name "Lake Doric" :category 'living-world-3
      :events `(,(make-orrient-event :name "Noran's Homestead" :offset '(0 30) :frequency '(2 0) :length '(0 30))
                ,(make-orrient-event :name "Saidra's Haven"    :offset '(1  0) :frequency '(2 0) :length '(0 45))
                ,(make-orrient-event :name "New Loamhurst"     :offset '(1 45) :frequency '(2 0) :length '(0 45))))
    ,(make-orrient-meta :name "Crystal Oasis" :category 'path-of-fire
      :events `(,(make-orrient-event :name "Rounds 1 to 3" :offset '(0  5) :frequency '(2 0) :length '(0 10))
                ,(make-orrient-event :name "Pinata/Reset"  :offset '(0 20) :frequency '(2 0) :length '(0 10))))
    ,(make-orrient-meta :name "Desert Highlands" :category 'path-of-fire
      :events `(,(make-orrient-event :name "Buried Treasure" :offset '(0 5) :frequency '(2 0) :length '(0 10)))))
  "List of meta events.")

(defvar orrient--timers-heading-length nil)

(defvar orrient--timers-event-length 20)

(defvar orrient-timers-time nil)


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

(defun orrient--timers-event-next (event time)
  "Returns next event occurance in minutes offset from UTC 0."
  (let* ((offset (orrient-event-offset event))
         (frequency (orrient-event-frequency event))
         (index (/ time frequency))
         (next-start (+ offset (* index frequency)))
         (time-until (- time next-start)))
    (when (>= time-until 0)
      (setq next-start (+ offset (* (1+ index) frequency))))
    next-start))

(defun orrient--timers-meta-next-event (meta time)
  "Returns a cons of the next `orrient-event' and minutes of
it's next occurance from UTC 0."
  (let ((event-times (mapcar (lambda (event)
                               (cons event (orrient--timers-event-next event time)))
                             (orrient-meta-events meta))))
    (seq-reduce (lambda (a b)
                  (if (and a
                       (< (cdr a) (cdr b)))
                      a
                    b))
                event-times
                nil)))

(iter-defun orrient--timers-meta-iter (meta time)
  "Yields a cons of a orrient-event to it's next start time."
  (let ((events (orrient-meta-events meta)))
    (while t
      (let* ((upcoming-events (seq-map
                              (lambda (event)
                                (cons event (orrient--timers-event-next event time)))
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
(define-widget 'orrient-timers-meta 'group
  ""
  :create 'orrient-timers-meta-widget-create
  :format "%t %v")

(defun orrient-timers-meta-widget-create (widget)
  (let ((meta (widget-get widget :meta)))
    ;; (widget-put widget :action (orrient-timers-open-meta meta)) ; TODO Add new buffer for more detailed information on meta
    (widget-put widget :tag (propertize (format (format "%%%ss " (1+ (orrient--timers-heading-length)))
                                                (orrient-meta-name meta))
                                        'face (orrient--timers-get-category-face (orrient-meta-category meta)))))
  (widget-default-create widget))

(define-widget 'orrient-timers-event 'push-button
  ""
  :create 'orrient-timers-event-widget-create
  :format "%[%t%]")

(defun orrient-timers-event-widget-create (widget)
  (let ((event (widget-get widget :value)))
    (widget-put widget :tag (string-limit (format
                                           (format "%%-%ss " orrient--timers-event-length)
                                           (orrient-event-name event))
                                          orrient--timers-event-length)))
  (widget-default-create widget))

(define-widget 'orrient-timers-countdown 'push-button
  ""
  :create 'orrient-timers-countdown-widget-create
  :value-create 'orrient-timers-countdown-widget-value-create
  :time (orrient--timers-current-time)
  :format "%v")

(defun orrient-timers-countdown-widget-value-create (widget)
  "Format the remaining time into hours and minutes."
  (let* ((event-occurance (widget-get widget :value))
         (time (widget-get widget :time))
         (time-until (- (cdr event-occurance) time))
         (hours (/ time-until 60))
         (minutes (% time-until 60)))
    (insert
     (format "%s %s "
             (if (> hours 0)
                 (format "%2dh" hours)
               "   ")
             (format "%02dm" minutes)))))

(defun orrient-timers-countdown-widget-create (widget)
  (let ((meta (widget-get widget :value))
        (time (widget-get widget :time)))
    (widget-put widget :value (orrient--timers-meta-next-event meta time)))
  (widget-default-create widget))


;; Rendering
(defun orrient--timers-heading-length ()
  (or orrient--timers-heading-length
      (setq orrient--timers-heading-length
            (let ((name-lengths (mapcar (lambda (meta)
                                          (cons meta (length (orrient-meta-name meta))))
                                        orrient-timers-schedule)))
              (cdr (seq-reduce (lambda (left right)
                                 (if (and left
                                          (> (cdr left)
                                             (cdr right)))
                                     left
                                   right))
                               name-lengths
                               nil))))))

(defun orrient--timers-draw-meta (meta time)
  (widget-create
   (append `(orrient-timers-meta :meta ,meta)
           (orrient--timers-upcoming-events-widgets meta time)))
  (insert "\n"))

(defun orrient--timers-upcoming-events-widgets (meta time)
  (let ((iter (orrient--timers-meta-iter meta time)))
    (append
     `((orrient-timers-countdown :time ,time ,meta))
     (cl-loop repeat 5 collect
              `(orrient-timers-event ,(car (iter-next iter)))))))

(defun orrient--timers-render-buffer ()
  (with-current-buffer (get-buffer-create orrient-timers-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((hours (/ orrient-timers-time 60))
            (minutes (% orrient-timers-time 60)))
        (insert (format "Current time: %02d:%02d UTC\n" hours minutes)))
      (dolist (meta orrient-timers-schedule)
        (orrient--timers-draw-meta meta orrient-timers-time))
      (orrient-timers-mode))))

(defun orrient--timers-render-buffer-at-time (time)
  (setq orrient-timers-time (% time 1440))
  (orrient--timers-render-buffer))

(defun orrient-timers-open ()
  (interactive)
  (orrient--timers-render-buffer-at-time (or orrient-timers-time
                                             (orrient--timers-current-time)))
  (let* ((buffer (get-buffer orrient-timers-buffer))
         (window (get-buffer-window buffer)))
    (pop-to-buffer buffer)
    (set-window-dedicated-p window t)))

(define-derived-mode orrient-timers-mode special-mode "GW2 Event Timers"
  "View Guild Wars 2 Event Timers."
  :group 'orrient
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

;;; orrient-event-timers.el ends here
