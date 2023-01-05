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

(defmacro orrient--timers-with-buffer (&rest body)
  "Like `with-current-buffer' but with `orrient-timers-buffer'.
BODY is evaluated with `orrient-timers-buffer'"
  `(when-let ((buffer (get-buffer orrient-timers-buffer)))
    (with-current-buffer buffer
      ,@body)))

(defvar orrient-timers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'widget-button-press)
    (define-key map (kbd "C-n") #'orrient-timers-forward)
    (define-key map (kbd "C-p") #'orrient-timers-backward)
    (define-key map [tab] #'widget-forward)
    (define-key map [backtab] #'widget-backward)
    map)
  "Keymap for `orrient-timers-mode'.")

(when (featurep 'evil)
  (add-hook 'orrient-timers-mode-hook
            (lambda ()
              (orrient--timers-with-buffer
               (evil-local-set-key 'normal (kbd "]") 'orrient-timers-forward)
               (evil-local-set-key 'normal (kbd "[") 'orrient-timers-backward)
               (evil-local-set-key 'normal (kbd "gt") 'orrient-timers-goto)
               (evil-local-set-key 'normal (kbd "gk") 'orrient-timers-forward)
               (evil-local-set-key 'normal (kbd "gj") 'orrient-timers-backward)))))

(defcustom orrient-timers-skip-step 15
  "Amount of time to skip when stepping forward or backwards in
time.")

(defcustom orrient-timers-snap-to-step t
  "Whether to snap time to `orrient-timers-skip-step' increments.
For example, if the time is currently 01:04 and
`orrient-timers-skip-step' is set to the default 15, then going
forward in time by calling `orrient-timers-forward' will snap to
01:15.")


;; User functions
(defun orrient-timers-forward (&optional step)
  "Skip forward in time by `orrient-timers-skip-step'.
Specify STEP to forward by that amount instead."
  (interactive)
  (orrient--timers-timer-cancel)
  (let ((step (or step orrient-timers-skip-step))
        (time (orrient--timers-time)))
    (orrient--timers-update
     (% (if orrient-timers-snap-to-step
            (* (1+ (/ time step)) step)
          (+ time step)) 1440))))

(defun orrient-timers-backward (&optional step)
  "Skip backward in time by `orrient-timers-skip-step'.
Specify STEP to forward by that amount instead."
  (interactive)
  (orrient--timers-timer-cancel)
  (let ((step (or step orrient-timers-skip-step))
        (time (orrient--timers-time)))
    (orrient--timers-update
     (if orrient-timers-snap-to-step
         (* (1- (/ time step)) step)
       (- time step)))))

(defun orrient-timers-now (&rest _)
  "Jump to the current system time."
  (interactive)
  (orrient--timers-timer-start)
  (orrient--timers-update (orrient--timers-current-time)))

(defun orrient-timers-goto (time)
  "Jump to a specified time.
TIME is in ISO 8601 format as specified by `parse-time-string'"
  (interactive
   (let ((user-time (parse-time-string (format "%s" (read-from-minibuffer "Time (UTC): ")))))
     (list (+ (* (decoded-time-hour user-time) 60)
              (decoded-time-minute user-time)))))
  (orrient--timers-timer-cancel)
  (orrient--timers-update time))


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
  "A generic event with a repeating schedule."
  name
  offset
  length
  frequency)

(cl-defstruct orrient-event-instance
  "A specific occurance of `orrient-event' with a start and end time"
  event
  start
  end)

(cl-defstruct orrient-meta
  "A collection of events to create a meta schedule."
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
      :events `(,(make-orrient-event :name "Balthazar's Brawl"  :offset '(0 0) :frequency '(12 0) :length '(1 0))
                ,(make-orrient-event :name "Grenth's Game"      :offset '(3 0) :frequency '(12 0) :length '(1 0))
                ,(make-orrient-event :name "Melandru's Matchup" :offset '(6 0) :frequency '(12 0) :length '(1 0))
                ,(make-orrient-event :name "Lyssa's Legions"    :offset '(9 0) :frequency '(12 0) :length '(1 0))))
    ,(make-orrient-meta :name "Eye of the North" :category 'living-world-1
      :events `(,(make-orrient-event :name "Twisted Marionette"     :offset '(0  0) :frequency '(2 0) :length '(0 20))
                ,(make-orrient-event :name "Battle For Lion's Arch" :offset '(0 30) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "Tower of Nightmares"    :offset '(1 30) :frequency '(2 0) :length '(0 15))))
    ,(make-orrient-meta :name "Scarlets Invasion" :category 'living-world-1
      :events `(,(make-orrient-event :name "Defeat Scarlets minions" :offset '(1  0) :frequency '(2 0) :length '(0 15))))
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
      :events `(,(make-orrient-event :name "Buried Treasure" :offset '(0 5) :frequency '(2 0) :length '(0 10))))
    ,(make-orrient-meta :name "Elon Riverlands" :category 'path-of-fire
      :events `(,(make-orrient-event :name "The Path to Ascension: Augury Rock" :offset '(1 30) :frequency '(2 0) :length '(0 25))
                ,(make-orrient-event :name "Doppelganger"                       :offset '(1 50) :frequency '(2 0) :length '(0 20))))
    ,(make-orrient-meta :name "The Desolation" :category 'path-of-fire
      :events `(,(make-orrient-event :name "Junudu Rising"   :offset '(0 30) :frequency '(1 0) :length '(0 20))
                ,(make-orrient-event :name "Maws of Torment" :offset '(1  0) :frequency '(2 0) :length '(0 20))))
    ,(make-orrient-meta :name "Domain of Vabbi" :category 'path-of-fire
      :events `(,(make-orrient-event :name "Forged with Fire" :offset '(0  0) :frequency '(2 0) :length '(0 30))
                ,(make-orrient-event :name "Serpents Ire"     :offset '(0 30) :frequency '(2 0) :length '(0 30))))
    ,(make-orrient-meta :name "Domain of Istan" :category 'living-world-4
      :events `(,(make-orrient-event :name "Palawadan" :offset '(1 45) :frequency '(2 0) :length '(0 30))))
    ,(make-orrient-meta :name "Jahai Bluffs" :category 'living-world-4
      :events `(,(make-orrient-event :name "Escorts"                 :offset '(1  0) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "Death-Branded Shatterer" :offset '(1 15) :frequency '(2 0) :length '(0 15))))
    ,(make-orrient-meta :name "Thunderhead Peaks" :category 'living-world-4
      :events `(,(make-orrient-event :name "The Oil Floes"    :offset '(0 45) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "Thunderhead Keep" :offset '(1 45) :frequency '(2 0) :length '(0 20))))
    ,(make-orrient-meta :name "Grothmar Valley" :category 'icebrood-saga
      :events `(,(make-orrient-event :name "Effigy"          :offset '(0 10) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "Doomlore Shrine" :offset '(0 38) :frequency '(2 0) :length '(0 22))
                ,(make-orrient-event :name "Ooze Pits"       :offset '(1  5) :frequency '(2 0) :length '(0 20))
                ,(make-orrient-event :name "Metal Concert"   :offset '(1 40) :frequency '(2 0) :length '(0 20))))
    ,(make-orrient-meta :name "Bjora Marches" :category 'icebrood-saga
      :events `(,(make-orrient-event :name "Shards and Construct"            :offset '(0  0) :frequency '(2 0) :length '(0  5))
                ,(make-orrient-event :name "Icebrood Champions"              :offset '(0  5) :frequency '(2 0) :length '(0 15))
                ,(make-orrient-event :name "Drakkar and Spirits of the Wild" :offset '(1  5) :frequency '(2 0) :length '(0 35))
                ,(make-orrient-event :name "Raven Shrines"                   :offset '(1 45) :frequency '(2 0) :length '(0 15))))
    ,(make-orrient-meta :name "Dragonstorm" :category 'icebrood-saga
      :events `(,(make-orrient-event :name "Dragonstorm" :offset '(1  0) :frequency '(2 0) :length '(0 20))))
    ,(make-orrient-meta :name "Cantha: Day and Night" :category 'end-of-dragons
      :events `(,(make-orrient-event :name "Dawn"  :offset '(0 25) :frequency '(2 0) :length '(0  5))
                ,(make-orrient-event :name "Day"   :offset '(0 30) :frequency '(2 0) :length '(1 10))
                ,(make-orrient-event :name "Dusk"  :offset '(1 40) :frequency '(2 0) :length '(0  5))
                ,(make-orrient-event :name "Night" :offset '(1 45) :frequency '(2 0) :length '(0 40))))
    ,(make-orrient-meta :name "Seitung Province" :category 'end-of-dragons
      :events `(,(make-orrient-event :name "Aetherblade Assault" :offset '(1 30) :frequency '(2 0) :length '(0 30))))
    ,(make-orrient-meta :name "New Kaineng City" :category 'end-of-dragons
      :events `(,(make-orrient-event :name "Kaineng Blackout" :offset '(0 0) :frequency '(2 0) :length '(0 40))))
    ,(make-orrient-meta :name "The Echovald Wilds" :category 'end-of-dragons
      :events `(,(make-orrient-event :name "Gang War"  :offset '(0 30) :frequency '(2 0) :length '(0 35))
                ,(make-orrient-event :name "Aspenwood" :offset '(1 40) :frequency '(2 0) :length '(0 20))))
    ,(make-orrient-meta :name "Dragons End" :category 'end-of-dragons
      :events `(,(make-orrient-event :name "Jade Maw"                    :offset '(0  5) :frequency '(2 0) :length '(0  8))
                ,(make-orrient-event :name "Preparations"                :offset '(0 13) :frequency '(2 0) :length '(0 32))
                ,(make-orrient-event :name "Jade Maw"                    :offset '(0 45) :frequency '(2 0) :length '(0  8))
                ,(make-orrient-event :name "Preparations"                :offset '(0 53) :frequency '(2 0) :length '(0  8))
                ,(make-orrient-event :name "The Battle for the Jade Sea" :offset '(1  0) :frequency '(2 0) :length '(1  0)))))
  "List of meta events.")

(defvar orrient--timers-category-order
  '(core-tyria
    living-world-1
    living-world-2
    heart-of-thorns
    living-world-3
    path-of-fire
    living-world-4
    icebrood-saga
    end-of-dragons)
  "The order the table is sorted when sorted by 'Category'.")

(defun orrient--timers-category-sort (entry-a entry-b)
  "Predicate for `sort' that sorts categories by `orrient--timers-category-order'
Return t when ENTRY-A is before COL-B."
  (let ((category-a-id (plist-get (cdr (aref (nth 1 entry-a) 1)) 'orrient-category-id))
        (category-b-id (plist-get (cdr (aref (nth 1 entry-b) 1)) 'orrient-category-id)))
    (< (cl-position category-a-id orrient--timers-category-order)
       (cl-position category-b-id orrient--timers-category-order))))

(defun orrient--timers-category-name (category)
  "Convert the CATEGORY symbol into a human-readable string."
  (pcase category
    ('core-tyria "Core Tyria")
    ('living-world-1 "Living World S1")
    ('living-world-2 "Living World S2")
    ('heart-of-thorns "Heart of Thorns")
    ('living-world-3 "Living World S3")
    ('path-of-fire "Path of Fire")
    ('living-world-4 "Living World S4")
    ('icebrood-saga "The Icebrood Saga")
    ('end-of-dragons "End of Dragons")))

(defun orrient--timers-event-instance-sort (event-column entry-a entry-b)
  "Predicate for `sort' that sorts events by how soon they will occur next.
Return t when ENTRY-A comes before COL-B."
  (let ((category-a-minutes (orrient-event-instance-start (plist-get (cdr (aref (nth 1 entry-a) event-column)) 'orrient-event-instance)))
        (category-b-minutes (orrient-event-instance-start (plist-get (cdr (aref (nth 1 entry-b) event-column)) 'orrient-event-instance))))
    (< category-a-minutes category-b-minutes)))


;; Timers
(defvar orrient--timers-timer nil
  "Instance of the refresh timer.
If non-nil, then a `run-with-timer' timer is active.")

(defun orrient--timers-timer-start ()
  "Start the live update timer at the top of every minute."
  (orrient--timers-timer-cancel)
  (setq orrient--timers-timer
        (run-with-timer (- 60 (decoded-time-second (decode-time nil t nil)))
                        60
                        (lambda ()
                          (orrient--timers-with-buffer
                           (orrient--timers-update (orrient--timers-current-time)))))))

(defun orrient--timers-timer-cancel ()
  "Stop the live update timer if it's running."
  (when orrient--timers-timer
    (cancel-timer orrient--timers-timer)
    (setq orrient--timers-timer nil)))

(defun orrient--timers-timer-toggle ()
  "Toggle the live update timer."
  (if orrient--timers-timer
      (orrient--timers-timer-cancel)
    (orrient--timers-timer-start))
  (save-excursion
    (orrient--timers-render-buffer)))

(defun orrient--timers-current-time ()
  "Return current time in minutes from UTC 0."
  (let ((time (decode-time nil t nil)))
    (+ (* 60 (decoded-time-hour time))
       (decoded-time-minute time))))


;; Faces
(defface orrient-timers-category
  '((t (:extend t)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-core-tyria
  '((t (:background "#3f010c" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-1
  '((t (:background "#7b0418" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-2
  '((t (:background "#5c4a03" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-heart-of-thorns
  '((t (:background "#515c03" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-3
  '((t (:background "#4b7f40" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-path-of-fire
  '((t (:background "#7b4704" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-4
  '((t (:background "#662a77" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-icebrood-saga
  '((t (:background "#04497b" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-end-of-dragons
  '((t (:background "#0b6b75" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defun orrient--timers-get-category-face (category)
  "Convert the CATEGORY symbol into a face."
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

;; Countdown faces
(defface orrient-timers-countdown-now
  '((t (:inherit 'error)))
  "Orrient face for time remaining when an event is happening now."
  :group 'orrient)

(defface orrient-timers-countdown-soon
  '((t (:inherit 'warning)))
  "Orrient face for time remaining when an event is happening soon."
  :group 'orrient)

(defface orrient-timers-countdown-later
  '((t ()))
  "Orrient face for time remaining when an event is not happening soon."
  :group 'orrient)

(defun orrient--timers-get-countdown-face (minutes)
  (cond ((<= minutes 0) 'orrient-timers-countdown-now)
        ((< minutes 15) 'orrient-timers-countdown-soon)
        (t 'orrient-timers-countdown-later)))


;; Event prediction
(defun orrient--timers-event-next (event time)
  "Returns the next `orrient-event-instance' of EVENT starting from
TIME in minutes from UTC 0."
  (let* ((offset (orrient-event-offset event))
         (frequency (orrient-event-frequency event))
         (length (orrient-event-length event))
         (start-time (+ offset (* (/ time frequency) frequency)))
         (end-time (+ start-time length)))
    (while (<= end-time time)
      (setq start-time (+ start-time frequency))
      (setq end-time (+ start-time length)))
    (make-orrient-event-instance :event event
                                 :start start-time
                                 :end end-time)))

(iter-defun orrient--timers-event-iter (event time &optional reset-new-day)
  "Returns next start time `orrient-event', in minutes, from UTC 0."
  (let* ((offset (orrient-event-offset event))
         (frequency (orrient-event-frequency event))
         (index (/ time frequency))
         (start-time (+ offset (* index frequency))))

    ; If event has passed, skip to next occurance.
    (while (> time start-time)
      (setq index (1+ index)))

    (while t
      (setq start-time (+ offset (* index frequency)))

      (when (and reset-new-day
                 (>= start-time 1440))
        (setq index 0)
        (setq start-time offset))

      (iter-yield (make-orrient-event-instance :event event
                                               :start start-time
                                               :end (+ start-time (orrient-event-length event))))
      (setq index (1+ index)))))

(iter-defun orrient--timers-meta-iter (meta time)
  "Returns a `orrient-event-instance' of the next `orrient-event' to occur."
  (let ((events (orrient-meta-events meta)))
    (while t
      (let* ((event-times (mapcar (lambda (event)
                                    (orrient--timers-event-next event time))
                                  events))
             (next-event (seq-reduce
                          (lambda (a b)
                            (if (and a
                                     (< (orrient-event-instance-start a)
                                        (orrient-event-instance-start b)))
                                a
                              b))
                          event-times
                          nil)))
        (iter-yield next-event)
        (setq time (orrient-event-instance-end next-event))))))


;; Rendering
(defvar-local orrient-timers-time nil
  "The current time rendered in the table.")

(defvar-local orrient--timers-heading-length nil
  "Length of the longest meta name.")

(defvar-local orrient--timers-event-length 20
  "Length of the longest event name.")

(defun orrient--timers-time ()
  "Returns the current time being rendered."
  (or orrient-timers-time
      (orrient--timers-current-time)))

(defun orrient--timers-format-eta (minutes)
  "Format an ETA shown on an event of its next occurance."
  (let ((hours (/ minutes 60))
        (minutes (% minutes 60)))
    ;; 6 is select here because the longest possible time between events is 2
    ;; hours and "2h 00m" is 6 characters. So we normalize all timestamps to 6
    ;; characters.
    (format "%6s" (if (> hours 0)
	              (format "%dh %02dm" hours minutes)
                    (format "%02dm" minutes)))))

(defun orrient--timers-format-event (event-name minutes-until)
  "Format an event of its next occurance."
  (format "%s %s" (orrient--timers-format-eta minutes-until) event-name))

(defun orrient--timers-heading-length ()
  "Calculate the longest meta name length."
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

(defun orrient--timers-event-entry (event-instance time)
  "Generate an event entry for a tabulated list.
EVENT-INSTANCE is the cl-struct `orrient-event-instance.'

TIME is used to calculate the eta for EVENT-INSTANCE."
  (let ((event (orrient-event-instance-event event-instance))
        (minutes-until (- (orrient-event-instance-start event-instance) time)))
    (cons (orrient--timers-format-event (orrient-event-name event) minutes-until)
          `(orrient-event-instance ,event-instance
            face ,(orrient--timers-get-countdown-face minutes-until)))))

(defun orrient--timers-entries (time)
  "Returns all entries in tabulated list at TIME."
  (mapcar
   (lambda (meta)
     (let* ((meta-name (orrient-meta-name meta))
            (meta-category (orrient-meta-category meta))
            (meta-iter (orrient--timers-meta-iter meta time)))
       (list meta-name
             (vector meta-name
                     (cons (orrient--timers-category-name meta-category) `(orrient-category-id ,meta-category))
                     (orrient--timers-event-entry (iter-next meta-iter) time)
                     (orrient--timers-event-entry (iter-next meta-iter) time)
                     (orrient--timers-event-entry (iter-next meta-iter) time)))))
   orrient-timers-schedule))

(defun orrient--timers-update (time)
  "Update the timers at TIME."
  (setq orrient-timers-time time)
  (setq tabulated-list-entries (orrient--timers-entries time))
  (tabulated-list-print t nil))

(defun orrient-timers-open ()
  "Open the event timers buffer."
  (interactive)
  (let* ((buffer (get-buffer-create orrient-timers-buffer))
         (window (get-buffer-window buffer)))
    (pop-to-buffer buffer)
    (set-window-dedicated-p window t)
    (with-current-buffer buffer
      (orrient-timers-mode))))

(defun orrient--timers-printer (id cols)
  "Used for printing to the `tabulated-list'."
  (when-let ((index 1)
             (column (aref cols index))
             (name (car column))
             (id (plist-get (cdr column) 'orrient-category-id)))
    (setf (car (aref cols index))
          (propertize name
                      'face (orrient--timers-get-category-face id))))
  (tabulated-list-print-entry id cols))

(define-derived-mode orrient-timers-mode tabulated-list-mode "GW2 Event Timers"
  "View Guild Wars 2 Event Timers."
  :group 'orrient
  :syntax-table nil
  :abbrev-table nil
  :interactive t
  (setq-local revert-buffer-function (lambda (&rest _)
                                       (orrient-timers-now)))

  (setq tabulated-list-printer #'orrient--timers-printer)
  (setq tabulated-list-format (vector '("Meta" 21 t)
                                      '("Category" 21 orrient--timers-category-sort)
                                      `("Current" 21 ,(apply-partially #'orrient--timers-event-instance-sort 2))
                                      `("Next" 21 ,(apply-partially #'orrient--timers-event-instance-sort 3))
                                      `("Later" 21 ,(apply-partially #'orrient--timers-event-instance-sort 4))))
  (orrient-timers-now)
  (tabulated-list-init-header)
  (tabulated-list-print))

(provide 'orrient-event-timers)

;;; orrient-event-timers.el ends here
