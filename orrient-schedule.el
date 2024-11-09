;;; orrient-schedule.el --- Information about GW2 -*- lexical-binding: t; -*-

;; Homepage: https://github.com/purplg/orrient
;; SPDX-License-Identifier: MIT
;;; Commentary:

;;; Code:
(require 'cl-lib)

(cl-defstruct
    (orrient-event
     (:constructor make-orrient-event
                   (name &key offset length frequency waypoint 
                    &aux
                    ;; Convert tuple of '(hours minutes) to minutes for the following keys
                    (offset (+ (* 60 (pop offset)) (pop offset)))
                    (length (+ (* 60 (pop length)) (pop length)))
                    (frequency (+ (* 60 (pop frequency)) (pop frequency))))))
  "A generic event with a repeating schedule."
  name
  waypoint
  offset
  length
  frequency)

(cl-defstruct orrient-event-instance
  "A specific occurance of `orrient-event' with a start and end time"
  event
  start
  end)

(cl-defstruct (orrient-meta
               (:constructor make-orrient-meta
                             (name &key category events
                                   &aux (events (mapcar (lambda (event) (apply #'make-orrient-event event))
                                                        events)))))
  "A collection of events to create a meta schedule."
  name
  category
  events)

(defun orrient-schedule--meta-category-name (category)
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
    ('end-of-dragons "End of Dragons")
    ('secrets-of-the-obscure "Secrets of the Obscure")
    ('janthir-wilds "Janthir Wilds")))

(defvar orrient-schedule nil
  "List of meta events.")

(defcustom orrient-schedule-soon-time 15
  "How many minutes until an event is considered to be starting
  'soon'.")

;;; Countdown
(defface orrient-schedule-countdown-now
  '((t (:inherit error)))
  "Orrient face for time remaining when an event is happening now."
  :group 'orrient)

(defface orrient-schedule-countdown-soon
  '((t (:inherit warning)))
  "Orrient face for time remaining when an event is happening soon."
  :group 'orrient)

(defface orrient-schedule-countdown-later
  '((t ()))
  "Orrient face for time remaining when an event is not happening soon."
  :group 'orrient)

(defface orrient-schedule-countdown-soon-watched
  '((t (:inverse-video t
        :inherit 'orrient-schedule-countdown-soon)))
  "Orrient face for time remaining when an event is happening soon."
  :group 'orrient)

(defface orrient-schedule-countdown-later-watched
  '((t (:inverse-video t
        :inherit orrient-schedule-countdown-later)))
  "Orrient face for time remaining when an event is not happening soon."
  :group 'orrient)

(defun orrient-schedule--get-countdown-face (minutes)
  "Return the face used when MINUTES remain."
  (cond ((<= minutes 0) 'orrient-schedule-countdown-now)
        ((< minutes orrient-schedule-soon-time) 'orrient-schedule-countdown-soon)
        (t 'orrient-schedule-countdown-later)))

(defun orrient-schedule--current-time ()
  "Return current time in minutes from UTC 0."
  (let ((time (decode-time nil t nil)))
    (+ (* 60 (decoded-time-hour time))
       (decoded-time-minute time))))

(defun orrient-schedule--format-eta (minutes &optional short)
  "Format an ETA shown on an event of its next occurance.
When SHORT is non-nil, use the shorted version of the time format
by inserting a minus '-' sign before the formatted time and omitting
the 'ago' at the end.

For example, say an event started 10 minutes ago. When SHORT is
non-nil, it'll be formatted as '-10m'. When SHORT is nil, it'll be
formatted as '10m ago'."
  (let ((past (< minutes 0))
        (hours (abs (/ minutes 60)))
        (minutes (abs (% minutes 60))))
    (concat
     (when (and past short) "-")
     (when (> hours 0)
       (format "%sh " hours))
     (format "%02dm" minutes)
     (when (and past (not short)) " ago"))))

(defmacro orrient-schedule--add (&rest args)
  (declare (indent defun))
  `(push
    (make-orrient-meta ,@args)
    orrient-schedule))

(orrient-schedule--add "Day and Night"
  :category 'core-tyria
  :events
  '(("Dawn"                               :offset ( 0 25) :frequency ( 2  0) :length ( 0  5) :waypoint nil)
    ("Day"                                :offset ( 0 30) :frequency ( 2  0) :length ( 1 10) :waypoint nil)
    ("Dusk"                               :offset ( 1 40) :frequency ( 2  0) :length ( 0  5) :waypoint nil)
    ("Night"                              :offset ( 1 45) :frequency ( 2  0) :length ( 0 40) :waypoint nil)))

(orrient-schedule--add "World Bosses"
  :category 'core-tyria
  :events
  '(("Admiral Taidha Covington"           :offset ( 0  0) :frequency ( 3  0) :length ( 0 15) :waypoint "[&BKgBAAA=]")
    ("Svanir Shaman Chief"                :offset ( 0 15) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BMIDAAA=]")
    ("Megadestroyer"                      :offset ( 0 30) :frequency ( 3  0) :length ( 0 15) :waypoint "[&BM0CAAA=]")
    ("Fire Elemental"                     :offset ( 0 45) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BEcAAAA=]")
    ("The Shatterer"                      :offset ( 1  0) :frequency ( 3  0) :length ( 0 15) :waypoint "[&BE4DAAA=]")
    ("Great Jungle Wurm"                  :offset ( 1 15) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BEEFAAA=]")
    ("Modniir Ulgoth"                     :offset ( 1 30) :frequency ( 3  0) :length ( 0 15) :waypoint "[&BLAAAAA=]")
    ("Shadow Behemoth"                    :offset ( 1 45) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BPcAAAA=]")
    ("Golem Mark II"                      :offset ( 2  0) :frequency ( 3  0) :length ( 0 15) :waypoint "[&BNQCAAA=]")
    ("Claw of Jormag"                     :offset ( 2 30) :frequency ( 3  0) :length ( 0 15) :waypoint "[&BHoCAAA=]")))

(orrient-schedule--add "Hard World Bosses"
  :category 'core-tyria
  :events
  '(("Tequatl the Sunless"                :offset ( 0  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNABAAA=]")
    ("Triple Trouble"                     :offset ( 1  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BKoBAAA=]")
    ("Karka Queen"                        :offset ( 2  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNUGAAA=]")
    ("Tequatl the Sunless"                :offset ( 3  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNABAAA=]")
    ("Triple Trouble"                     :offset ( 4  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BKoBAAA=]")
    ("Karka Queen"                        :offset ( 6  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNUGAAA=]")
    ("Tequatl the Sunless"                :offset ( 7  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNABAAA=]")
    ("Triple Trouble"                     :offset ( 8  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BKoBAAA=]")
    ("Karka Queen"                        :offset (10  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNUGAAA=]")
    ("Tequatl the Sunless"                :offset (11  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNABAAA=]")
    ("Triple Trouble"                     :offset (12  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BKoBAAA=]")
    ("Karka Queen"                        :offset (15  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNUGAAA=]")
    ("Tequatl the Sunless"                :offset (16  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNABAAA=]")
    ("Triple Trouble"                     :offset (17  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BKoBAAA=]")
    ("Karka Queen"                        :offset (18  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNUGAAA=]")
    ("Tequatl the Sunless"                :offset (19  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNABAAA=]")
    ("Triple Trouble"                     :offset (20  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BKoBAAA=]")
    ("Karka Queen"                        :offset (23  0) :frequency (24  0) :length ( 0 30) :waypoint "[&BNUGAAA=]")))

(orrient-schedule--add "Ley-Line Anomaly"
  :category 'core-tyria
  :events
  '(("Timberline Falls"                   :offset ( 0 20) :frequency ( 6  0) :length ( 0 20) :waypoint "[&BEwCAAA=]")
    ("Iron Marches"                       :offset ( 2 20) :frequency ( 6  0) :length ( 0 20) :waypoint "[&BOcBAAA=]")
    ("Gendarran Fields"                   :offset ( 4 20) :frequency ( 6  0) :length ( 0 20) :waypoint "[&BOQAAAA=]")))

(orrient-schedule--add "PVP Tournaments"
  :category 'core-tyria
  :events
  '(("Balthazar's Brawl"                  :offset ( 0  0) :frequency (12  0) :length ( 1  0) :waypoint nil)
    ("Grenth's Game"                      :offset ( 3  0) :frequency (12  0) :length ( 1  0) :waypoint nil)
    ("Melandru's Matchup"                 :offset ( 6  0) :frequency (12  0) :length ( 1  0) :waypoint nil)
    ("Lyssa's Legions"                    :offset ( 9  0) :frequency (12  0) :length ( 1  0) :waypoint nil)))

(orrient-schedule--add "Eye of the North"
  :category 'living-world-1
  :events
  '(("Twisted Marionette"                 :offset ( 0  0) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BAkMAAA=]")
    ("Battle For Lion's Arch"             :offset ( 0 30) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BAkMAAA=]")
    ("Tower of Nightmares"                :offset ( 1 30) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BAkMAAA=]")))

(orrient-schedule--add "Scarlets Invasion"
  :category 'living-world-1
  :events
  '(("Defeat Scarlets minions"            :offset ( 1  0) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BOQAAAA=]")))

(orrient-schedule--add "Dry Top"
  :category 'living-world-2
  :events
  '(("Crash Site"                         :offset ( 0  0) :frequency ( 1  0) :length ( 0 40) :waypoint nil)
    ("Sandstorm"                          :offset ( 0 40) :frequency ( 1  0) :length ( 0 20) :waypoint "[&BIAHAAA=]")))

(orrient-schedule--add "Verdant Brink"
  :category 'heart-of-thorns
  :events
  '(("Night: Night and the Enemy"         :offset ( 1 45) :frequency ( 2  0) :length ( 0 25) :waypoint nil)
    ("Night Bosses"                       :offset ( 0 10) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BAgIAAA=]")
    ("Day: Securing Verdant Brink"        :offset ( 0 30) :frequency ( 2  0) :length ( 1 15) :waypoint nil)))

(orrient-schedule--add "Auric Basin"
  :category 'heart-of-thorns
  :events
  '(("Challenges"                         :offset ( 0 45) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BGwIAAA=]")
    ("Octovine"                           :offset ( 1  0) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BAIIAAA=]")
    ("Reset"                              :offset ( 1 20) :frequency ( 2  0) :length ( 0 10) :waypoint nil)
    ("Pylons"                             :offset ( 1 30) :frequency ( 2  0) :length ( 1 15) :waypoint "[&BN0HAAA=]")))

(orrient-schedule--add "Tangled Depths"
  :category 'heart-of-thorns
  :events
  '(("Prep"                               :offset ( 0 25) :frequency ( 2  0) :length ( 0  5) :waypoint nil)
    ("Chak Gerent"                        :offset ( 0 30) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BPUHAAA=]")
    ("Help the Outposts"                  :offset ( 0 50) :frequency ( 2  0) :length ( 1 35) :waypoint nil)))

(orrient-schedule--add "Dragons Stand"
  :category 'heart-of-thorns
  :events
  '(("Dragons Stand"                      :offset ( 0 90) :frequency ( 2  0) :length ( 2  0) :waypoint "[&BBAIAAA=]")))

(orrient-schedule--add "Lake Doric"
  :category 'living-world-3
  :events
  '(("Noran's Homestead"                  :offset ( 0 30) :frequency ( 2  0) :length ( 0 30) :waypoint "[&BK8JAAA=]")
    ("Saidra's Haven"                     :offset ( 1  0) :frequency ( 2  0) :length ( 0 45) :waypoint "[&BK0JAAA=]")
    ("New Loamhurst"                      :offset ( 1 45) :frequency ( 2  0) :length ( 0 45) :waypoint "[&BLQJAAA=]")))

(orrient-schedule--add "Crystal Oasis"
  :category 'path-of-fire
  :events
  '(("Rounds 1 to 3"                      :offset ( 0  5) :frequency ( 2  0) :length ( 0 10) :waypoint "[&BLsKAAA=]")
    ("Pinata/Reset"                       :offset ( 0 20) :frequency ( 2  0) :length ( 0 10) :waypoint "[&BLsKAAA=]")))

(orrient-schedule--add "Desert Highlands"
  :category 'path-of-fire
  :events
  '(("Buried Treasure"                    :offset ( 0  5) :frequency ( 2  0) :length ( 0 10) :waypoint "[&BGsKAAA=]")))

(orrient-schedule--add "Elon Riverlands"
  :category 'path-of-fire
  :events
  '(("The Path to Ascension: Augury Rock" :offset ( 1 30) :frequency ( 2  0) :length ( 0 25) :waypoint "[&BFMKAAA=]")
    ("Doppelganger"                       :offset ( 1 50) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BFMKAAA=]")))

(orrient-schedule--add "The Desolation"
  :category 'path-of-fire
  :events
  '(("Junudu Rising"                     :offset ( 0 30) :frequency ( 1  0) :length ( 0 20) :waypoint "[&BMEKAAA=]")
    ("Maws of Torment"                   :offset ( 1  0) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BKMKAAA=]")))

(orrient-schedule--add "Domain of Vabbi"
  :category 'path-of-fire
  :events
  '(("Forged with Fire"                  :offset ( 0  0) :frequency ( 2  0) :length ( 0 30) :waypoint "[&BO0KAAA=]")
    ("Serpents Ire"                      :offset ( 0 30) :frequency ( 2  0) :length ( 0 30) :waypoint "[&BHQKAAA=]")))

(orrient-schedule--add "Domain of Istan"
 :category 'living-world-4
 :events
 '(("Palawadan"                          :offset ( 1 45) :frequency ( 2  0) :length ( 0 30) :waypoint "[&BAkLAAA=]")))

(orrient-schedule--add "Jahai Bluffs"
  :category 'living-world-4
  :events
  '(("Escorts"                           :offset ( 1  0) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BIMLAAA=]")
    ("Death-Branded Shatterer"           :offset ( 1 15) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BJMLAAA=]")))

(orrient-schedule--add "Thunderhead Peaks"
  :category 'living-world-4
  :events
  '(("The Oil Floes"                     :offset ( 0 45) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BKYLAAA=]")
    ("Thunderhead Keep"                  :offset ( 1 45) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BLsLAAA=]")))

(orrient-schedule--add "Grothmar Valley"
  :category 'icebrood-saga
  :events
  '(("Effigy"                            :offset ( 0 10) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BA4MAAA=]")
    ("Doomlore Shrine"                   :offset ( 0 38) :frequency ( 2  0) :length ( 0 22) :waypoint "[&BA4MAAA=]")
    ("Ooze Pits"                         :offset ( 1  5) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BPgLAAA=]")
    ("Metal Concert"                     :offset ( 1 40) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BPgLAAA=]")))

(orrient-schedule--add "Bjora Marches"
  :category 'icebrood-saga
  :events
  '(("Shards and Construct"              :offset ( 0  0) :frequency ( 2  0) :length ( 0  5) :waypoint "[&BCcMAAA=]")
    ("Icebrood Champions"                :offset ( 0  5) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BCcMAAA=]")
    ("Drakkar and Spirits of the Wild"   :offset ( 1  5) :frequency ( 2  0) :length ( 0 35) :waypoint "[&BDkMAAA=]")
    ("Defend Jora's Keep"                :offset ( 1 45) :frequency ( 2  0) :length ( 0 15) :waypoint "[&BCcMAAA=]")))

(orrient-schedule--add "Dragonstorm"
 :category 'icebrood-saga
 :events
 '(("Dragonstorm"                        :offset ( 1  0) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BAkMAAA=]")))

(orrient-schedule--add "Cantha: Day and Night"
  :category 'end-of-dragons
  :events
  '(("Dawn"                              :offset ( 0 25) :frequency ( 2  0) :length ( 0  5) :waypoint nil)
    ("Day"                               :offset ( 0 30) :frequency ( 2  0) :length ( 1 10) :waypoint nil)
    ("Dusk"                              :offset ( 1 40) :frequency ( 2  0) :length ( 0  5) :waypoint nil)
    ("Night"                             :offset ( 1 45) :frequency ( 2  0) :length ( 0 40) :waypoint nil)))

(orrient-schedule--add "Seitung Province"
 :category 'end-of-dragons
 :events
 '(("Aetherblade Assault"                :offset ( 1 30) :frequency ( 2  0) :length ( 0 30) :waypoint "[&BGUNAAA=]")))

(orrient-schedule--add "New Kaineng City"
 :category 'end-of-dragons
 :events
 '(("Kaineng Blackout"                   :offset ( 0  0) :frequency ( 2  0) :length ( 0 40) :waypoint "[&BBkNAAA=]")))

(orrient-schedule--add "The Echovald Wilds"
  :category 'end-of-dragons
  :events
  '(("Gang War"                          :offset ( 0 30) :frequency ( 2  0) :length ( 0 35) :waypoint "[&BMwMAAA=]")
    ("Aspenwood"                         :offset ( 1 40) :frequency ( 2  0) :length ( 0 20) :waypoint "[&BPkMAAA=]")))

(orrient-schedule--add "Dragons End"
  :category 'end-of-dragons
  :events
  '(("Jade Maw"                          :offset ( 0  5) :frequency ( 2  0) :length ( 0  8) :waypoint "[&BKIMAAA=]")
    ("Preparations"                      :offset ( 0 13) :frequency ( 2  0) :length ( 0 32) :waypoint "[&BKIMAAA=]")
    ("Jade Maw"                          :offset ( 0 45) :frequency ( 2  0) :length ( 0  8) :waypoint "[&BKIMAAA=]")
    ("Preparations"                      :offset ( 0 53) :frequency ( 2  0) :length ( 0  8) :waypoint "[&BKIMAAA=]")
    ("The Battle for the Jade Sea"       :offset ( 1  0) :frequency ( 2  0) :length ( 1  0) :waypoint "[&BKIMAAA=]")))

(orrient-schedule--add "Skywatch Archipelago"
  :category 'secrets-of-the-obscure
  :events
  '(("Unlocking the Wizard's Tower"      :offset ( 1  0) :frequency ( 2  0) :length ( 0 25) :waypoint "[&BL4NAAA=]")))

(orrient-schedule--add "Wizard's Tower"
  :category 'secrets-of-the-obscure
  :events
  '(("Target Practice"                   :offset ( 1  0) :frequency ( 2  0) :length ( 0 50) :waypoint "[&BB8OAAA=]")
    ("Fly by Night"                      :offset ( 1 55) :frequency ( 2  0) :length ( 0 25) :waypoint "[&BB8OAAA=]")))

(orrient-schedule--add "Amnytas"
 :category 'secrets-of-the-obscure
 :events
 '(("Defense of Amnytas"                 :offset ( 0  0) :frequency ( 2  0) :length ( 0 25) :waypoint "[&BDQOAAA=]")))

(orrient-schedule--add "Convergences"
 :category 'secrets-of-the-obscure
 :events
 '(("Convergences"                       :offset ( 1 30) :frequency ( 3  0) :length ( 0 10) :waypoint nil)))

(orrient-schedule--add "Janthir Syntri"
 :category 'janthir-wilds
 :events
 '(("Of Mists and Monsters"              :offset ( 0 30) :frequency ( 2  0) :length ( 0 25) :waypoint nil)))

(provide 'orrient-schedule)
;;; orrient-schedule.el ends here
