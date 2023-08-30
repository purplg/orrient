;;; orrient-data.el --- Information about GW2 -*- lexical-binding: t; -*-

;; Homepage: https://github.com/purplg/orrient
;; SPDX-License-Identifier: MIT
;;; Commentary:

;;; Code:
(require 'cl-lib)

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

(defun orrient--meta-category-name (category)
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
    ('secrets-of-the-obscure "Secrets of the Obscure")))

(defvar orrient-schedule
  `(,(make-orrient-meta :name "Day and Night"
                        :category 'core-tyria
                        :events `(,(make-orrient-event :name "Dawn"
                                                       :offset '(0 25)
                                                       :frequency '(2 0)
                                                       :length '(0  5))
                                  ,(make-orrient-event :name "Day"
                                                       :offset '(0 30)
                                                       :frequency '(2 0)
                                                       :length '(1 10))
                                  ,(make-orrient-event :name "Dusk"
                                                       :offset '(1 40)
                                                       :frequency '(2 0)
                                                       :length '(0  5))
                                  ,(make-orrient-event :name "Night"
                                                       :offset '(1 45)
                                                       :frequency '(2 0)
                                                       :length '(0 40))))
    ,(make-orrient-meta :name "World Bosses"
                        :category 'core-tyria
                        :events `(,(make-orrient-event :name "Admiral Taidha Covington"
                                                       :offset '(0  0)
                                                       :frequency '(3 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Svanir Shaman Chief"
                                                       :offset '(0 15)
                                                       :frequency '(2 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Megadestroyer"
                                                       :offset '(0 30)
                                                       :frequency '(3 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Fire Elemental"
                                                       :offset '(0 45)
                                                       :frequency '(2 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "The Shatterer"
                                                       :offset '(1  0)
                                                       :frequency '(3 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Great Jungle Wurm"
                                                       :offset '(1 15)
                                                       :frequency '(2 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Modniir Ulgoth"
                                                       :offset '(1 30)
                                                       :frequency '(3 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Shadow Behemoth"
                                                       :offset '(1 45)
                                                       :frequency '(2 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Golem Mark II"
                                                       :offset '(2  0)
                                                       :frequency '(3 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Claw of Jormag"
                                                       :offset '(2 30)
                                                       :frequency '(2 0)
                                                       :length '(0 15))))
    ,(make-orrient-meta :name "Hard World Bosses"
                        :category 'core-tyria
                        :events `(,(make-orrient-event :name "Tequatl the Sunless"
                                                       :offset '( 0 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Triple Trouble"
                                                       :offset '( 1 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Karka Queen"
                                                       :offset '( 2 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Tequatl the Sunless"
                                                       :offset '( 3 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Triple Trouble"
                                                       :offset '( 4 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Karka Queen"
                                                       :offset '( 6 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Tequatl the Sunless"
                                                       :offset '( 7 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Triple Trouble"
                                                       :offset '( 8 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Karka Queen"
                                                       :offset '(10 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Tequatl the Sunless"
                                                       :offset '(11 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Triple Trouble"
                                                       :offset '(12 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Karka Queen"
                                                       :offset '(15 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Tequatl the Sunless"
                                                       :offset '(16 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Triple Trouble"
                                                       :offset '(17 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Karka Queen"
                                                       :offset '(18 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Tequatl the Sunless"
                                                       :offset '(19 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Triple Trouble"
                                                       :offset '(20 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Karka Queen"
                                                       :offset '(23 0)
                                                       :frequency '(24 0)
                                                       :length '(0 30))))
    ,(make-orrient-meta :name "Ley-Line Anomaly"
                        :category 'core-tyria
                        :events `(,(make-orrient-event :name "Timberline Falls"
                                                       :offset '(0 20)
                                                       :frequency '(6 0)
                                                       :length '(0 20))
                                  ,(make-orrient-event :name "Iron Marches"
                                                       :offset '(2 20)
                                                       :frequency '(6 0)
                                                       :length '(0 20))
                                  ,(make-orrient-event :name "Gendarran Fields"
                                                       :offset '(4 20)
                                                       :frequency '(6 0)
                                                       :length '(0 20))))
    ,(make-orrient-meta :name "PVP Tournaments"
                        :category 'core-tyria
                        :events `(,(make-orrient-event :name "Balthazar's Brawl"
                                                       :offset '(0 0)
                                                       :frequency '(12 0)
                                                       :length '(1 0))
                                  ,(make-orrient-event :name "Grenth's Game"
                                                       :offset '(3 0)
                                                       :frequency '(12 0)
                                                       :length '(1 0))
                                  ,(make-orrient-event :name "Melandru's Matchup"
                                                       :offset '(6 0)
                                                       :frequency '(12 0)
                                                       :length '(1 0))
                                  ,(make-orrient-event :name "Lyssa's Legions"
                                                       :offset '(9 0)
                                                       :frequency '(12 0)
                                                       :length '(1 0))))
    ,(make-orrient-meta :name "Eye of the North"
                        :category 'living-world-1
                        :events `(,(make-orrient-event :name "Twisted Marionette"
                                                       :offset '(0  0)
                                                       :frequency '(2 0)
                                                       :length '(0 20))
                                  ,(make-orrient-event :name "Battle For Lion's Arch"
                                                       :offset '(0 30)
                                                       :frequency '(2 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Tower of Nightmares"
                                                       :offset '(1 30)
                                                       :frequency '(2 0)
                                                       :length '(0 15))))
    ,(make-orrient-meta :name "Scarlets Invasion"
                        :category 'living-world-1
                        :events `(,(make-orrient-event :name "Defeat Scarlets minions"
                                                       :offset '(1  0)
                                                       :frequency '(2 0)
                                                       :length '(0 15))))
    ,(make-orrient-meta :name "Dry Top"
                        :category 'living-world-2
                        :events `(,(make-orrient-event :name "Crash Site"
                                                       :offset '(0  0)
                                                       :frequency '(1 0)
                                                       :length '(0 40))
                                  ,(make-orrient-event :name "Sandstorm"
                                                       :offset '(0 40)
                                                       :frequency '(1 0)
                                                       :length '(0 20))))
    ,(make-orrient-meta :name "Verdant Brink"
                        :category 'heart-of-thorns
                        :events `(,(make-orrient-event :name "Night: Night and the Enemy"
                                                       :offset '(1 45)
                                                       :frequency '(2 0)
                                                       :length '(0 25))
                                  ,(make-orrient-event :name "Night Bosses"
                                                       :offset '(0 10)
                                                       :frequency '(2 0)
                                                       :length '(0 20))
                                  ,(make-orrient-event :name "Day: Securing Verdant Brink"
                                                       :offset '(0 30)
                                                       :frequency '(2 0)
                                                       :length '(1 15))))
    ,(make-orrient-meta :name "Auric Basin"
                        :category 'heart-of-thorns
                        :events `(,(make-orrient-event :name "Challenges"
                                                       :offset '(0 45)
                                                       :frequency '(2 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Octovine"
                                                       :offset '(1  0)
                                                       :frequency '(2 0)
                                                       :length '(0 20))
                                  ,(make-orrient-event :name "Reset"
                                                       :offset '(1 20)
                                                       :frequency '(2 0)
                                                       :length '(0 10))
                                  ,(make-orrient-event :name "Pylons"
                                                       :offset '(1 30)
                                                       :frequency '(2 0)
                                                       :length '(1 15))))
    ,(make-orrient-meta :name "Tangled Depths"
                        :category 'heart-of-thorns
                        :events `(,(make-orrient-event :name "Prep"
                                                       :offset '(0 25)
                                                       :frequency '(2 0)
                                                       :length '(0  5))
                                  ,(make-orrient-event :name "Chak Gerent"
                                                       :offset '(0 30)
                                                       :frequency '(2 0)
                                                       :length '(0 20))
                                  ,(make-orrient-event :name "Help the Outposts"
                                                       :offset '(0 50)
                                                       :frequency '(2 0)
                                                       :length '(1 35))))
    ,(make-orrient-meta :name "Dragons Stand"
                        :category 'heart-of-thorns
                        :events `(,(make-orrient-event :name "Dragon's Stand"
                                                       :offset '(0 90)
                                                       :frequency '(2 0)
                                                       :length '(2 0))))
    ,(make-orrient-meta :name "Lake Doric"
                        :category 'living-world-3
                        :events `(,(make-orrient-event :name "Noran's Homestead"
                                                       :offset '(0 30)
                                                       :frequency '(2 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Saidra's Haven"
                                                       :offset '(1  0)
                                                       :frequency '(2 0)
                                                       :length '(0 45))
                                  ,(make-orrient-event :name "New Loamhurst"
                                                       :offset '(1 45)
                                                       :frequency '(2 0)
                                                       :length '(0 45))))
    ,(make-orrient-meta :name "Crystal Oasis"
                        :category 'path-of-fire
                        :events `(,(make-orrient-event :name "Rounds 1 to 3"
                                                       :offset '(0  5)
                                                       :frequency '(2 0)
                                                       :length '(0 10))
                                  ,(make-orrient-event :name "Pinata/Reset"
                                                       :offset '(0 20)
                                                       :frequency '(2 0)
                                                       :length '(0 10))))
    ,(make-orrient-meta :name "Desert Highlands"
                        :category 'path-of-fire
                        :events `(,(make-orrient-event :name "Buried Treasure"
                                                       :offset '(0 5)
                                                       :frequency '(2 0)
                                                       :length '(0 10))))
    ,(make-orrient-meta :name "Elon Riverlands"
                        :category 'path-of-fire
                        :events `(,(make-orrient-event :name "The Path to Ascension: Augury Rock"
                                                       :offset '(1 30)
                                                       :frequency '(2 0)
                                                       :length '(0 25))
                                  ,(make-orrient-event :name "Doppelganger"
                                                       :offset '(1 50)
                                                       :frequency '(2 0)
                                                       :length '(0 20))))
    ,(make-orrient-meta :name "The Desolation"
                        :category 'path-of-fire
                        :events `(,(make-orrient-event :name "Junudu Rising"
                                                       :offset '(0 30)
                                                       :frequency '(1 0)
                                                       :length '(0 20))
                                  ,(make-orrient-event :name "Maws of Torment"
                                                       :offset '(1  0)
                                                       :frequency '(2 0)
                                                       :length '(0 20))))
    ,(make-orrient-meta :name "Domain of Vabbi"
                        :category 'path-of-fire
                        :events `(,(make-orrient-event :name "Forged with Fire"
                                                       :offset '(0  0)
                                                       :frequency '(2 0)
                                                       :length '(0 30))
                                  ,(make-orrient-event :name "Serpents Ire"
                                                       :offset '(0 30)
                                                       :frequency '(2 0)
                                                       :length '(0 30))))
    ,(make-orrient-meta :name "Domain of Istan"
                        :category 'living-world-4
                        :events `(,(make-orrient-event :name "Palawadan"
                                                       :offset '(1 45)
                                                       :frequency '(2 0)
                                                       :length '(0 30))))
    ,(make-orrient-meta :name "Jahai Bluffs"
                        :category 'living-world-4
                        :events `(,(make-orrient-event :name "Escorts"
                                                       :offset '(1  0)
                                                       :frequency '(2 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Death-Branded Shatterer"
                                                       :offset '(1 15)
                                                       :frequency '(2 0)
                                                       :length '(0 15))))
    ,(make-orrient-meta :name "Thunderhead Peaks"
                        :category 'living-world-4
                        :events `(,(make-orrient-event :name "The Oil Floes"
                                                       :offset '(0 45)
                                                       :frequency '(2 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Thunderhead Keep"
                                                       :offset '(1 45)
                                                       :frequency '(2 0)
                                                       :length '(0 20))))
    ,(make-orrient-meta :name "Grothmar Valley"
                        :category 'icebrood-saga
                        :events `(,(make-orrient-event :name "Effigy"
                                                       :offset '(0 10)
                                                       :frequency '(2 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Doomlore Shrine"
                                                       :offset '(0 38)
                                                       :frequency '(2 0)
                                                       :length '(0 22))
                                  ,(make-orrient-event :name "Ooze Pits"
                                                       :offset '(1  5)
                                                       :frequency '(2 0)
                                                       :length '(0 20))
                                  ,(make-orrient-event :name "Metal Concert"
                                                       :offset '(1 40)
                                                       :frequency '(2 0)
                                                       :length '(0 20))))
    ,(make-orrient-meta :name "Bjora Marches"
                        :category 'icebrood-saga
                        :events `(,(make-orrient-event :name "Shards and Construct"
                                                       :offset '(0  0)
                                                       :frequency '(2 0)
                                                       :length '(0  5))
                                  ,(make-orrient-event :name "Icebrood Champions"
                                                       :offset '(0  5)
                                                       :frequency '(2 0)
                                                       :length '(0 15))
                                  ,(make-orrient-event :name "Drakkar and Spirits of the Wild"
                                                       :offset '(1  5)
                                                       :frequency '(2 0)
                                                       :length '(0 35))
                                  ,(make-orrient-event :name "Raven Shrines"
                                                       :offset '(1 45)
                                                       :frequency '(2 0)
                                                       :length '(0 15))))
    ,(make-orrient-meta :name "Dragonstorm"
                        :category 'icebrood-saga
                        :events `(,(make-orrient-event :name "Dragonstorm"
                                                       :offset '(1  0)
                                                       :frequency '(2 0)
                                                       :length '(0 20))))
    ,(make-orrient-meta :name "Cantha: Day and Night"
                        :category 'end-of-dragons
                        :events `(,(make-orrient-event :name "Dawn"
                                                       :offset '(0 25)
                                                       :frequency '(2 0)
                                                       :length '(0  5))
                                  ,(make-orrient-event :name "Day"
                                                       :offset '(0 30)
                                                       :frequency '(2 0)
                                                       :length '(1 10))
                                  ,(make-orrient-event :name "Dusk"
                                                       :offset '(1 40)
                                                       :frequency '(2 0)
                                                       :length '(0  5))
                                  ,(make-orrient-event :name "Night"
                                                       :offset '(1 45)
                                                       :frequency '(2 0)
                                                       :length '(0 40))))
    ,(make-orrient-meta :name "Seitung Province"
                        :category 'end-of-dragons
                        :events `(,(make-orrient-event :name "Aetherblade Assault"
                                                       :offset '(1 30)
                                                       :frequency '(2 0)
                                                       :length '(0 30))))
    ,(make-orrient-meta :name "New Kaineng City"
                        :category 'end-of-dragons
                        :events `(,(make-orrient-event :name "Kaineng Blackout"
                                                       :offset '(0 0)
                                                       :frequency '(2 0)
                                                       :length '(0 40))))
    ,(make-orrient-meta :name "The Echovald Wilds"
                        :category 'end-of-dragons
                        :events `(,(make-orrient-event :name "Gang War"
                                                       :offset '(0 30)
                                                       :frequency '(2 0)
                                                       :length '(0 35))
                                  ,(make-orrient-event :name "Aspenwood"
                                                       :offset '(1 40)
                                                       :frequency '(2 0)
                                                       :length '(0 20))))
    ,(make-orrient-meta :name "Dragons End"
                        :category 'end-of-dragons
                        :events `(,(make-orrient-event :name "Jade Maw"
                                                       :offset '(0  5)
                                                       :frequency '(2 0)
                                                       :length '(0  8))
                                  ,(make-orrient-event :name "Preparations"
                                                       :offset '(0 13)
                                                       :frequency '(2 0)
                                                       :length '(0 32))
                                  ,(make-orrient-event :name "Jade Maw"
                                                       :offset '(0 45)
                                                       :frequency '(2 0)
                                                       :length '(0  8))
                                  ,(make-orrient-event :name "Preparations"
                                                       :offset '(0 53)
                                                       :frequency '(2 0)
                                                       :length '(0  8))
                                  ,(make-orrient-event :name "The Battle for the Jade Sea"
                                                       :offset '(1  0)
                                                       :frequency '(2 0)
                                                       :length '(1  0))))
    ,(make-orrient-meta :name "Skywatch Archipelago"
                        :category 'secrets-of-the-obscure
                        :events `(,(make-orrient-event
                                    :name "Unlocking the Wizard's Tower"
                                    :offset '(1  0)
                                    :frequency '(2 0)
                                    :length '(0  25))))
    ,(make-orrient-meta :name "Amnytas"
                        :category 'secrets-of-the-obscure
                        :events `(,(make-orrient-event
                                    :name "Defense of Amnytas"
                                    :offset '(0  0)
                                    :frequency '(2 0)
                                    :length '(0  25)))))
  "List of meta events.")

(provide 'orrient-data)
;;; orrient-data.el ends here
