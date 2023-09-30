;;; orrient-data.el --- Information about GW2 -*- lexical-binding: t; -*-

;; Homepage: https://github.com/purplg/orrient
;; SPDX-License-Identifier: MIT
;;; Commentary:

;;; Code:
(require 'cl-lib)

(cl-defstruct
    (orrient-event
     (:constructor make-orrient-event
                   (name &key offset length frequency
                    &aux
                    ;; Convert tuple of '(hours minutes) to minutes for the following keys
                    (offset (+ (* 60 (pop offset)) (pop offset)))
                    (length (+ (* 60 (pop length)) (pop length)))
                    (frequency (+ (* 60 (pop frequency)) (pop frequency))))))
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

(cl-defstruct (orrient-meta
               (:constructor make-orrient-meta
                             (name &key category events
                                   &aux (events (mapcar (lambda (event) (apply #'make-orrient-event event))
                                                        events)))))
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

(defvar orrient-schedule nil
  "List of meta events.")

(defmacro orrient-schedule--add (&rest args)
  (declare (indent defun))
  `(push
    (make-orrient-meta ,@args)
    orrient-schedule))

(orrient-schedule--add "Day and Night"
  :category 'core-tyria
  :events '(("Dawn"                              :offset ( 0 25) :frequency ( 2  0) :length ( 0  5))
            ("Day"                               :offset ( 0 30) :frequency ( 2  0) :length ( 1 10))
            ("Dusk"                              :offset ( 1 40) :frequency ( 2  0) :length ( 0  5))
            ("Night"                             :offset ( 1 45) :frequency ( 2  0) :length ( 0 40))))

(orrient-schedule--add "World Bosses"
  :category 'core-tyria
  :events '(("Admiral Taidha Covington"          :offset ( 0  0) :frequency ( 3  0) :length ( 0 15))
            ("Svanir Shaman Chief"               :offset ( 0 15) :frequency ( 2  0) :length ( 0 15))
            ("Megadestroyer"                     :offset ( 0 30) :frequency ( 3  0) :length ( 0 15))
            ("Fire Elemental"                    :offset ( 0 45) :frequency ( 2  0) :length ( 0 15))
            ("The Shatterer"                     :offset ( 1  0) :frequency ( 3  0) :length ( 0 15))
            ("Great Jungle Wurm"                 :offset ( 1 15) :frequency ( 2  0) :length ( 0 15))
            ("Modniir Ulgoth"                    :offset ( 1 30) :frequency ( 3  0) :length ( 0 15))
            ("Shadow Behemoth"                   :offset ( 1 45) :frequency ( 2  0) :length ( 0 15))
            ("Golem Mark II"                     :offset ( 2  0) :frequency ( 3  0) :length ( 0 15))
            ("Claw of Jormag"                    :offset ( 2 30) :frequency ( 2  0) :length ( 0 15))))

(orrient-schedule--add "Hard World Bosses"
  :category 'core-tyria
  :events '(("Tequatl the Sunless"               :offset ( 0  0) :frequency (24  0) :length ( 0 30))
            ("Triple Trouble"                    :offset ( 1  0) :frequency (24  0) :length ( 0 30))
            ("Karka Queen"                       :offset ( 2  0) :frequency (24  0) :length ( 0 30))
            ("Tequatl the Sunless"               :offset ( 3  0) :frequency (24  0) :length ( 0 30))
            ("Triple Trouble"                    :offset ( 4  0) :frequency (24  0) :length ( 0 30))
            ("Karka Queen"                       :offset ( 6  0) :frequency (24  0) :length ( 0 30))
            ("Tequatl the Sunless"               :offset ( 7  0) :frequency (24  0) :length ( 0 30))
            ("Triple Trouble"                    :offset ( 8  0) :frequency (24  0) :length ( 0 30))
            ("Karka Queen"                       :offset (10  0) :frequency (24  0) :length ( 0 30))
            ("Tequatl the Sunless"               :offset (11  0) :frequency (24  0) :length ( 0 30))
            ("Triple Trouble"                    :offset (12  0) :frequency (24  0) :length ( 0 30))
            ("Karka Queen"                       :offset (15  0) :frequency (24  0) :length ( 0 30))
            ("Tequatl the Sunless"               :offset (16  0) :frequency (24  0) :length ( 0 30))
            ("Triple Trouble"                    :offset (17  0) :frequency (24  0) :length ( 0 30))
            ("Karka Queen"                       :offset (18  0) :frequency (24  0) :length ( 0 30))
            ("Tequatl the Sunless"               :offset (19  0) :frequency (24  0) :length ( 0 30))
            ("Triple Trouble"                    :offset (20  0) :frequency (24  0) :length ( 0 30))
            ("Karka Queen"                       :offset (23  0) :frequency (24  0) :length ( 0 30))))

(orrient-schedule--add "Ley-Line Anomaly"
  :category 'core-tyria
  :events '(("Timberline Falls"                  :offset ( 0 20) :frequency ( 6  0) :length ( 0 20))
            ("Iron Marches"                      :offset ( 2 20) :frequency ( 6  0) :length ( 0 20))
            ("Gendarran Fields"                  :offset ( 4 20) :frequency ( 6  0) :length ( 0 20))))

(orrient-schedule--add "PVP Tournaments"
 :category 'core-tyria
 :events '(("Balthazar's Brawl"                  :offset ( 0  0) :frequency (12  0) :length ( 1  0))
           ("Grenth's Game"                      :offset ( 3  0) :frequency (12  0) :length ( 1  0))
           ("Melandru's Matchup"                 :offset ( 6  0) :frequency (12  0) :length ( 1  0))
           ("Lyssa's Legions"                    :offset ( 9  0) :frequency (12  0) :length ( 1  0))))

(orrient-schedule--add "Eye of the North"
 :category 'living-world-1
 :events '(("Twisted Marionette"                 :offset ( 0  0) :frequency ( 2  0) :length ( 0 20))
           ("Battle For Lion's Arch"             :offset ( 0 30) :frequency ( 2  0) :length ( 0 15))
           ("Tower of Nightmares"                :offset ( 1 30) :frequency ( 2  0) :length ( 0 15))))

(orrient-schedule--add "Scarlets Invasion"
  :category 'living-world-1
  :events '(("Defeat Scarlets minions"           :offset ( 1  0) :frequency ( 2  0) :length ( 0 15))))

(orrient-schedule--add "Dry Top"
  :category 'living-world-2
  :events '(("Crash Site"                        :offset ( 0  0) :frequency ( 1  0) :length ( 0 40))
            ("Sandstorm"                         :offset ( 0 40) :frequency ( 1  0) :length ( 0 20))))

(orrient-schedule--add "Verdant Brink"
 :category 'heart-of-thorns
 :events '(("Night: Night and the Enemy"         :offset ( 1 45) :frequency ( 2  0) :length ( 0 25))
           ("Night Bosses"                       :offset ( 0 10) :frequency ( 2  0) :length ( 0 20))
           ("Day: Securing Verdant Brink"        :offset ( 0 30) :frequency ( 2  0) :length ( 1 15))))

(orrient-schedule--add "Auric Basin"
 :category 'heart-of-thorns
 :events '(("Challenges"                         :offset ( 0 45) :frequency ( 2  0) :length ( 0 15))
           ("Octovine"                           :offset ( 1  0) :frequency ( 2  0) :length ( 0 20))
           ("Reset"                              :offset ( 1 20) :frequency ( 2  0) :length ( 0 10))
           ("Pylons"                             :offset ( 1 30) :frequency ( 2  0) :length ( 1 15))))

(orrient-schedule--add "Tangled Depths"
  :category 'heart-of-thorns
  :events '(("Prep"                              :offset ( 0 25) :frequency ( 2  0) :length ( 0  5))
            ("Chak Gerent"                       :offset ( 0 30) :frequency ( 2  0) :length ( 0 20))
            ("Help the Outposts"                 :offset ( 0 50) :frequency ( 2  0) :length ( 1 35))))

(orrient-schedule--add "Dragons Stand"
  :category 'heart-of-thorns
  :events '(("Dragons Stand"                     :offset ( 0 90) :frequency ( 2  0) :length ( 2  0))))

(orrient-schedule--add "Lake Doric"
 :category 'living-world-3
 :events '(("Noran's Homestead"                  :offset ( 0 30) :frequency ( 2  0) :length ( 0 30))
           ("Saidra's Haven"                     :offset ( 1  0) :frequency ( 2  0) :length ( 0 45))
           ("New Loamhurst"                      :offset ( 1 45) :frequency ( 2  0) :length ( 0 45))))

(orrient-schedule--add "Crystal Oasis"
 :category 'path-of-fire
 :events '(("Rounds 1 to 3"                      :offset ( 0  5) :frequency ( 2  0) :length ( 0 10))
           ("Pinata/Reset"                       :offset ( 0 20) :frequency ( 2  0) :length ( 0 10))))

(orrient-schedule--add "Desert Highlands"
 :category 'path-of-fire
 :events '(("Buried Treasure"                    :offset ( 0  5) :frequency ( 2  0) :length ( 0 10))))

(orrient-schedule--add "Elon Riverlands"
 :category 'path-of-fire
 :events '(("The Path to Ascension: Augury Rock" :offset ( 1 30) :frequency ( 2  0) :length ( 0 25))
           ("Doppelganger"                       :offset ( 1 50) :frequency ( 2  0) :length ( 0 20))))

(orrient-schedule--add "The Desolation"
 :category 'path-of-fire
 :events '(("Junudu Rising"                      :offset ( 0 30) :frequency ( 1  0) :length ( 0 20))
           ("Maws of Torment"                    :offset ( 1  0) :frequency ( 2  0) :length ( 0 20))))

(orrient-schedule--add "Domain of Vabbi"
 :category 'path-of-fire
 :events '(("Forged with Fire"                   :offset ( 0  0) :frequency ( 2  0) :length ( 0 30))
           ("Serpents Ire"                       :offset ( 0 30) :frequency ( 2  0) :length ( 0 30))))

(orrient-schedule--add "Domain of Istan"
 :category 'living-world-4
 :events '(("Palawadan"                          :offset ( 1 45) :frequency ( 2  0) :length ( 0 30))))

(orrient-schedule--add "Jahai Bluffs"
 :category 'living-world-4
 :events '(("Escorts"                            :offset ( 1  0) :frequency ( 2  0) :length ( 0 15))
           ("Death-Branded Shatterer"            :offset ( 1 15) :frequency ( 2  0) :length ( 0 15))))

(orrient-schedule--add "Thunderhead Peaks"
 :category 'living-world-4
 :events '(("The Oil Floes"                      :offset ( 0 45) :frequency ( 2  0) :length ( 0 15))
           ("Thunderhead Keep"                   :offset ( 1 45) :frequency ( 2  0) :length ( 0 20))))

(orrient-schedule--add "Grothmar Valley"
 :category 'icebrood-saga
 :events '(("Effigy"                             :offset ( 0 10) :frequency ( 2  0) :length ( 0 15))
           ("Doomlore Shrine"                    :offset ( 0 38) :frequency ( 2  0) :length ( 0 22))
           ("Ooze Pits"                          :offset ( 1  5) :frequency ( 2  0) :length ( 0 20))
           ("Metal Concert"                      :offset ( 1 40) :frequency ( 2  0) :length ( 0 20))))

(orrient-schedule--add "Bjora Marches"
 :category 'icebrood-saga
 :events '(("Shards and Construct"               :offset ( 0  0) :frequency ( 2  0) :length ( 0  5))
           ("Icebrood Champions"                 :offset ( 0  5) :frequency ( 2  0) :length ( 0 15))
           ("Drakkar and Spirits of the Wild"    :offset ( 1  5) :frequency ( 2  0) :length ( 0 35))
           ("Raven Shrines"                      :offset ( 1 45) :frequency ( 2  0) :length ( 0 15))))

(orrient-schedule--add "Dragonstorm"
 :category 'icebrood-saga
 :events '(("Dragonstorm"                        :offset ( 1  0) :frequency ( 2  0) :length ( 0 20))))

(orrient-schedule--add "Cantha: Day and Night"
 :category 'end-of-dragons
 :events '(("Dawn"                               :offset ( 0 25) :frequency ( 2  0) :length ( 0  5))
           ("Day"                                :offset ( 0 30) :frequency ( 2  0) :length ( 1 10))
           ("Dusk"                               :offset ( 1 40) :frequency ( 2  0) :length ( 0  5))
           ("Night"                              :offset ( 1 45) :frequency ( 2  0) :length ( 0 40))))

(orrient-schedule--add "Seitung Province"
 :category 'end-of-dragons
 :events '(("Aetherblade Assault"                :offset ( 1 30) :frequency ( 2  0) :length ( 0 30))))

(orrient-schedule--add "New Kaineng City"
 :category 'end-of-dragons
 :events '(("Kaineng Blackout"                   :offset ( 0  0) :frequency ( 2  0) :length ( 0 40))))

(orrient-schedule--add "The Echovald Wilds"
 :category 'end-of-dragons
 :events '(("Gang War"                           :offset ( 0 30) :frequency ( 2  0) :length ( 0 35))
           ("Aspenwood"                          :offset ( 1 40) :frequency ( 2  0) :length ( 0 20))))

(orrient-schedule--add "Dragons End"
  :category 'end-of-dragons
  :events '(("Jade Maw"                          :offset ( 0  5) :frequency ( 2  0) :length ( 0  8))
            ("Preparations"                      :offset ( 0 13) :frequency ( 2  0) :length ( 0 32))
            ("Jade Maw"                          :offset ( 0 45) :frequency ( 2  0) :length ( 0  8))
            ("Preparations"                      :offset ( 0 53) :frequency ( 2  0) :length ( 0  8))
            ("The Battle for the Jade Sea"       :offset ( 1  0) :frequency ( 2  0) :length ( 1  0))))

(orrient-schedule--add "Skywatch Archipelago"
 :category 'secrets-of-the-obscure
 :events '(("Unlocking the Wizard's Tower"       :offset ( 1  0) :frequency ( 2  0) :length ( 0 25))))

(orrient-schedule--add "Amnytas"
 :category 'secrets-of-the-obscure
 :events '(("Defense of Amnytas"                 :offset ( 0  0) :frequency ( 2  0) :length ( 0 25))))

(provide 'orrient-data)
;;; orrient-data.el ends here
