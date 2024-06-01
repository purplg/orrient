;;; orrient-api.el --- GW2 API -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'subr-x)
(require 'eieio)


;; Achievements

(defclass orrient-achievement ()
  ((id :initarg :id
       :type number)
   (name :initarg :name
         :type string)
   (bits :initarg :bits
         :initform '()
         :type (list-of orrient-achievement-bit))
   ;; (rewards)
   )
  "A single achievement.")

(defclass orrient-achievement-bit ()
  ((type :initarg :type
         :type string)
   (id :initarg :id
       :initform nil
       :type (or null number))
   (text :initarg :text
         :initform nil
         :type (or null string)))
  "A bit in an achievement.")

(defclass orrient-account-achievement ()
  ((id :type number
       :initarg :id
       :documentation
       "The achievement id.")
   (bits :type (list-of number)
         :initarg :bits
         :documentation
         "This attribute contains an array of numbers, giving more specific information on the progress for the achievement. The meaning of each value varies with each achievement. Bits start at zero. If an achievement is done, the in-progress bits are not displayed.")
   (current :initarg :current
            :type number
            :documentation
            "The player's current progress towards the achievement.")
   (max :type number
        :initarg :max
        :documentation
        "The amount needed to complete the achievement.")
   (done :type boolean
         :initarg :done
         :documentation
         "Whether or not the achievement is done.")
   (repeated :type (or null number)
             :initarg :repeated
             :documentation
             "The number of times the achievement has been completed if the achievement is repeatable.")
   (unlocked :type boolean
             :initform t
             :initarg :unlocked
             :documentation
             "Whether or not the achievement is unlocked. Note that if this property does not exist, the achievement is unlocked as well."))
  "A single achievement for the configured account.")


;; Items

(defclass orrient-api-item ()
  ((id :type number
       :initarg :id
       :documentation
       "The item id.")
   (name :type string
         :initarg :name
         :documentation
         "The item name."))
  "An in-game item.

Usually used for achievement rewards.")


;; Dailies

;; TODO Obsolete. Replace with Wizard's Vault

(cl-defstruct orrient-api-daily
  "A single daily achievement.

Every daily is an achievement, but most achievements are not
dailies.

This is only created and for use with the `orrinet-api-dailies'
struct."
  achievement
  level-min
  level-max
  required-access
  type)

(cl-defstruct orrient-api-dailies
  "All the dailies for a specific day.

The PVE, PVP, WVW, FRACTALS, and SPECIAL fields are each a list
of `orrient-api-daily''s."
  pve
  pvp
  wvw
  fractals
  special)

(provide 'orrient-model)
;;; orrient-model.el ends here.