;;; orrient-api.el --- GW2 API -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'subr-x)
(require 'eieio)

(require 'plz)

(require 'orrient-cache)

(defconst orrient-api--url "https://api.guildwars2.com")
(defconst orrient-api--endpoints '((achievements . "/v2/achievements")
                                   (dailies . "/v2/achievements/daily")
                                   (dailies-tomorrow . "/v2/achievements/daily/tomorrow")
                                   (items . "/v2/items")))


;; STRUCTS

;; These are typically response objects based on the GW2
;; API. `https://wiki.guildwars2.com/wiki/API:Main'

(cl-defstruct orrient-api-achievement
  "A single achievement."
  id
  name
  rewards)

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

(cl-defstruct orrient-api-item
  "An in-game item.

Usually used for achievement rewards."
  id
  name)


;; REQUESTS

;; Send API request to the relevant GW2 API endpoints.

(defun orrient-api--achievements (ids &optional callback)
  "Retrieve data about achievements.
IDS is list of achievement ids to resolve."
    (let* ((cached (orrient-cache--get-achievements ids))
           (cached-ids (seq-map (lambda (achievement) (orrient-api-achievement-id achievement)) cached))
           (uncached (seq-filter (lambda (item)
                                   (not (memq item cached-ids)))
                                 ids)))
      (if uncached
          (let ((path (concat (alist-get 'achievements orrient-api--endpoints)
                              "?ids="
                              (string-join (mapcar #'prin1-to-string uncached) ","))))
            (orrient-api--request path
                                  #'orrient-api--parse-achievements
                                  #'orrient-api--handler-achievements
                                  cached
                                  callback))
        (orrient-api--handler-achievements nil
                                           cached
                                           callback))))

(defun orrient-api--dailies (&optional callback)
  "Retrieve data about todays dailies."
  (let ((cached (orrient-cache--get-dailies)))
    (if (and (orrient-api-dailies-pve cached)
             (orrient-api-dailies-pvp cached)
             (orrient-api-dailies-wvw cached)
             (orrient-api-dailies-fractals cached))
        (orrient-api--handler-dailies cached
                                      nil
                                      callback)
      (let ((path (alist-get 'dailies orrient-api--endpoints)))
        (orrient-api--request path
                              #'orrient-api--parse-dailies
                              #'orrient-api--handler-dailies
                              nil
                              callback)))))

(defun orrient-api--request (path parser handler cached &optional callback)
  "Make a request to the GW2 API.
PATH is the full endpoint path to query.

CACHED is a list of items to be append to the result of the API
query."
  (let ((url (concat orrient-api--url path)))
    (plz 'get url
      :as parser
      :then (lambda (buffer) (funcall handler buffer cached callback)))))


;; PARSERS

;; A parser takes no arguments and is expected to be called in the buffer with
;; the response payload to utilize `json-parse-buffer'

(defun orrient-api--parse-achievements ()
  "Used by `orrient-api--achievements' to parse a response from the
GW2 API.

See: `https://wiki.guildwars2.com/wiki/API:2/achievements'"
  (let ((achievements (json-parse-buffer)))
    (seq-map (lambda (achievement)
               (make-orrient-api-achievement
                :id (gethash "id" achievement)
                :name (gethash "name" achievement)))
             achievements)))

(defun orrient-api--parse-dailies ()
  "Used by `orrient-api--dailies' to parse a response from the
GW2 API.

See: `https://wiki.guildwars2.com/wiki/API:2/achievements/daily'
and: `https://wiki.guildwars2.com/wiki/API:2/achievements/daily/tomorrow'"
  (let* ((dailies (json-parse-buffer))
         (pve (gethash "pve" dailies))
         (pvp (gethash "pvp" dailies))
         (wvw (gethash "wvw" dailies))
         (fractals (gethash "fractals" dailies))
         (special (gethash "special" dailies)))
    (make-orrient-api-dailies
     :pve (seq-map (lambda (daily)
                     (make-orrient-api-daily :achievement (make-orrient-api-achievement :id (gethash "id" daily))
                                             :type 'pve))
                   pve)
     :pvp (seq-map (lambda (daily)
                     (make-orrient-api-daily :achievement (make-orrient-api-achievement :id (gethash "id" daily))
                                             :type 'pvp))
                   pvp)
     :wvw (seq-map (lambda (daily)
                     (make-orrient-api-daily :achievement (make-orrient-api-achievement :id (gethash "id" daily))
                                             :type 'wvw))
                   wvw)
     :fractals (seq-map (lambda (daily)
                          (make-orrient-api-daily :achievement (make-orrient-api-achievement :id (gethash "id" daily))
                                                  :type 'fractals))
                        fractals)
     :special (seq-map (lambda (daily)
                         (make-orrient-api-daily :achievement (make-orrient-api-achievement :id (gethash "id" daily))
                                                 :type 'special))
                       special))))


;; RESPONSE HANDLERS

;; Each response handler must take the achievements fetched from remote, the
;; cached achievements, and a callback function to send this combined lists.

(defun orrient-api--handler-achievements (fetched cached callback)
  "Caches the parsed response from the GW2 achievements endpoint and
dispatches the callback."
  (dolist (achievement fetched)
    (orrient-cache--insert-achievement achievement (decode-time)))
  (when callback
    (funcall callback (append fetched cached))))

(defun orrient-api--handler-dailies (fetched cached callback)
  "Caches the parsed response from the GW2 dailies endpoint and
dispatches the callback.

This also embeds the relevent achievement
data (`orrient-api-achievement') in the `orrient-api-daily'
struct."
  (when-let ((dailies (or fetched cached)))
    (orrient-cache--insert-dailies dailies (decode-time))
    (let ((all `(,@(orrient-api-dailies-pve dailies)
                 ,@(orrient-api-dailies-pvp dailies)
                 ,@(orrient-api-dailies-wvw dailies)
                 ,@(orrient-api-dailies-fractals dailies)
                 ,@(orrient-api-dailies-special dailies))))
      (orrient-api--achievements
       (mapcar (lambda (daily)
                 (orrient-api-achievement-id (orrient-api-daily-achievement daily)))
               all)
       (lambda (achievements)
         (let ((achievements (mapcar (lambda (achievement)
                                       (cons (orrient-api-achievement-id achievement) achievement))
                                     achievements)))
           (dolist (daily all)
             (setf (slot-value daily 'achievement)
                   (alist-get (orrient-api-achievement-id (orrient-api-daily-achievement daily))
                              achievements)))
           (funcall callback dailies)))))))

(provide 'orrient-api)
;;; orrient-api.el ends here
