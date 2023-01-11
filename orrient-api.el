;;; orrient-api.el --- GW2 API -*- lexical-binding: t; -*-
(require 'plz)

(require 'orrient-cache)

(defconst orrient-api--url "https://api.guildwars2.com")
(defconst orrient-api--endpoints '((achievements . "/v2/achievements")
                                   (dailies . "/v2/achievements/daily")
                                   (dailies-tomorrow . "/v2/achievements/daily/tomorrow")
                                   (items . "/v2/items")))

(cl-defstruct orrient-api-achievement
  id
  name
  rewards)

(cl-defstruct orrient-api-daily
  achievement-id
  level-min
  level-max
  required-access
  type)

(cl-defstruct orrient-api-dailies
  pve
  pvp
  wvw
  fractals
  special)

(cl-defstruct orrient-api-item
  id
  name)


;; Requests
(defun orrient-api--achievements (ids &optional callback)
  "Retrieve data about achievements.
IDS is list of achievement ids to resolve."
  (let* ((cached (orrient-cache--get-achievements ids))
         (uncached (seq-filter (lambda (item)
                                 (not (memq item cached)))
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
  (if-let ((dailies (orrient-cache--get-dailies)))
      (orrient-api--handler-dailies dailies
                                    nil
                                    callback)
    (let ((path (alist-get 'dailies orrient-api--endpoints)))
      (orrient-api--request path
                            #'orrient-api--parse-dailies
                            #'orrient-api--handler-dailies
                            nil
                            callback))))

(defun orrient-api--request (path parser handler cached &optional callback)
  "Make a request to the GW2 API.
PATH is the full endpoint path to query.

CACHED is a list of items to be append to the result of the API
query."
  (let ((url (concat orrient-api--url path)))
    (hass--debug "REQUEST" "%S" path)
    (plz 'get url
      :as parser
      :then (lambda (buffer) (funcall handler buffer cached callback)))))


;; PARSERS

;; A parser takes no arguments and is expected to be called in the buffer with
;; the response payload to utilize `json-parse-buffer'

(defun orrient-api--parse-achievements ()
  (let ((achievements (json-parse-buffer)))
    (seq-map (lambda (achievement)
               (make-orrient-api-achievement
                :id (gethash "id" achievement)
                :name (gethash "name" achievement)))
             achievements)))

(defun orrient-api--parse-dailies ()
  (let* ((dailies (json-parse-buffer))
         (pve (gethash "pve" dailies))
         (pvp (gethash "pvp" dailies))
         (wvw (gethash "wvw" dailies))
         (fractals (gethash "fractals" dailies))
         (special (gethash "special" dailies)))
    (make-orrient-api-dailies
     :pve (seq-map (lambda (daily)
                     (make-orrient-api-daily :achievement-id (gethash "id" daily)
                                             :type 'pve))
                   pve)
     :pvp (seq-map (lambda (daily)
                     (make-orrient-api-daily :achievement-id (gethash "id" daily)
                                             :type 'pvp))
                   pvp)
     :wvw (seq-map (lambda (daily)
                     (make-orrient-api-daily :achievement-id (gethash "id" daily)
                                             :type 'wvw))
                   wvw)
     :fractals (seq-map (lambda (daily)
                          (make-orrient-api-daily :achievement-id (gethash "id" daily)
                                                  :type 'fractals))
                        fractals)
     :special (seq-map (lambda (daily)
                         (make-orrient-api-daily :achievement-id (gethash "id" daily)
                                                 :type 'special))
                       special))))


;; RESPONSE HANDLERS

;; Each response handler must take the achievements fetched from remote, the
;; cached achievements, and a callback function to send this combined lists.

(defun orrient-api--handler-achievements (fetched cached callback)
  (dolist (achievement fetched)
    (orrient-cache--insert-achievement achievement (decode-time)))
  (when callback
    (funcall callback (append fetched cached))))

;; TODO Utilize `fetched' and `cached' properly.
(defun orrient-api--handler-dailies (dailies _cached callback)
  (orrient-cache--insert-dailies dailies (decode-time))
  (let ((pve (orrient-api-dailies-pve dailies))
        (pvp (orrient-api-dailies-pvp dailies))
        (wvw (orrient-api-dailies-wvw dailies))
        (fractals (orrient-api-dailies-fractals dailies))
        (special (orrient-api-dailies-special dailies)))
    (orrient-api--achievements (mapcar #'orrient-api-daily-achievement-id pve))
    (orrient-api--achievements (mapcar #'orrient-api-daily-achievement-id pvp))
    (orrient-api--achievements (mapcar #'orrient-api-daily-achievement-id wvw))
    (orrient-api--achievements (mapcar #'orrient-api-daily-achievement-id fractals))
    (orrient-api--achievements (mapcar #'orrient-api-daily-achievement-id special))))

(orrient-api--dailies
 (lambda (dailies)
   (message "%S" dailies)))

(let* ((dailies (make-orrient-api-dailies :pve `(,(make-orrient-api-daily :achievement-id 1))))
       (pve (orrient-api-dailies-pve dailies)))
  (mapcar #'orrient-api-daily-achievement-id pve))

(provide 'orrient-api)
;;; orrient-api.el ends here
