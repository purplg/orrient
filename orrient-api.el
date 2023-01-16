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
  id
  name
  rewards)

(cl-defstruct orrient-api-daily
  achievement
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


;; REQUESTS

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
  (dolist (achievement fetched)
    (orrient-cache--insert-achievement achievement (decode-time)))
  (when callback
    (funcall callback (append fetched cached))))

(defun orrient-api--handler-dailies (fetched cached callback)
  (when-let ((dailies (or fetched cached)))
    (orrient-cache--insert-dailies dailies (decode-time))
    (let ((pve (orrient-api-dailies-pve dailies))
          (pvp (orrient-api-dailies-pvp dailies))
          (wvw (orrient-api-dailies-wvw dailies))
          (fractals (orrient-api-dailies-fractals dailies))
          (special (orrient-api-dailies-special dailies)))
      (setq dailies `(,@pve ,@pvp ,@wvw ,@fractals ,@special))
      (orrient-api--achievements
       (mapcar (lambda (daily)
                 (orrient-api-achievement-id (orrient-api-daily-achievement daily)))
               dailies)
       (lambda (achievements)
         (let ((achievements (mapcar (lambda (achievement)
                                       (cons (orrient-api-achievement-id achievement) achievement))
                                     achievements)))
           (dolist (daily dailies)
             (setf (slot-value daily 'achievement)
                   (alist-get (orrient-api-achievement-id (orrient-api-daily-achievement daily))
                              achievements)))
           (funcall callback dailies)))))))

(provide 'orrient-api)
;;; orrient-api.el ends here