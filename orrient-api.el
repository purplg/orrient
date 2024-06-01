;;; orrient-api.el --- GW2 API -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'subr-x)
(require 'eieio)

(require 'plz)

(require 'orrient-model)
(require 'orrient-cache)

(defcustom orrient-api-key nil
  "")

(defconst orrient-api--url "https://api.guildwars2.com/")
(defconst orrient-api--endpoints `((achievement :path "v2/achievements"
                                                :requires-auth nil
                                                :response orrient-achievement)
                                   (account-achievement :path "v2/account/achievements"
                                                        :requires-auth t
                                                        :response orrient-account-achievement)
                                   (item :path "v2/items"
                                         :requires-auth nil
                                         :response orrient-item)))

(defun orrient-api--endpoint (endpoint)
  (plist-get (alist-get endpoint orrient-api--endpoints) :path))

(defun orrient-api--endpoint-auth-p (endpoint)
  (plist-get (alist-get endpoint orrient-api--endpoints) :requires-auth))

(defun orrient-api--endpoint-response-type (endpoint)
  (plist-get (alist-get endpoint orrient-api--endpoints) :response))

(cl-defmethod orrient-api--request ((class (subclass orrient-api)) ids callback)
  (let* ((cached (orrient-cache--get class ids))
         (cached-ids (seq-map (lambda (struct) (slot-value struct :id)) cached))
         (missed (seq-filter (lambda (item) (not (memq item cached-ids))) ids)))
    (if missed
        (when-let* ((endpoint (intern (string-remove-prefix "orrient-" (symbol-name class))))
                    (path (orrient-api--endpoint endpoint))
                    (url (concat orrient-api--url
                                 path
                                 "?ids="
                                 (string-join (mapcar #'number-to-string missed) ","))))
          (message "url: %s" url)
          (plz 'get url
            :as 'response
            :headers (when (orrient-api--endpoint-auth-p endpoint)
                       `(("Authorization" . ,(concat "Bearer " orrient-api-key))))
            :then (lambda (response)
                    (let* ((fetched (thread-first response
                                                  (plz-response-body)
                                                  (json-parse-string :array-type 'list)))
                           (fetched (mapcar (lambda (item)
                                              (orrient-api--from-response class item))
                                            fetched)))
                      (dolist (item fetched)
                        (message "caching: %S" (slot-value item :id))
                        (orrient-cache--insert item))
                      (let ((combined (append fetched cached)))
                        (funcall callback combined))))
            :else (lambda (err)
                    (let* ((response (plz-error-response err))
                           (code (plz-response-status response)))
                      (message "error: %s" code)))))
      (funcall callback cached))))

(cl-defgeneric orrient-api--from-response (response))

;; * Achievements

(cl-defmethod orrient-api--from-response ((obj (subclass orrient-achievement)) json)
  (orrient-achievement
   :id (gethash "id" json)
   :bits (thread-last (gethash "bits" json)
                      (seq-filter (lambda (bit) (gethash "type" bit)))
                      (mapcar (lambda (bit)
                                (orrient-achievement-bit
                                 :id (gethash "id" bit)
                                 :type (gethash "type" bit)
                                 :text (gethash "text" bit)))))
   :name (gethash "name" json)))

(defun orrient-api--populate-achievements (&optional page)
  "CALLBACK called for every single achievement returned by the endpoint."
  (let ((page (or page 0))
        (page-size 200))
    (orrient-api--request 'achievements
      :query `(("page_size" ,page-size)
               ("page" ,page))
      :callback (lambda (response)
                  (orrient-api--achievements:handler response)
                  (let ((page-total (thread-last response
                                                 (plz-response-headers)
                                                 (alist-get 'x-page-total)
                                                 (string-to-number))))
                    (when (< page page-total)
                      (orrient-api--populate-achievements (1+ page))))))))

(defun orrient-api--parse-achievement-page (response)
  "Used by `orrient-api--achievements' to parse a response from the
GW2 API.

See: `https://wiki.guildwars2.com/wiki/API:2/achievements'"
  (let ((body (plz-response-body response)))
    (dolist (achievement (seq-map (lambda (achievement)
                                    (orrient-achievement
                                     :id (gethash "id" achievement)
                                     :name (gethash "name" achievement)))
                                  (json-parse-string body)))
      (orrient-cache--insert-achievement achievement (decode-time)))))

;; * Items

(cl-defmethod orrient-api--from-response ((obj (subclass orrient-item)) json)
  (orrient-item
   :id (gethash "id" json)
   :name (gethash "name" json)))


;; Dailies

;; TODO Obsolete. Replace with Wizard's Vault

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
