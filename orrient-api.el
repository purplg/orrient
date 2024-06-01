;;; orrient-api.el --- GW2 API -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'subr-x)
(require 'eieio)

(require 'plz)

(require 'orrient-cache)
(require 'orrient-model)

(defcustom orrient-api-key nil
  "")

(defconst orrient-api--url "https://api.guildwars2.com/")
(defconst orrient-api--endpoints '((achievements "v2/achievements" nil)
                                   (account-achievements "v2/account/achievements" t)
                                   (dailies "v2/achievements/daily" nil)
                                   (dailies-tomorrow "v2/achievements/daily/tomorrow" nil)
                                   (items "v2/items" nil)
                                   (discovered-items "v1/items" nil)))

(defun orrient-api--endpoint (endpoint)
  (concat orrient-api--url
          (car (alist-get endpoint orrient-api--endpoints))))

(defun orrient-api--endpoint-auth-p (endpoint)
  (cadr (alist-get endpoint orrient-api--endpoints)))

(cl-defun orrient-api--request (endpoint
                                &key
                                query
                                callback)
  "Make a request to the GW2 API.
PATH is the full endpoint path to query.

QUERY is the arguments to URL-BUILD-QUERY-STRING to add query
arguments to the request."
  (declare (indent defun))
  (let ((url (if query
                 (concat (orrient-api--endpoint endpoint) "?" (url-build-query-string query))
               (orrient-api--endpoint endpoint))))
    (message "url: %s" url)
    (plz 'get url
      :as 'response
      :headers (when (orrient-api--endpoint-auth-p endpoint)
                 `(("Authorization" . ,(concat "Bearer " orrient-api-key))))
      :then callback
      :else (lambda (err)
              (let* ((response (plz-error-response err))
                     (code (plz-response-status response)))
                (message "error: %s" code))))))


;; Achievements

(defun orrient-api--achievements (ids &optional callback)
  "Retrieve data about achievements.
IDS is list of achievement ids to resolve.

Will return with result immediately if cached. Otherwise, use CALLBACK
to get the result asynchronously."
  (let* ((cached (orrient-cache--get-achievements ids))
         (cached-ids (seq-map (lambda (achievement)
                                (slot-value achievement :id))
                              cached))
         (missed (seq-filter (lambda (item)
                               (not (memq item cached-ids)))
                             cached-ids)))
    (if missed
        (orrient-api--request 'achievements
          :callback (lambda (response)
                      (orrient-api--achievements:handler
                       response
                       cached
                       callback)))
      (orrient-api--achievements:handler nil
                                         cached
                                         callback))))

(defun orrient-api--achievement (id &optional callback)
  "Retrieve data about a single achievement.
ID is achievement id to resolve."
  (when-let ((result (orrient-api--achievements `(,id) callback)))
    (car result)))

(defun orrient-api--achievements:handler (response &optional cached callback)
  "Caches the parsed response from the GW2 achievements endpoint and
dispatches the callback."
  (let* ((fetched (and response
                       (thread-first response
                                     (plz-response-body)
                                     (json-parse-string :array-type 'list)))))
    (dolist (achievement fetched)
      (let ((struct (orrient-achievement
                     :id (gethash "id" achievement)
                     :bits (thread-last (gethash "bits" achievement)
                                        (seq-filter (lambda (bit) (gethash "type" bit)))
                                        (mapcar (lambda (bit)
                                                  (orrient-achievement-bit
                                                   :id (gethash "id" bit)
                                                   :type (gethash "type" bit)
                                                   :text (gethash "text" bit)))))
                     :name (gethash "name" achievement))))
        (orrient-cache--insert-achievement struct (decode-time))))
    (let ((combined (append fetched cached)))
      (if callback
          (funcall callback combined)
        combined))))

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

(defun orrient-api--account-achievement (id &optional callback)
  "Retrieve account data about a single achievement.
ID is achievement id to resolve."
  (when-let ((result (orrient-api--account-achievements `(,id) callback)))
    (car result)))

(defun orrient-api--account-achievements (ids &optional callback)
  "Retrieve data about achievements for the configured account.
IDS is list of achievement ids to resolve."
  (let* ((cached (orrient-cache--get-account-achievements ids))
         (cached-ids (seq-map (lambda (achievement)
                                (slot-value achievement :id))
                              cached))
         (uncached (seq-filter (lambda (item)
                                 (not (memq item cached-ids)))
                               ids)))
    (if uncached
        (orrient-api--request 'account-achievements
          :query (list (append '("ids") uncached))
          :callback (lambda (response)
                      (orrient-api--account-achievements:handler response
                                                                 cached
                                                                 callback)))
      (orrient-api--account-achievements:handler nil
                                                 cached
                                                 callback))))

(defun orrient-api--account-achievements:handler (response cached callback)
  (let* ((fetched (and response
                       (thread-first response
                                     (plz-response-body)
                                     (json-parse-string
                                      :array-type 'list
                                      :false-object nil)))))
    (dolist (achievement fetched)
      (let ((struct (orrient-account-achievement
                     :id (gethash "id" achievement)
                     :bits (gethash "bits" achievement)
                     :current (gethash "current" achievement)
                     :max (gethash "max" achievement)
                     :done (gethash "done" achievement)
                     :repeated (gethash "repeated" achievement)
                     :unlocked (gethash "unlocked" achievement))))
        (orrient-cache--insert-account-achievement struct (decode-time))))
    (let ((combined (append fetched cached)))
      (if callback
          (funcall callback combined)
        combined))))


;; Items

(defun orrient-api--items (ids &optional callback)
  "Retrieve data about items.
IDS is list of item ids to resolve."
  (let* ((cached (orrient-cache--get-items ids))
         (cached-ids (seq-map (lambda (item)
                                (orrient-api-item-id item))
                              cached))
         (uncached (seq-filter (lambda (item)
                                 (not (memq item cached-ids)))
                               ids)))
    (if uncached
        (let ((url (concat (alist-get 'items orrient-api--endpoints)
                            "?ids="
                            (string-join (mapcar #'prin1-to-string uncached) ","))))
          (orrient-api--request url
                                :handler #'orrient-api--handler-items
                                :callback callback))
      (orrient-api--handler-items nil
                                         cached
                                         callback))))

(defun orrient-api--item (id &optional callback)
  "Retrieve data about a single item.
ID is item id to resolve."
  (orrient-api--items `(,id) callback))

(defun orrient-api--handler-items (response cached callback)
  "Caches the parsed response from the GW2 items endpoint and
dispatches the callback."
  (let* ((fetched (and response
                       (thread-first response
                                     (plz-response-body)
                                     (json-parse-string :array-type 'list)))))
    (dolist (item fetched)
      (let ((struct (make-orrient-api-item
                     :id (gethash "id" item)
                     :name (gethash "name" item))))
        (orrient-cache--insert-item struct (decode-time))))
    (when callback
      (funcall callback (append fetched cached)))))

(defun orrient-api--discovered-items ()
  (orrient-api--request (alist-get 'discovered-items orrient-api--endpoints)
                        #'orrient-api--discovered-items:handler
                        nil
                        nil
                        (lambda (&rest args) (message "args: %s" args))))

(defun orrient-api--discovered-items:handler (response cached callback)
  ""
  (let* ((fetched (and response
                       (thread-first response
                                     (plz-response-body)
                                     (json-parse-string :array-type 'list)))))
    (orrient-cache--set-items-discovered (gethash "items" fetched))))


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
