;;; orrient-cache.el --- GW2 API cache -*- lexical-binding: t; -*-
(require 'sqlite)
(require 'emacsql)
(require 'emacsql-sqlite)

(require 'orrient-model)

(defconst orrient-cache--path (expand-file-name "orrient/" user-emacs-directory))
(defconst orrient-cache--filename "cache.db")

(defvar orrient-cache--shortest-refresh-time nil
  "The shortest refresh time recorded so far.")

(defvar orrient-cache--db nil
  "The sqlite database object used for caching.")

(defun orrient-cache--db ()
  (unless orrient-cache--db
    (setq orrient-cache--db
          (emacsql-sqlite
           (expand-file-name orrient-cache--filename orrient-cache--path))))
  orrient-cache--db)

(defun orrient-cache--close ()
  "Initialize the Orrient caching database."
  (interactive)
  (emacsql (orrient-cache--db)
           [ :drop-table :if-exists achievement])
  (emacsql (orrient-cache--db)
           [ :drop-table :if-exists account-achievement])
  (emacsql (orrient-cache--db)
           [ :drop-table :if-exists item]))

(defun orrient-cache--init ()
  "Initialize the Orrient caching database."
  (interactive)
  (emacsql (orrient-cache--db)
           [ :create-table :if-not-exists timestamp
             ([(id integer)
               (table text)
               (timestamp integer)])])
  (emacsql (orrient-cache--db)
           [ :create-table :if-not-exists achievement
             ([(id integer :primary-key)
               (name text)
               (bits object)
               (tiers object)])])
  (emacsql (orrient-cache--db)
           [ :create-table :if-not-exists account-achievement
             ([(id integer :primary-key)
               (bits object)
               (current integer)
               (max integer)
               (done integer :not-null)
               (repeated number)
               (unlocked boolean)])])
  (emacsql (orrient-cache--db)
           [ :create-table :if-not-exists item
             ([(id integer :primary-key)
               (name text :not-null)])])
  (emacsql (orrient-cache--db)
           [ :create-table :if-not-exists skin
             ([(id integer :primary-key)
               (name text :not-null)])]))

(cl-defmethod orrient-cache--get-all ((class (subclass orrient-api)))
  (let ((table (intern (string-remove-prefix "orrient-" (symbol-name orrient-achievement)))))
    (seq-map
     (lambda (result) (orrient-cache-from-db class result))
     (emacsql (orrient-cache--db)
              [ :select * :from $i1]
              table))))

(cl-defmethod orrient-cache--get ((class (subclass orrient-api)) ids &optional ignore-timeout)
  (let ((table (intern (string-remove-prefix "orrient-" (symbol-name class))))
        (ids (apply #'vector ids)))
    (seq-map
     (lambda (result)
       (orrient-cache-from-db class result))
     (if ignore-timeout
         (emacsql (orrient-cache--db)
                  [ :select * :from $i1
                    :where (in id $v2)]
                  table ids)
       (emacsql (orrient-cache--db)
                [ :select * :from $i1
                  :where (and (in id $v2)
                              (in id [ :select [id] :from timestamp
                                       :where (and (= table $s1)
                                                   (< (- $s3 timestamp) $s4))]))]
                table
                ids
                (time-convert nil 'integer)
                (let ((cache-time (orrient-cache-time class)))
                  (when (or (not orrient-cache--shortest-refresh-time)
                            (< cache-time orrient-cache--shortest-refresh-time))
                    (setq orrient-cache--shortest-refresh-time cache-time))
                  cache-time))))))

(cl-defmethod orrient-cache--insert ((obj orrient-api))
  (let ((table (intern (string-remove-prefix "orrient-" (symbol-name (eieio-object-class obj)))))
        (values (thread-last (orrient-cache-to-db obj)
                             (apply #'vector))))
    (emacsql orrient-cache--db
             [ :insert :or :replace
               :into $i1
               :values ($v2)]
             table values)
    (emacsql orrient-cache--db
             [ :insert :or :replace
               :into timestamp
               :values ([$s1 $s2 $s3])]
             (slot-value obj :id) table (time-convert nil 'integer))))

(cl-defgeneric orrient-cache-from-db (result)
  "Implement for EIEIO objects that can be fetched from the database
  cache.
RESULT is a sequential list of the fields fetched from the database
that should be used to construct a new object instance.")

(cl-defgeneric orrient-cache-to-db ()
  "Implement for EIEIO objects that can be inserted into the database
  cache.
This method should return a sequential list of fields to be inserted
respective to the columns in the database.")

(cl-defgeneric orrient-cache-to-db-error (id)
  "")

(cl-defgeneric orrient-cache-time ()
  "")

(cl-defmethod orrient-cache-time ((class (subclass orrient-api)))
  "By default, items are cached for 1000 seconds period of time. If a type
needs to be more frequently refresh, override this method and return
a new value."
  1000)

;; * Achievements
(cl-defmethod orrient-cache-from-db ((class (subclass orrient-achievement)) result)
  (orrient-achievement
   :id (pop result)
   :name (pop result)
   :bits (mapcar
          (lambda (bit)
            (orrient-achievement-bit
             :id (plist-get bit :id)
             :type (plist-get bit :type)
             :text (plist-get bit :text)))
          (pop result))
   :tiers (mapcar
           (lambda (tier)
             (orrient-achievement-tier
              :count (plist-get tier :count)
              :points (plist-get tier :points)))
           (pop result))))

(cl-defmethod orrient-cache-to-db ((obj orrient-achievement))
  (list (slot-value obj :id)
        (slot-value obj :name)
        (mapcar (lambda (bit)
                  (let ((id (slot-value bit :id))
                        (type (slot-value bit :type))
                        (text (slot-value bit :text)))
                    `(:id ,id :type ,type :text, text)))
                (slot-value obj :bits))
        (mapcar (lambda (tier)
                  (let ((count (slot-value tier :count))
                        (points (slot-value tier :points)))
                    `(:count ,count :points ,points)))
                (slot-value obj :tiers))))

(cl-defmethod orrient-cache-to-db-error ((class (subclass orrient-achievement)) id)
  (orrient-achievement :id id :name (format "Unknown achievement #%d" id)))

;; * Account Achievements
(cl-defmethod orrient-cache-from-db ((class (subclass orrient-account-achievement)) result)
  (orrient-account-achievement
      :id (pop result)
      :bits (pop result)
      :current (pop result)
      :max (pop result)
      :done (if (= (pop result) 1) t nil)
      :repeated (pop result)
      :unlocked (if (= (pop result) 1) t nil)))

(cl-defmethod orrient-cache-to-db ((obj orrient-account-achievement))
  (list (slot-value obj :id)
        (slot-value obj :bits)
        (slot-value obj :current)
        (slot-value obj :max)
        (if (slot-value obj :done) 1 0)
        (slot-value obj :repeated)
        (if (slot-value obj :unlocked) 1 0)))

(cl-defmethod orrient-cache-to-db-error ((class (subclass orrient-account-achievement)) id)
  nil)

(cl-defmethod orrient-cache-time ((class (subclass orrient-account-achievement)))
  "Account achievements should be refreshed more frequently."
  60)

;; * Items
(cl-defmethod orrient-cache-from-db ((class (subclass orrient-item)) result)
  (orrient-item
      :id (pop result)
      :name (pop result)))

(cl-defmethod orrient-cache-to-db ((obj orrient-item))
  (list (slot-value obj :id)
        (slot-value obj :name)))

(cl-defmethod orrient-cache-to-db-error ((class (subclass orrient-item)) id)
  (orrient-item :id id :name (format "Unknown item #%d" id)))

;; * Skins
(cl-defmethod orrient-cache-from-db ((class (subclass orrient-skin)) result)
  (orrient-skin
      :id (pop result)
      :name (pop result)))

(cl-defmethod orrient-cache-to-db ((obj orrient-skin))
  (list (slot-value obj :id)
        (slot-value obj :name)))

(cl-defmethod orrient-cache-to-db-error ((class (subclass orrient-skin)) id)
  (orrient-skin :id id :name (format "Unknown skin #%d" id)))

;; * Dailies

;; TODO Obsolete. Replace with Wizard's Vault

(defun orrient-cache--insert-daily (daily timestamp)
  "Cache a daily.

TIMESTAMP is `decoded-time' struct of the time the DAILY was active."
  (sqlite-execute (orrient-cache--db)
    (format "INSERT OR REPLACE INTO dailies VALUES (%d, \"%s\");"
            (orrient-api-achievement-id (orrient-api-daily-achievement daily))
            (orrient-api-daily-type daily))))

(defun orrient-cache--insert-dailies (dailies timestamp)
  "Cache a set of dailies.
DAILIES is a list of dailies.

TIMESTAMP is `decoded-time' struct of the time the DAILIES were active."
  (let* ((dailies `(,@(orrient-api-dailies-pve dailies)
                    ,@(orrient-api-dailies-pvp dailies)
                    ,@(orrient-api-dailies-wvw dailies)
                    ,@(orrient-api-dailies-fractals dailies)
                    ,@(orrient-api-dailies-special dailies))))
    (dolist (daily dailies)
      (orrient-cache--insert-daily daily timestamp))))

(defun orrient-cache--get-dailies ()
  "Return the current dailies in cache."
  (let ((pve (orrient-cache--get-dailies-of-type 'pve))
        (pvp (orrient-cache--get-dailies-of-type 'pvp))
        (wvw (orrient-cache--get-dailies-of-type 'wvw))
        (fractals (orrient-cache--get-dailies-of-type 'fractals))
        (special (orrient-cache--get-dailies-of-type 'special)))
    (make-orrient-api-dailies
     :pve pve
     :pvp pvp
     :wvw wvw
     :fractals fractals
     :special special)))

(defun orrient-cache--get-dailies-of-type (type)
  "Return the current dailies in cache of a certain type.

TYPE is either `pve', `pvp', `wvw', `fractals', or `special'."
  (mapcar
   (lambda (daily)
     (make-orrient-api-daily :achievement (make-orrient-api-achievement :id (pop daily))
                             :type (intern (pop daily))))
   (sqlite-select (orrient-cache--db)
                  (format "SELECT * FROM dailies WHERE TYPE='%s'" type))))

(provide 'orrient-cache)
;;; orrient-cache.el ends here
