;;; orrient-cache.el --- GW2 API cache -*- lexical-binding: t; -*-
(require 'sqlite)
(require 'emacsql)
(require 'emacsql-sqlite)

(defconst orrient-cache--path (expand-file-name "orrient/" user-emacs-directory))
(defconst orrient-cache--filename "cache.db")

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
           [ :drop-table :if-exists achievements])
  (emacsql (orrient-cache--db)
           [ :drop-table :if-exists account_achievements]))

(defun orrient-cache--init ()
  "Initialize the Orrient caching database."
  (interactive)
  (emacsql (orrient-cache--db)
           [ :create-table :if-not-exists achievements
             ([(id integer :primary-key)
               (name text)
               (bits object)])])
  (emacsql (orrient-cache--db)
           [ :create-table :if-not-exists account_achievements
             ([(id integer :primary-key)
               (bits object)
               (current integer)
               (max integer)
               (done integer :not-null)
               (repeated number)
               (unlocked boolean)])])
  (emacsql (orrient-cache--db)
           [ :create-table :if-not-exists items
             ([(id integer :primary-key)
               (name text :not-null)
               (discovered boolean)])]))


;; Achievements

(defun orrient-cache--insert-achievement (achievement timestamp)
  "Cache an achievement.
ACHIEVEMENT is a single achievement.

TIMESTAMP is `decoded-time' struct of the time the ACHIEVEMENT was requested."
  (let* ((id (slot-value achievement :id))
         (name (slot-value achievement :name))
         (bits (mapcar (lambda (bit)
                         (let ((id (slot-value bit :id))
                               (type (slot-value bit :type))
                               (text (slot-value bit :text)))
                           `(:id ,id :type ,type :text, text)))
                       (slot-value achievement :bits))))
    (emacsql orrient-cache--db
             [ :insert :or :replace
               :into achievements
               :values ([$s1 $s2 $s3])]
             id name bits)))

(defun orrient-cache--get-achievement (id)
  "Return the achievement with the id ID."
  (when-let ((result (orrient-cache--get-achievements (list id))))
    (pop result)))

(defun orrient-cache--get-achievements (&optional ids)
  "Return all the achievements with the list of id's in IDS."
  (seq-map
   (lambda (result)
     (orrient-achievement
      :id (pop result)
      :name (pop result)
      :bits (mapcar
             (lambda (bit)
               (orrient-achievement-bit
                :id (plist-get bit :id)
                :type (plist-get bit :type)
                :text (plist-get bit :text)))
             (pop result))))
   (if ids
       (emacsql (orrient-cache--db)
                [ :select * :from achievements
                  :where (in id $v1)]
                (apply #'vector ids))
     (emacsql (orrient-cache--db) [ :select * :from achievements]))))

(defun orrient-cache--insert-account-achievement (account-achievement timestamp)
  "Cache an account achievement.
ACCOUNT-ACHIEVEMENT is a single account achievement.

TIMESTAMP is `decoded-time' struct of the time the ACCOUNT-ACHIEVEMENT was requested."
  (let* ((id (slot-value account-achievement :id))
         (bits (slot-value account-achievement :bits))
         (current (slot-value account-achievement :current))
         (max (slot-value account-achievement :max))
         (done (slot-value account-achievement :done))
         (repeated (slot-value account-achievement :repeated))
         (unlocked (slot-value account-achievement :unlocked)))
    (emacsql orrient-cache--db
             [ :insert :or :replace
               :into account_achievements
               :values ([$s1 $s2 $s3 $s4 $s5 $s6 $s7])]
             id bits current max (if done 1 0) repeated unlocked)))

(defun orrient-cache--get-account-achievements (&optional ids)
  "Return all the achievements with the list of id's in IDS."
  (seq-map
   (lambda (result)
     (orrient-account-achievement
      :id (pop result)
      :bits (pop result)
      :current (pop result)
      :max (pop result)
      :done (if (= (pop result) 1) t nil)
      :repeated (pop result)
      :unlocked (pop result)))
   (emacsql (orrient-cache--db)
            [ :select * :from account_achievements
              :where (in id $v1)]
            (apply #'vector ids))))



;; Items

(defun orrient-cache--insert-item (item timestamp)
  "Cache an item.
ITEM is a single item.

TIMESTAMP is `decoded-time' struct of the time the ITEM was requested."
  (let* ((id (slot-value item :id))
         (name (slot-value item :name))
         (discovered (slot-value item :discovered)))
    (emacsql orrient-cache--db
             [ :insert :or :replace
               :into item
               :values ([$s1 $s2 $s3])]
             id name (if discovered 1 0))))

(defun orrient-cache--get-item (id)
  "Return the item with ID."
  (when-let ((result (orrient-cache--get-items (list id))))
    (pop result)))

(defun orrient-cache--get-items (&optional ids)
  "Return all the items with IDS."
  (seq-map
   (lambda (result)
     (orrient-item
      :id (pop result)
      :name (pop result)
      :discovered (pop result)))
   (if ids
       (emacsql (orrient-cache--db)
                [ :select * :from items
                  :where (in id $v1)]
                (apply #'vector ids))
     (emacsql (orrient-cache--db) [ :select * :from items]))))

(defun orrient-cache--set-items-discovered (ids)
  (sqlite-transaction (orrient-cache--db))
  (dolist (id ids)
    (sqlite-execute (orrient-cache--db)
                    (format "INSERT OR REPLACE INTO items (ID,NAME,DISCOVERED) VALUES (%d,(SELECT name FROM items WHERE id = %d),%s);"
                            id,
                            id
                            "TRUE")))
  (sqlite-commit (orrient-cache--db)))


;; Dailies

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
