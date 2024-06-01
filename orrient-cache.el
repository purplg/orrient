;;; orrient-cache.el --- GW2 API cache -*- lexical-binding: t; -*-
(require 'sqlite)
(require 'emacsql)
(require 'emacsql-sqlite)

(require 'orrient-model)

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
           [ :drop-table :if-exists achievement])
  (emacsql (orrient-cache--db)
           [ :drop-table :if-exists account-achievement])
  (emacsql (orrient-cache--db)
           [ :drop-table :if-exists item]))

(defun orrient-cache--init ()
  "Initialize the Orrient caching database."
  (interactive)
  (emacsql (orrient-cache--db)
           [ :create-table :if-not-exists achievement
             ([(id integer :primary-key)
               (name text)
               (bits object)])])
  (emacsql (orrient-cache--db)
           [ :create-table :if-not-exists account-achievements
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
               (name text :not-null)
               (discovered boolean)])]))

(cl-defmethod orrient-cache--get-all ((class (subclass orrient-api)))
  (let ((table (intern (string-remove-prefix "orrient-" (symbol-name orrient-achievement)))))
    (seq-map
     (lambda (result) (orrient-cache-from-db class result))
     (emacsql (orrient-cache--db)
              [ :select * :from $i1]
              table))))

(cl-defmethod orrient-cache--get ((class (subclass orrient-api)) ids)
  (let ((table (intern (string-remove-prefix "orrient-" (symbol-name class))))
        (ids (apply #'vector ids)))
    (seq-map
     (lambda (result)
       (orrient-cache-from-db class result))
     (emacsql (orrient-cache--db)
              [ :select * :from $i1
                :where (in id $v2)]
              table ids))))

(cl-defmethod orrient-cache--insert ((obj orrient-api) &rest achievements)
  (let ((table (intern (string-remove-prefix "orrient-" (symbol-name (eieio-object-class obj)))))
        (values (thread-last (orrient-cache-to-db obj)
                             (apply #'vector))))
    (emacsql orrient-cache--db
             [ :insert :or :replace
               :into $i1
               :values ($v2)]
             table values)))

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
             (pop result))))

(cl-defmethod orrient-cache-to-db ((obj orrient-achievement))
  (list (slot-value obj :id)
        (slot-value obj :name)
        (mapcar (lambda (bit)
                  (let ((id (slot-value bit :id))
                        (type (slot-value bit :type))
                        (text (slot-value bit :text)))
                    `(:id ,id :type ,type :text, text)))
                (slot-value obj :bits))))

;; * Account Achievements
(cl-defmethod orrient-cache-from-db ((class (subclass orrient-account-achievement)) result)
  (orrient-account-achievement
      :id (pop result)
      :bits (pop result)
      :current (pop result)
      :max (pop result)
      :done (if (= (pop result) 1) t nil)
      :repeated (pop result)
      :unlocked (pop result)))

(cl-defmethod orrient-cache-to-db ((obj orrient-account-achievement))
  (message "caching: %S" obj)
  (list (slot-value obj :id)
        (slot-value obj :bits)
        (slot-value obj :current)
        (slot-value obj :max)
        (if (slot-value obj :done) 1 0)
        (slot-value obj :repeated)
        (if (slot-value obj :unlocked) 1 0)))

;; * Items
(cl-defmethod orrient-cache-from-db ((class (subclass orrient-item)) result)
  (orrient-item
      :id (pop result)
      :name (pop result)))

(cl-defmethod orrient-cache-to-db ((obj orrient-item))
  (list (slot-value obj :id)
        (slot-value obj :name)))

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
