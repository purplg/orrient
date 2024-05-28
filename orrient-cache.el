;;; orrient-cache.el --- GW2 API cache -*- lexical-binding: t; -*-
(require 'sqlite)

(defconst orrient-cache--path (expand-file-name "orrient/" user-emacs-directory))
(defconst orrient-cache--filename "cache.db")

(defvar orrient-cache--db nil
  "The sqlite database object used for caching.")

(defun orrient-cache--db ()
  "Return the sqlite database object. Create it if it doesn't exist."
  (unless orrient-cache--db
    (make-directory orrient-cache--path t)
    (setq orrient-cache--db (sqlite-open (expand-file-name orrient-cache--filename orrient-cache--path))))
  orrient-cache--db)

(defun orrient-cache--close ()
  "Close the Orrient caching database."
  (when (and orrient-cache--db
             (sqlitep orrient-cache--db))
    (sqlite-close orrient-cache--db)
    (setq orrient-cache--db nil)))

(defun orrient-cache--clear ()
  "Delete all data in cache"
  (sqlite-execute (orrient-cache--db) "
DROP TABLE IF EXISTS achievements;
"))

(defun orrient-cache--init ()
  "Initialize the Orrient caching database."
  (sqlite-execute (orrient-cache--db) "
CREATE TABLE IF NOT EXISTS achievements(
  ID   INTEGER PRIMARY KEY NOT NULL,
  NAME TEXT                NOT NULL,
  BITS TEXT
);
"))


;; Achievements

(defun orrient-cache--insert-achievement (achievement timestamp)
  "Cache an achievement.
ACHIEVEMENT is a single achievement.

TIMESTAMP is `decoded-time' struct of the time the ACHIEVEMENT was requested."
  (let* ((bits (mapcar (lambda (bit)
                         (let ((id (orrient-api-achievement-bit-id bit))
                               (type (orrient-api-achievement-bit-type bit))
                               (text (orrient-api-achievement-bit-text bit)))
                           (let ((result `(:id ,id :type ,type)))
                             (when (append `(:text ,text)))
                             result)))
                       (orrient-api-achievement-bits achievement))))
    (sqlite-execute (orrient-cache--db)
                    (format "INSERT OR REPLACE INTO achievements (ID,NAME,BITS) VALUES (%d,\"%s\",\"%s\");"
                            (orrient-api-achievement-id achievement)
                            (string-replace "\"" "''" (orrient-api-achievement-name achievement))
                            (string-replace "\"" "''" (json-serialize (apply #'vector bits)))))))

(defun orrient-cache--get-achievement (id)
  "Return the achievement with the id ID."
  (when-let ((result (car (sqlite-select
                           (orrient-cache--db)
                           (format "SELECT * FROM achievements WHERE id=%d"
                                   id)))))
    (make-orrient-api-achievement
     :id (pop result)
     :name (pop result)
     :bits (let ((json (pop result)))
             (mapcar
              (lambda (bit)
                (make-orrient-api-achievement-bit
                 :id (gethash "id" bit)
                 :type (gethash "type" bit)
                 :text (gethash "text" bit))        )
              (json-parse-string
               (string-replace "''" "\"" json)
               :array-type 'list))))))

(defun orrient-cache--get-achievements (&optional ids)
  "Return all the achievements with the list of id's in IDS."
  (if ids
      (seq-map
       (lambda (result)
         (make-orrient-api-achievement :id (pop result) :name (pop result)))
       (sqlite-select (orrient-cache--db)
                      (concat "SELECT * FROM achievements WHERE id IN ( "
                              (string-join (mapcar #'prin1-to-string ids)
                                           ", ")
                              " )")))
    (seq-map
       (lambda (result)
         (make-orrient-api-achievement :id (pop result) :name (pop result)))
       (sqlite-select (orrient-cache--db)
                      "SELECT * FROM achievements"))))


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
