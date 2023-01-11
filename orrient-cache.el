;;; orrient-cache.el --- GW2 API cache -*- lexical-binding: t; -*-
(require 'sqlite)

(defconst orrient-cache--path (expand-file-name "orrient/" user-emacs-directory))
(defconst orrient-cache--filename "cache.db")

(defvar orrient-cache--db nil)

(defun orrient-cache--db ()
  (unless orrient-cache--db
    (make-directory orrient-cache--path t)
    (setq orrient-cache--db (sqlite-open (expand-file-name orrient-cache--filename orrient-cache--path))))
  orrient-cache--db)

(defun orrient-cache--store (key value)
  (orrient-cache--db))

(defun orrient-cache--close ()
  (when (and orrient-cache--db
             (sqlitep orrient-cache--db))
    (sqlite-close orrient-cache--db)
    (setq orrient-cache--db nil)))

(defun orrient-cache--init ()
  (sqlite-execute (orrient-cache--db)
    "DROP TABLE IF EXISTS dailies;")
  (sqlite-execute (orrient-cache--db)
    "CREATE TABLE dailies(
       ID   INT  PRIMARY KEY NOT NULL,
       TYPE TEXT             NOT NULL
     );")

  (sqlite-execute (orrient-cache--db)
    "DROP TABLE IF EXISTS achievements;")
  (sqlite-execute (orrient-cache--db)
    "CREATE TABLE achievements(
       ID   INT  PRIMARY KEY NOT NULL,
       NAME TEXT             NOT NULL
     );"))

(defun orrient-cache--insert-achievement (achievement timestamp)
  "Cache an achievement.
ACHIEVEMENT is a single achievement.

TIMESTAMP is `decoded-time' struct of the time the ACHIEVEMENT was requested."
  (sqlite-execute (orrient-cache--db)
    (format "INSERT OR REPLACE INTO achievements VALUES (%d, \"%s\");"
            (orrient-api-achievement-id achievement)
            (orrient-api-achievement-name achievement))))

(defun orrient-cache--insert-daily (daily timestamp)
  "Cache a daily.

TIMESTAMP is `decoded-time' struct of the time the DAILY was active."
  (sqlite-execute (orrient-cache--db)
    (format "INSERT OR REPLACE INTO dailies VALUES (%d, \"%s\");"
            (orrient-api-daily-achievement-id daily)
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
  (when-let ((pve (orrient-cache--get-dailies-of-type 'pve))
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
  (mapcar (lambda (daily)
            (make-orrient-api-daily :achievement-id (pop daily) :type (intern (pop daily))))
          (sqlite-select (orrient-cache--db)
                         (format "SELECT * FROM dailies WHERE TYPE='%s'" type))))

(defun orrient-cache--get-achievement (id)
  (car (sqlite-select (orrient-cache--db)
                      (format "SELECT * FROM achievements WHERE id=%d"
                              id))))

(defun orrient-cache--get-achievements (ids)
  (sqlite-select (orrient-cache--db)
                 (concat "SELECT * FROM achievements WHERE id IN ( "
                         (string-join (mapcar #'prin1-to-string ids)
                                      ", ")
                         " )")))

(provide 'orrient-cache)
;;; orrient-cache.el ends here
