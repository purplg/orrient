;;; orrient-daily.el --- GW2 API dailies -*- lexical-binding: t; -*-
(require 'orrient)
(require 'orrient-api)

(defvar orrient-daily-buffer-suffix "dailies")
(defvar orrient-daily--dailies '())

(defmacro orrient-daily--with-buffer (&rest body)
  "Like `with-current-buffer' but with an `orrient-' buffer namespace.
BODY is evaluated in an orrient buffer."
  `(orrient--with-buffer ,orrient-daily-buffer-suffix
    ,@body))

(defvar orrient-daily-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'orrient--quit)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* 'normal map
        (kbd "q") #'orrient--quit))
    map)
  "Keymap for `orrient-daily-mode'.")

(defun orrient-daily--update ()
  (unless orrient-daily--dailies
    (orrient-api--dailies
     (lambda (dailies)
       (setq orrient-daily--dailies dailies)
       (orrient-daily--with-buffer
        (orrient-daily--render)))))
  (orrient-daily--render))

;; TODO Colorize / group by daily type
(defun orrient-daily--render ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (daily (sort orrient-daily--dailies
                         (lambda (a b)
                           (string< (orrient-api-achievement-name (orrient-api-daily-achievement a))
                                    (orrient-api-achievement-name (orrient-api-daily-achievement b))))))
      (insert (format "%S" (orrient-api-daily-type daily)))
      (set-text-properties (point)
                           (progn (insert (orrient-api-achievement-name (orrient-api-daily-achievement daily)))
                                  (point))
                           `(face 'info-title-2))
      (insert ?\n))))

;;;###autoload
(defun orrient-daily-open (&optional interactive)
  "Open the daily dailies buffer."
  (interactive)
  (orrient--display-buffer
   (orrient-daily--with-buffer (orrient-daily-mode))
   (not interactive)))

(define-derived-mode orrient-daily-mode orrient-mode "GW2 Dailies"
  "View Guild Wars 2 dailies."
  :group 'orrient-daily
  :syntax-table nil
  :abbrev-table nil
  :interactive t
  (orrient-daily--update))

(provide 'orrient-daily)
;;; orrient-daily.el ends here
