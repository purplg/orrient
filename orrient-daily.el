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

(defun orrient-daily--render-section (title dailies)
  "Render a section on the dailies buffer.

TITLE is rendered at the top of the section.

DAILIES is a list of `orrient-api-daily' to be rendered in the section."
  (set-text-properties (point)
                       (progn (insert title)
                              (point))
                       `(face 'info-title-2))
  (insert ?\n)
  (dolist (daily (sort dailies
                       (lambda (a b)
                         (string< (orrient-api-achievement-name (orrient-api-daily-achievement a))
                                  (orrient-api-achievement-name (orrient-api-daily-achievement b))))))
    (insert (orrient-api-achievement-name (orrient-api-daily-achievement daily)))
    (insert ?\n)))

(defun orrient-daily--render ()
  (erase-buffer)
  (orrient-daily--render-section "PvE" (orrient-api-dailies-pve orrient-daily--dailies))
  (insert ?\n)
  (orrient-daily--render-section "PvP" (orrient-api-dailies-pvp orrient-daily--dailies))
  (insert ?\n)
  (orrient-daily--render-section "WvW" (orrient-api-dailies-wvw orrient-daily--dailies))
  (insert ?\n)
  (orrient-daily--render-section "Fractals" (orrient-api-dailies-fractals orrient-daily--dailies))
  (when-let ((special (orrient-api-dailies-special orrient-daily--dailies)))
    (insert ?\n)
    (orrient-daily--render-section "Special" special)))

;;;###autoload
(defun orrient-daily-open (&optional interactive)
  "Open the daily dailies buffer."
  (interactive)
  (orrient--display-buffer
   (orrient-daily-mode)
   (not interactive)))

(define-derived-mode orrient-daily-mode orrient-mode "GW2 Dailies"
  "View Guild Wars 2 dailies."
  :group 'orrient-daily
  :syntax-table nil
  :abbrev-table nil
  :interactive nil
  (let ((inhibit-read-only t))
    (orrient-daily--update)))

(provide 'orrient-daily)
;;; orrient-daily.el ends here
