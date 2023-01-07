;;; orrient-event.el --- Event information for GW2 -*- lexical-binding: t; -*-
(require 'orrient-data)
(require 'orrient-timers)

(defvar orrient-event-buffer-format "*orrient-event: %s*")

(defun orrient-event--buffer-name (event)
  (format orrient-event-buffer-format (orrient-event-name event)))

(defmacro orrient-event--with-buffer (event &rest body)
  "Like `with-current-buffer' but with `orrient-event-buffer'.
BODY is evaluated with `orrient-event-buffer'"
  `(when-let ((buffer (get-buffer (orrient-event--buffer-name event))))
    (with-current-buffer buffer
      ,@body)))

;;;###autoload
(defun orrient-event-open (event)
  "Open a GW2 event buffer.
EVENT is a `orrient-event' struct that is to be rendered."
  (interactive
   (let* ((completions (mapcar (lambda (event)
                                 (cons (orrient-event-name event) event))
                               (flatten-list (mapcar #'orrient-meta-events orrient-schedule)))))
     (list (cdr (assoc (completing-read "Event: " completions) completions)))))
  (let* ((buffer (get-buffer-create (orrient-event--buffer-name event))))
    (orrient--display-buffer buffer)
    (orrient-event--with-buffer
     event
     (orrient-event--render event (orrient-timers--current-time)))))

(defun orrient-event--render (event time)
  (erase-buffer)
  (insert (propertize (orrient-event-name event) 'face 'info-title-1))
  (goto-char (point-min)))

(define-derived-mode orrient-event-mode special-mode "GW2 Event Information"
  "View Guild Wars 2 Event."
  :group 'orrient
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

(provide 'orrient-event)
;;; orrient-event.el ends here
