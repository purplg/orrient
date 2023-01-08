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

(defun orrient-event--format-eta (minutes)
  "Format an ETA shown on an event of its next occurance."
  (let ((hours (/ minutes 60))
        (minutes (% minutes 60)))
    ;; 6 is select here because the longest possible time between events is 2
    ;; hours and "2h 00m" is 6 characters. So we normalize all timestamps to 6
    ;; characters.
    (format " %s " (if (> hours 0)
	              (format "%dh %02dm" hours minutes)
                    (format "%02dm" minutes)))))

(defun orrient-event--render-event (instance time)
  (let* ((event-start (- (orrient-event-instance-start instance) time))
         (event-end (- (orrient-event-instance-end instance) time)))
    (if (< event-start time)
        (progn
          (insert "| started")
          (set-text-properties (point)
                               (progn (insert (orrient-event--format-eta event-start))
                                      (point))
                               `(face (,(orrient-timers--get-countdown-face event-start))))
          ;; Event end time
          (insert ?-)
          (set-text-properties (point)
                               (progn (insert (orrient-event--format-eta event-end))
                                      (point))
                               `(face (,(orrient-timers--get-countdown-face event-end)))))

      (insert "| starts")
      (set-text-properties (point)
                           (progn (insert (orrient-event--format-eta event-end))
                                  (point))
                           `(face (,(orrient-timers--get-countdown-face event-end)))))))

(defun orrient-event--render (event time)
  (erase-buffer)
  (insert (propertize (orrient-event-name event) 'face 'info-title-1))
  (insert ?\n ?\n)
  (set-text-properties (point)
                       (progn (insert "Upcoming")
                              (point))
                       `(face info-title-3))
  (insert ?\n)
  (let ((iter (orrient-timers--event-iter event time)))
    (cl-loop repeat 5
             do (orrient-event--render-event (iter-next iter)
                                             time)))
  (insert ?|)
  (goto-char (point-min)))

(define-derived-mode orrient-event-mode special-mode "GW2 Event Information"
  "View Guild Wars 2 Event."
  :group 'orrient
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

(provide 'orrient-event)
;;; orrient-event.el ends here