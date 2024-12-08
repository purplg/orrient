;;; orrient-event.el --- Event information for GW2 -*- lexical-binding: t; -*-
(require 'cl-lib)

(require 'orrient-schedule)

(defvar orrient-event--buffer-suffix-format "event: %s")

(defun orrient-event--buffer-name (event)
  (format orrient-event--buffer-suffix-format (orrient-event-name event)))

(defmacro orrient-event--with-buffer (event &rest body)
  "Like `with-current-buffer' but with `orrient-event-buffer'.
BODY is evaluated with `orrient-event-buffer'"
  (declare (indent defun))
  `(orrient--with-buffer (orrient-event--buffer-name ,event)
                         ,@body))

(defvar orrient-event-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'orrient--quit)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* 'normal map
        (kbd "q") #'orrient--quit
        (kbd "y") #'orrient-event-copy-waypoint
        (when (fboundp #'evil-define-key*)
          (evil-define-key* 'normal map
            (kbd "gr") #'orrient-event-revert))))
    map)
  "Keymap for `orrient-event-mode'.")

(defvar-local orrient-event nil
  "The event associated with the current buffer.")

(defvar orrient-event--watch nil
  "List of events to be notified when they start.")

(defface orrient-event-title
  '((t (:height 180 :inherit success)))
  "Face for the event name at the top of an event buffer."
  :group 'orrient)

(defface orrient-event-instance-label
  '((t (:height 140 :inherit info-title-2)))
  "Face for a the start time of single event instance."
  :group 'orrient)

(defun orrient-event-watch (event-instance)
  "Watch an event-instance to send a notification when it is
starting."
  (if (member event-instance orrient-event--watch)
      (progn
        (setq orrient-event--watch
              (remove event-instance orrient-event--watch))
        (message "orrient: Disabled notification when %s starts"
                 (orrient-event-name (orrient-event-instance-event event-instance))))
    (add-to-list 'orrient-event--watch event-instance)
    (message "orrient: Enabled notification when %s starts"
             (orrient-event-name (orrient-event-instance-event event-instance))))) 

;;;###autoload
(defun orrient-event-open (event)
  "Open a GW2 event buffer.
EVENT is a `orrient-event' struct that is to be rendered."
  (interactive
   (let ((completions (mapcar (lambda (event)
                                (cons (orrient-event-name event) event))
                              (flatten-list (mapcar #'orrient-meta-events orrient-schedule)))))
     (list (cdr (assoc (completing-read "Event: " completions) completions)))))
  (orrient--display-buffer
   (orrient-event--with-buffer event
     (orrient-event-mode)
     (setq orrient-event event)
     (orrient-event--render orrient-event (orrient-schedule--current-time)))))

(defun orrient-event-revert ()
  (interactive)
  (let ((inhibit-read-only t)
        (pos (point)))
    (orrient-event--render orrient-event (orrient-schedule--current-time))
    (goto-char pos)))

(defun orrient-event--render-event (instance time)
  (let* ((event-start (- (orrient-event-instance-start instance) time))
         (event-end (- (orrient-event-instance-end instance) time)))
    (set-text-properties
     (point)
     (progn
       (insert ?\n
               (format "%s" (let* ((time (+ (orrient-schedule--current-time) event-start))
                                   (hours (/ time 60))
                                   (minutes (% time 60)))
                              (format-time-string "%H:%M"
                                                  (encode-time 0 minutes hours 0 0 0))))
               ?\n)
       (point))
     '(face orrient-event-instance-label))
    (if (< event-start 0)
        (progn
          (insert "Started: ")
          (set-text-properties (point)
                               (progn (insert (orrient-schedule--format-eta event-start t))
                                      (point))
                               `(face (,(orrient-schedule--get-countdown-face event-start))))
          (insert ?\n)
          (insert "Ends in: ")
          (message "event-end: %s" event-end)
          (set-text-properties (point)
                               (progn (insert (orrient-schedule--format-eta event-end))
                                      (point))
                               `(face (,(orrient-schedule--get-countdown-face event-end)))))

      (insert "Starts in: ")
      (set-text-properties (point)
                           (progn (insert (orrient-schedule--format-eta event-start))
                                  (point))
                           `(face (,(orrient-schedule--get-countdown-face event-end)))))
    (insert ?\n)))

(defun orrient-event--render (event time)
  (erase-buffer)
  (insert (propertize (orrient-event-name event) 'face 'orrient-event-title))
  (insert ?\n ?\n)
  (when (orrient-event-waypoint event)
    (insert-button "[Copy waypoint]"
                   'action (lambda (_) (orrient--waypoint-copy orrient-event)))
    (insert " "))
  (let ((event-instance (iter-next (orrient-timers--event-iter event time))))
    (insert-button (if (member event-instance orrient-event--watch) "[Watch]" "[Unwatch]")
                   'action (lambda (_)
                             (orrient-event-watch event-instance)
                             (orrient-event-revert))))
  (insert ?\n ?\n)
  (set-text-properties (point)
                       (progn (insert "Upcoming")
                              (point))
                       `(face org-level-1))
  (insert ?\n)
  (let ((iter (orrient-timers--event-iter event time)))
    (cl-loop repeat 5
             do (orrient-event--render-event (iter-next iter)
                                             time)))
  (goto-char (point-min)))

(defun orrient-event-copy-waypoint ()
  "Copy the waypoint of the current event buffers' event to clipboard."
  (interactive)
  (orrient--waypoint-copy orrient-event))

(define-derived-mode orrient-event-mode orrient-mode "GW2 Event"
  "View Guild Wars 2 Event."
  :group 'orrient-event
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

(provide 'orrient-event)
;;; orrient-event.el ends here
