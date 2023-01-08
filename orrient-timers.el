;;; orrient-timers.el --- Event Timers in GW2 -*- lexical-binding: t; -*-

;; Homepage: https://github.com/purplg/orrient
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Create and organize goals for yourself in Guild Wars 2

;; --------------------
;; Configuration

;;; Code:
(require 'generator)

(require 'orrient)
(require 'orrient-data)

(defvar orrient-timers-buffer "*orrient-timers*")

(defmacro orrient-timers--with-buffer (&rest body)
  "Like `with-current-buffer' but with `orrient-timers-buffer'.
BODY is evaluated with `orrient-timers-buffer'"
  `(when-let ((buffer (get-buffer orrient-timers-buffer)))
    (with-current-buffer buffer
      ,@body)))

(defvar orrient-timers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'widget-button-press)
    (define-key map (kbd "q") #'orrient--quit)
    (define-key map (kbd "C-n") #'orrient-timers-forward)
    (define-key map (kbd "C-p") #'orrient-timers-backward)
    (define-key map [tab] #'widget-forward)
    (define-key map [backtab] #'widget-backward)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* 'normal map
        (kbd "]") #'orrient-timers-forward
        (kbd "[") #'orrient-timers-backward
        (kbd "gk") #'orrient-timers-forward
        (kbd "gj") #'orrient-timers-backward
        (kbd "gt") #'orrient-timers-goto
        (kbd "q") #'orrient--quit))
    map)
  "Keymap for `orrient-timers-mode'.")

(defcustom orrient-timers-skip-step 5
  "Amount of time to skip when stepping forward or backwards in
time.")

(defcustom orrient-timers-snap-to-step t
  "Whether to snap time to `orrient-timers-skip-step' increments.
For example, if the time is currently 01:04 and
`orrient-timers-skip-step' is set to the default 15, then going
forward in time by calling `orrient-timers-forward' will snap to
01:15.")


;; User functions
(defun orrient-timers-forward (&optional step)
  "Skip forward in time by `orrient-timers-skip-step'.
Specify STEP to forward by that amount instead."
  (interactive)
  (orrient-timers--timer-cancel)
  (let ((step (or step orrient-timers-skip-step))
        (time (orrient-timers--time)))
    (orrient-timers--update
     (% (if orrient-timers-snap-to-step
            (* (1+ (/ time step)) step)
          (+ time step)) 1440))))

(defun orrient-timers-backward (&optional step)
  "Skip backward in time by `orrient-timers-skip-step'.
Specify STEP to forward by that amount instead."
  (interactive)
  (orrient-timers--timer-cancel)
  (let ((step (or step orrient-timers-skip-step))
        (time (orrient-timers--time)))
    (orrient-timers--update
     (if orrient-timers-snap-to-step
         (* (1- (/ time step)) step)
       (- time step)))))

(defun orrient-timers-now (&rest _)
  "Jump to the current system time."
  (interactive)
  (orrient-timers--timer-start)
  (orrient-timers--update (orrient-timers--current-time)))

(defun orrient-timers-goto (time)
  "Jump to a specified time.
TIME is in ISO 8601 format as specified by `parse-time-string'"
  (interactive
   (let ((user-time (parse-time-string (format "%s" (read-from-minibuffer "Time (UTC): ")))))
     (list (+ (* (decoded-time-hour user-time) 60)
              (decoded-time-minute user-time)))))
  (orrient-timers--timer-cancel)
  (orrient-timers--update time))


;; Sorting
(defun orrient-timers--schedule-sort (entry-a entry-b)
  "Predicate for `sort' that sorts by the order the meta appears in `orrient-schedule'.
Return t when ENTRY-A is before ENTRY-B."
  (let ((meta-a (plist-get (cdr (aref (nth 1 entry-a) 0)) 'orrient-meta))
        (meta-b (plist-get (cdr (aref (nth 1 entry-b) 0)) 'orrient-meta)))
    (< (cl-position meta-a orrient-schedule)
       (cl-position meta-b orrient-schedule))))

(defun orrient-timers--event-instance-sort (event-column entry-a entry-b)
  "Predicate for `sort' that sorts events by how soon they will occur next.
EVENT-COLUMN is the column index that was sorted.

Return t when ENTRY-A comes before ENTRY-B."
  (let ((category-a-minutes (orrient-event-instance-start (plist-get (cdr (aref (nth 1 entry-a) event-column)) 'orrient-event-instance)))
        (category-b-minutes (orrient-event-instance-start (plist-get (cdr (aref (nth 1 entry-b) event-column)) 'orrient-event-instance))))
    (< category-a-minutes category-b-minutes)))


;; Timers
(defvar orrient-timers--timer nil
  "Instance of the refresh timer.
If non-nil, then a `run-with-timer' timer is active.")

(defun orrient-timers--timer-start ()
  "Start the live update timer at the top of every minute."
  (orrient-timers--timer-cancel)
  (setq orrient-timers--timer
        (run-with-timer (- 60 (decoded-time-second (decode-time nil t nil)))
                        60
                        (lambda ()
                          (orrient-timers--with-buffer
                           (orrient-timers--update (orrient-timers--current-time)))))))

(defun orrient-timers--timer-cancel ()
  "Stop the live update timer if it's running."
  (when orrient-timers--timer
    (cancel-timer orrient-timers--timer)
    (setq orrient-timers--timer nil)))

(defun orrient-timers--timer-toggle ()
  "Toggle the live update timer."
  (if orrient-timers--timer
      (orrient-timers--timer-cancel)
    (orrient-timers--timer-start))
  (save-excursion
    (orrient-timers--render-buffer)))

(defun orrient-timers--current-time ()
  "Return current time in minutes from UTC 0."
  (let ((time (decode-time nil t nil)))
    (+ (* 60 (decoded-time-hour time))
       (decoded-time-minute time))))


;; Faces
;;; Category
(defface orrient-timers-category
  '((t (:extend t)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-core-tyria
  '((t (:background "#3f010c" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-1
  '((t (:background "#7b0418" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-2
  '((t (:background "#5c4a03" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-heart-of-thorns
  '((t (:background "#515c03" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-3
  '((t (:background "#4b7f40" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-path-of-fire
  '((t (:background "#7b4704" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-living-world-4
  '((t (:background "#662a77" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-icebrood-saga
  '((t (:background "#04497b" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defface orrient-timers-category-end-of-dragons
  '((t (:background "#0b6b75" :extend t :inherit 'orrient-timers-category)))
  "Face for meta heading in the event timers buffer."
  :group 'orrient)

(defun orrient-timers--get-category-face (category)
  "Convert the CATEGORY symbol into a face."
  (pcase category
    ('core-tyria 'orrient-timers-category-core-tyria)
    ('living-world-1 'orrient-timers-category-living-world-1)
    ('living-world-2 'orrient-timers-category-living-world-2)
    ('heart-of-thorns 'orrient-timers-category-heart-of-thorns)
    ('living-world-3 'orrient-timers-category-living-world-3)
    ('path-of-fire 'orrient-timers-category-path-of-fire)
    ('living-world-4 'orrient-timers-category-living-world-4)
    ('icebrood-saga 'orrient-timers-category-icebrood-saga)
    ('end-of-dragons 'orrient-timers-category-end-of-dragons)))

;;; Countdown
(defface orrient-timers-countdown-now
  '((t (:inherit 'error)))
  "Orrient face for time remaining when an event is happening now."
  :group 'orrient)

(defface orrient-timers-countdown-soon
  '((t (:inherit 'warning)))
  "Orrient face for time remaining when an event is happening soon."
  :group 'orrient)

(defface orrient-timers-countdown-later
  '((t ()))
  "Orrient face for time remaining when an event is not happening soon."
  :group 'orrient)

(defun orrient-timers--get-countdown-face (minutes)
  (cond ((<= minutes 0) 'orrient-timers-countdown-now)
        ((< minutes 15) 'orrient-timers-countdown-soon)
        (t 'orrient-timers-countdown-later)))


;; Callbacks
(defun orrient-timers--button-meta (button)
  (let ((meta (button-get button 'orrient-meta)))
    (orrient-meta-open meta)))

(defun orrient-timers--button-event (button)
  (let* ((instance (button-get button 'orrient-event-instance))
        (event (orrient-event-instance-event instance)))
    (orrient-event-open event)))


;; Event prediction
(defun orrient-timers--event-next (event time)
  "Returns the next `orrient-event-instance' of EVENT starting from
TIME in minutes from UTC 0."
  (let* ((offset (orrient-event-offset event))
         (frequency (orrient-event-frequency event))
         (length (orrient-event-length event))
         (start-time (+ offset (* (/ time frequency) frequency)))
         (end-time (+ start-time length)))
    (while (<= end-time time)
      (setq start-time (+ start-time frequency))
      (setq end-time (+ start-time length)))
    (make-orrient-event-instance :event event
                                 :start start-time
                                 :end end-time)))

(iter-defun orrient-timers--event-iter (event time &optional reset-new-day)
  "Returns next start time `orrient-event', in minutes, from UTC 0."
  (let* ((offset (orrient-event-offset event))
         (frequency (orrient-event-frequency event))
         (length (orrient-event-length event))
         (index (/ time frequency))
         (start-time (+ offset (* index frequency)))
         (end-time (+ start-time length)))

    ; If event has passed, skip to next occurance.
    (while (<= end-time time)
      (setq index (1+ index))
      (setq start-time (+ offset (* index frequency)))
      (setq end-time (+ start-time length)))

    (while t
      (setq start-time (+ offset (* index frequency)))

      (when (and reset-new-day
                 (>= start-time 1440))
        (setq index 0)
        (setq start-time offset))

      (iter-yield (make-orrient-event-instance
                   :event event
                   :start start-time
                   :end (+ start-time (orrient-event-length event))))
      (setq index (1+ index)))))

(iter-defun orrient-timers--meta-iter (meta time)
  "Returns a `orrient-event-instance' of the next `orrient-event' to occur."
  (let ((events (orrient-meta-events meta)))
    (while t
      (let* ((event-times (mapcar (lambda (event)
                                    (orrient-timers--event-next event time))
                                  events))
             (next-event (seq-reduce
                          (lambda (a b)
                            (if (and a
                                     (< (orrient-event-instance-start a)
                                        (orrient-event-instance-start b)))
                                a
                              b))
                          event-times
                          nil)))
        (iter-yield next-event)
        (setq time (orrient-event-instance-end next-event))))))


;; Rendering
(defvar-local orrient-timers-time nil
  "The current time rendered in the table.")

(defvar-local orrient-timers--heading-length nil
  "Length of the longest meta name.")

(defvar-local orrient-timers--event-length 20
  "Length of the longest event name.")

(defun orrient-timers--time ()
  "Returns the current time being rendered."
  (or orrient-timers-time
      (orrient-timers--current-time)))

(defun orrient-timers--format-eta (minutes)
  "Format an ETA shown on an event of its next occurance."
  (let ((hours (/ minutes 60))
        (minutes (% minutes 60)))
    ;; 6 is select here because the longest possible time between events is 2
    ;; hours and "2h 00m" is 6 characters. So we normalize all timestamps to 6
    ;; characters.
    (format "%6s" (if (> hours 0)
	              (format "%dh %02dm" hours minutes)
                    (format "%02dm" minutes)))))

(defun orrient-timers--format-event (event-name minutes-until)
  "Format an event of its next occurance."
  (format "%s %s" (orrient-timers--format-eta minutes-until) event-name))

(defun orrient-timers--heading-length ()
  "Calculate the longest meta name length."
  (or orrient-timers--heading-length
      (setq orrient-timers--heading-length
            (let ((name-lengths (mapcar (lambda (meta)
                                          (cons meta (length (orrient-meta-name meta))))
                                        orrient-schedule)))
              (cdr (seq-reduce (lambda (left right)
                                 (if (and left
                                          (> (cdr left)
                                             (cdr right)))
                                     left
                                   right))
                               name-lengths
                               nil))))))

(defun orrient-timers--event-entry (event-instance time)
  "Generate an event entry for a tabulated list.
EVENT-INSTANCE is the cl-struct `orrient-event-instance.'

TIME is used to calculate the eta for EVENT-INSTANCE."
  (let ((event (orrient-event-instance-event event-instance))
        (minutes-until (- (orrient-event-instance-start event-instance) time)))
    (cons (orrient-timers--format-event (orrient-event-name event) minutes-until)
          `(action orrient-timers--button-event
            orrient-event-instance ,event-instance
            face ,(orrient-timers--get-countdown-face minutes-until)))))

(defun orrient-timers--entries (time)
  "Returns all entries in tabulated list at TIME."
  (mapcar
   (lambda (meta)
     (let* ((meta-name (orrient-meta-name meta))
            (meta-category (orrient-meta-category meta))
            (meta-iter (orrient-timers--meta-iter meta time)))
       (list meta-name
             (vector (cons meta-name `(action orrient-timers--button-meta
                                       orrient-meta ,meta))
                     (cons (orrient--meta-category-name meta-category) `(face ,(orrient-timers--get-category-face meta-category)
                                                                         orrient-category-id ,meta-category))
                     (orrient-timers--event-entry (iter-next meta-iter) time)
                     (orrient-timers--event-entry (iter-next meta-iter) time)
                     (orrient-timers--event-entry (iter-next meta-iter) time)))))
   orrient-schedule))

(defun orrient-timers--update (time)
  "Update the timers at TIME."
  (setq orrient-timers-time time)
  (setq tabulated-list-entries (orrient-timers--entries time))
  (tabulated-list-print t nil))

;;;###autoload
(defun orrient-timers-open (&optional interactive)
  "Open the event timers buffer."
  (interactive "p")
  (let* ((buffer (get-buffer-create orrient-timers-buffer)))
    (with-current-buffer buffer
      (orrient-timers-mode))
    (orrient--display-buffer buffer (not interactive))))

(define-derived-mode orrient-timers-mode tabulated-list-mode "GW2 Timers"
  "View Guild Wars 2 Event Timers."
  :group 'orrient-timers
  :syntax-table nil
  :abbrev-table nil
  :interactive t
  (setq-local revert-buffer-function (lambda (&rest _)
                                       (orrient-timers-now)))

  (setq tabulated-list-format (vector '("Meta" 21 t)
                                      '("Category" 21 orrient-timers--schedule-sort)
                                      `("Current" 21 ,(apply-partially #'orrient-timers--event-instance-sort 2))
                                      `("Next" 21 ,(apply-partially #'orrient-timers--event-instance-sort 3))
                                      `("Later" 21 ,(apply-partially #'orrient-timers--event-instance-sort 4))))
  (orrient-timers-now)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (tabulated-list-sort 1))

(provide 'orrient-timers)

;;; orrient-timers.el ends here
