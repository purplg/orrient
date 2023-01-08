;;; orrient-meta.el --- Meta information for GW2 -*- lexical-binding: t; -*-
(require 'orrient-data)
(require 'orrient-timers)

(defvar orrient-meta-buffer-format "*orrient-meta: %s*")

(defun orrient-meta--buffer-name (meta)
  (format orrient-meta-buffer-format (orrient-meta-name meta)))

(defmacro orrient-meta--with-buffer (meta &rest body)
  "Like `with-current-buffer' but with `orrient-meta-buffer'.
BODY is evaluated with `orrient-meta-buffer'"
  `(when-let ((buffer (get-buffer (orrient-meta--buffer-name meta))))
    (with-current-buffer buffer
      ,@body)))

(defvar orrient-meta-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'orrient--quit)
    map)
  "Keymap for `orrient-meta-mode'.")

;;;###autoload
(defun orrient-meta-open (meta)
  "Open a GW2 meta buffer.
META is a `orrient-meta' struct that is to be rendered."
  (interactive
   (let ((completions (mapcar (lambda (meta)
                                (cons (orrient-meta-name meta) meta))
                              orrient-schedule)))
     (list (cdr (assoc (completing-read "Meta: " completions) completions)))))
  (let* ((buffer (get-buffer-create (orrient-meta--buffer-name meta))))
    (orrient-meta--with-buffer meta
                               (let ((inhibit-read-only t))
                                 (orrient-meta--render meta (orrient-timers--current-time)))
                               (orrient-meta-mode))
    (orrient--display-buffer buffer)))

(defun orrient-meta--render-event (instance time)
  (let* ((event (orrient-event-instance-event instance))
         (event-name (orrient-event-name event))
         (event-start (- (orrient-event-instance-start instance) time))
         (event-end (- (orrient-event-instance-end instance) time)))
    ;; Event name
    (insert ?\n)
    (insert ?\n)
    (insert-button event-name
                   'action (lambda (&rest _) (orrient-event-open event)))

    ;; Event start time
    (insert ?\n)
    (insert (format "%9s: " (if (< event-start 0) "started" "starts in")))
    (set-text-properties (point)
                         (progn (insert (orrient-timers--format-eta event-start))
                                (point))
                         `(face (,(orrient-timers--get-countdown-face event-start))))

    ;; Event end time
    (insert ?\n)
    (insert (format "%9s: " "ends in"))
    (set-text-properties (point)
                         (progn (insert (orrient-timers--format-eta (abs event-end)))
                                (point))
                         `(face (,(orrient-timers--get-countdown-face event-end))))))

(defun orrient-meta--render (meta time)
  (erase-buffer)
  (insert (propertize (orrient-meta-name meta) 'face 'info-title-1))
  (let ((iter (orrient-timers--meta-iter meta time)))
    ; fill the window with events
    (cl-loop while (< (line-number-at-pos (point))
                      (- (window-body-height) 6)) ; 6 because loop is looking ahead and the title takes 2 lines and an event is 4 lines.
             do (orrient-meta--render-event (iter-next iter)
                                            time)))
  (goto-char (point-min)))

(define-derived-mode orrient-meta-mode orrient-mode "GW2 Meta"
  "View Guild Wars 2 Meta."
  :group 'orrient-meta
  :syntax-table nil
  :abbrev-table nil
  :interactive t
  (when (featurep 'evil)
    (evil-local-set-key 'normal (kbd "q") 'orrient--quit)))

(provide 'orrient-meta)
;;; orrient-meta.el ends here
