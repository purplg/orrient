;;; orrient-meta.el --- Meta information for GW2 -*- lexical-binding: t; -*-
(require 'cl-lib)

(require 'orrient-schedule)

(defvar orrient-meta--buffer-suffix-format "meta: %s")

(defun orrient-meta--buffer-name (meta)
  "Return the name of buffer when displaying META."
  (format orrient-meta--buffer-suffix-format (orrient-meta-name meta)))

(defmacro orrient-meta--with-buffer (meta &rest body)
  "Like `with-current-buffer' but with `orrient-meta-buffer'.
BODY is evaluated with `orrient-meta-buffer'"
  `(orrient--with-buffer (orrient-meta--buffer-name ,meta)
                        ,@body))

(defvar orrient-meta-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'orrient--quit)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* 'normal map
        (kbd "q") #'orrient--quit))
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
  (orrient--display-buffer
   (orrient-meta--with-buffer meta
                              (let ((inhibit-read-only t))
                                (orrient-meta--render meta (orrient-schedule--current-time)))
                              (orrient-meta-mode))))

(defun orrient-meta--render-event (instance time)
  "Render an event in current buffer.

INSTANCE is an `orrient-event-instance'.

TIME is the amount of minutes offset from UTC 0."
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
                         (progn (insert (orrient-schedule--format-eta event-start t))
                                (point))
                         `(face (,(orrient-schedule--get-countdown-face event-start))))

    ;; Event end time
    (insert ?\n)
    (insert (format "%9s: " "ends in"))
    (set-text-properties (point)
                         (progn (insert (orrient-schedule--format-eta event-end t))
                                (point))
                         `(face (,(orrient-schedule--get-countdown-face event-end))))))

(defun orrient-meta--render (meta time)
  "Render a meta in current buffer.

META is an `orrient-meta' to be rendered.

TIME is the amount of minutes offset from UTC 0."
  (erase-buffer)
  (insert (propertize (orrient-meta-name meta) 'face 'outline-1))
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
  :interactive t)

(provide 'orrient-meta)
;;; orrient-meta.el ends here
