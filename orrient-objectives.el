;;; orrient-objectives.el --- Objective Tracker in GW2 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'orrient)
(require 'orrient-api)
(require 'orrient-cache)

(defvar orrient-objectives-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'orrient--quit)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* 'normal map
        (kbd "g r") #'orrient-objectives-refresh
        (kbd "q") #'orrient--quit))
    map)
  "Keymap for `orrient-objectives-mode'.")

(defvar orrient-objectives-buffer-suffix "objectives"
  "Suffix used for naming `orrient-objectives' buffers.")

(defvar orrient-objectives-refresh-timer nil
  "Timer to refresh timer. Used with
  `orrient-cache--shortest-refresh-time' to update when the most
  frequently expired item on the page needs to be updated..")

(defmacro orrient-objectives--with-buffer (&rest body)
  "Like `with-current-buffer' but with an `orrient-' buffer namespace.
BODY is evaluated in an orrient buffer."
  `(orrient--with-buffer ,orrient-objectives-buffer-suffix
    ,@body))

(defvar orrient-objectives-achievements nil
  "Achievements being tracked by the objective tracker.")

(defun orrient-objectives-add-item-amount (item-id quantity)
  ;; TODO
  )

(defun orrient-objectives--render-objective (achievement-id)
  "Write the tracked objectives in the current buffer."
  (if-let ((achievement (orrient-api--request
                          orrient-achievement
                          (list achievement-id)
                          (lambda (&rest _) (orrient-objectives-refresh))))
           (achievement (car achievement)))
      (progn
        (insert (propertize (format "%s (%d)"
                                    (slot-value achievement :name)
                                    (slot-value achievement :id)) 'face 'info-title-1))
        (when-let* ((account-achievement (orrient-api--request orrient-account-achievement
                                          (list achievement-id)
                                          (lambda (&rest _) (orrient-objectives-refresh))))
                    (account-achievement (car account-achievement)))
          (insert
           (let ((current (slot-value account-achievement :current))
                 (max (slot-value account-achievement :max)))
             (format " %d%% [%d/%d]"
                     (* 100 (/ (float current) (float max)))
                     current max)))
          (when-let* ((achievement-bits (slot-value achievement :bits))
                      (account-bits (slot-value account-achievement :bits)))
            (let ((i 0))
              (dolist (bit (slot-value achievement :bits))
                (insert
                 (pcase (slot-value bit :type)
                   ("Item" (format "\n  - [%s] item | %s"
                                   (if (memq i account-bits) "x" " ")
                                   (if-let ((bit (nth i achievement-bits))
                                            (item-id (slot-value bit :id))
                                            (item (orrient-cache--get orrient-item (list item-id)))
                                            (item (car item)))
                                       (slot-value item :name)
                                     (orrient-api--request
                                       orrient-item
                                       (list item-id)
                                       (lambda (&rest _) (orrient-objectives-refresh)))
                                     (format "Loading item %d..." item-id))))
                   ("Text" (format "\ntext: %s" (slot-value bit :text)))
                   ("Skin" (format "\n  - [%s] skin | %s"
                                   (if (memq i account-bits) "x" " ")
                                   (if-let ((bit (nth i achievement-bits))
                                            (skin-id (slot-value bit :id))
                                            (skin (orrient-cache--get orrient-skin (list skin-id)))
                                            (skin (car skin)))
                                       (slot-value skin :name)
                                     (orrient-api--request
                                       orrient-skin
                                       (list skin-id)
                                       (lambda (&rest _)
                                         (orrient-objectives-refresh)))
                                     (format "Loading skin %d..." skin-id))))
                   (_ "\nError")))
                (setq i (1+ i))))))
        (insert ?\n ?\n))
    (orrient-api--request orrient-achievement (list achievement-id))
    (insert (propertize (format "Achievement id #%s Loading...\n\n" achievement-id) 'face 'info-title-1))))

(defun orrient-objectives--render-buffer ()
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (orrient-objectives-mode)
    (when orrient-objectives-refresh-timer
      (cancel-timer orrient-objectives-refresh-timer)
      (setq orrient-objectives-refresh-timer nil))
    (dolist (objective orrient-objectives-achievements)
      (orrient-objectives--render-objective objective))
    (goto-char pos))
  (setq orrient-objectives-refresh-timer
        (run-with-timer orrient-cache--shortest-refresh-time
                        nil
                        #'orrient-objectives-refresh)))

;;;###autoload
(defun orrient-objectives-track ()
  ""
  (interactive)
  (let* ((candidates (seq-map
                      (lambda (achievement)
                        (cons (slot-value achievement :name)
                              achievement))
                      (orrient-cache--get-all orrient-achievement)))
         (achievement (assoc (completing-read "Track Achievement: " candidates)
                             candidates)))
    (add-to-list 'orrient-objectives-achievements (slot-value (cdr achievement) :id))
    (orrient-objectives-open)))

(defun orrient-objectives-untrack (achievement-id)
  ""
  (interactive
   (let* ((candidates (seq-map
                       (lambda (achievement-id)
                         (cons (slot-value
                                (car (orrient-cache--get orrient-achievement (list achievement-id) t))
                                :name)
                               achievement-id))
                       orrient-objectives-achievements))
          (selection (completing-read "Untrack Achievement: " candidates)))
     (list (cdr (assoc selection candidates)))))
  (setq orrient-objectives-achievements
        (remove achievement-id orrient-objectives-achievements))
  (orrient-objectives-open))

(defun orrient-objectives-refresh ()
  (interactive)
  (orrient-objectives--with-buffer
   (orrient-objectives--render-buffer)))

;;;###autoload
(defun orrient-objectives-open (&optional interactive)
  "Open the event objectives buffer.
INTERACTIVE is set only when this command is called interactively."
  (interactive "p")
  (orrient--display-buffer
   (orrient-objectives--with-buffer
    (orrient-objectives--render-buffer))
   (not interactive)))

(define-derived-mode orrient-objectives-mode orrient-mode "GW2 Objectives"
  "Track personal objectives in Guild Wars 2."
  :group 'orrient-objectives
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

(provide 'orrient-objectives)
;;; orrient-objectives.el ends here
