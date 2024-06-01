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
        (kbd "g r") #'orrient-objectives-open
        (kbd "q") #'orrient--quit))
    map)
  "Keymap for `orrient-objectives-mode'.")

(defvar orrient-objectives-buffer-suffix "objectives"
  "Suffix used for naming `orrient-objectives' buffers.")

(defmacro orrient-objectives--with-buffer (&rest body)
  "Like `with-current-buffer' but with an `orrient-' buffer namespace.
BODY is evaluated in an orrient buffer."
  `(orrient--with-buffer ,orrient-objectives-buffer-suffix
    ,@body))

(defvar orrient-objectives-achievements nil
  "Achievements being tracked by the objective tracker.")

(defun orrient-objectives-add-achievement (achievement-id)
  (add-to-list 'orrient-objectives-achievements achievement-id))

(defun orrient-objectives-add-item-amount (item-id quantity)
  ;; TODO
  )

(defun orrient-objectives--render-objective (achievement-id)
  "Write the tracked objectives in the current buffer."
  (if-let ((achievement (orrient-cache--get orrient-achievement (list achievement-id)))
           (achievement (car achievement)))
      (progn
        (insert (propertize (slot-value achievement :name) 'face 'info-title-1))
        (if-let* ((achievement-bits (slot-value achievement :bits))
                  (account-achievement (orrient-api--request orrient-account-achievement (list achievement-id)))
                  (account-achievement (car account-achievement))
                  (account-bits (slot-value account-achievement :bits)))
            (let ((i 0))
              (dolist (bit (slot-value achievement :bits))
                (insert
                 (pcase (slot-value bit :type)
                   ("Item" (format "\n  - [%s] %s"
                                   (if (memq i account-bits) "x" " ")
                                   (if-let ((bit (nth i achievement-bits))
                                            (item-id (slot-value bit :id))
                                            (item (orrient-cache--get orrient-item (list item-id)))
                                            (item (car item)))
                                       (slot-value item :name)
                                     (orrient-api--request orrient-item (list item-id) #'pg/nothing)
                                     (format "Loading item %d..." item-id))))
                   ("Text" (format "\ntext: %s" (slot-value bit :text)))
                   ("Skin" (format "\n  - [%s] %s"
                                   (if (memq i account-bits) "x" " ")
                                   (let ((bit (nth i achievement-bits)))
                                     (format "Loading skin %d..." (slot-value bit :id)))))
                   (_ "\nError")))
                (setq i (1+ i)))))
        (insert ?\n ?\n))
    (orrient-api--request orrient-achievement (list achievement-id))
    (insert (propertize (format "Achievement id #%s Loading..." achievement-id) 'face 'info-title-1))))

;;;###autoload
(defun orrient-objectives-track ()
  ""
  (interactive)
  (let* ((candidates (seq-map
                      (lambda (achievement)
                        (cons (slot-value achievement :name)
                              achievement))
                      (orrient-cache--get-all orrient-achievement)))
         (achievement (assoc (completing-read "Achievement: " candidates)
                           candidates)))
    (orrient-objectives-add-achievement (slot-value (cdr achievement) :id))))

;;;###autoload
(defun orrient-objectives-open (&optional interactive)
  "Open the event objectives buffer.
INTERACTIVE is set only when this command is called interactively."
  (interactive "p")
  (orrient--display-buffer
   (orrient-objectives--with-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (orrient-objectives-mode)
      (dolist (objective orrient-objectives-achievements)
        (orrient-objectives--render-objective objective))))
   (not interactive)))

(define-derived-mode orrient-objectives-mode orrient-mode "GW2 Objectives"
  "Track personal objectives in Guild Wars 2."
  :group 'orrient-objectives
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

(provide 'orrient-objectives)
;;; orrient-objectives.el ends here
