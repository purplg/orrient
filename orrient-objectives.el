;;; orrient-objectives.el --- Objective Tracker in GW2 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'orrient)
(require 'orrient-api)

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
  (if-let ((achievement (orrient-cache--get-achievement achievement-id)))
      (progn
        (insert (propertize (orrient-api-achievement-name achievement) 'face 'info-title-1))
        (dolist (bit (orrient-api-achievement-bits achievement))
          (insert
           (pcase (orrient-api-achievement-bit-type bit)
             (item (format "\nitem: %d" (orrient-api-achievement-bit-id bit)))
             (text (format "\ntext: %s" (orrient-api-achievement-bit-text bit))))))
        
        (insert ?\n ?\n))
    (insert (format "Achievement id #%s Loading..." achievement-id))
    (orrient-api--achievement achievement-id
                              (lambda (&rest _)
                                (orrient-objectives-open)))))

;;;###autoload
(defun orrient-objectives-track ()
  ""
  (interactive)
  (let* ((candidates (seq-map
                      (lambda (achievement)
                        (cons (orrient-api-achievement-name achievement)
                              achievement))
                      (orrient-cache--get-achievements)))
         (achievement (assoc (completing-read "Achievement: " candidates)
                           candidates)))
    (orrient-objectives-add-achievement (orrient-api-achievement-id (cdr achievement)))))

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
