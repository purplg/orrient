;;; orrient.el --- Tracks goals in GW2 -*- lexical-binding: t; -*-

;; Homepage: https://github.com/purplg/orrient
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Create and organize goals for yourself in Guild Wars 2

;; --------------------
;; Configuration

;;; Code:
(require 'cl-lib)

(defvar orrient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g r") 'orrient-refresh)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    map)
  "Keymap for `orrient-mode'.")

(defcustom orrient-buffer-name "*orrient*"
  "The name of the orrient buffer."
  :group 'orrient
  :type 'string)

(defvar orrient-goals `(,(make-orrient-goal :text "Goal 1" :state "not done")
                         ,(make-orrient-goal :text "Goal 2" :state "not done")
                         ,(make-orrient-goal :text "Goal 3" :state "50%")
                         ,(make-orrient-goal :text "Goal 4" :state "done")
                         ,(make-orrient-goal :text "Goal 5" :state "done"))
  "The list of goals.")

(cl-defstruct orrient-goal text (state t))

(cl-defstruct (orrient-goal-repeat
               (:include orrient-goal)))

(cl-defstruct (orrient-goal-daily
               (:include orrient-goal-repeat)))


;; Widgets
(defun orrient--widget-create (widget)
  (widget-default-create widget))

(defun orrient--widget-value-create (widget)
  (message "goal: %s" (widget-get widget :goal))
  (message "goal-state: %s" (orrient-goal-state (widget-get widget :goal)))
  (if-let ((state (orrient-goal-state (widget-get widget :goal))))
      (insert (format "%s" state))
    (insert "none")))

(define-widget 'orrient-widget-goal 'item
  ""
  :create #'orrient--widget-create
  :goal nil
  :format "%t %[%v%]\n"
  :value-create #'orrient--widget-value-create
  :value-get 'orrient--widget-value-get)


;; Buffer
(defun orrient--render ()
  (dolist (goal orrient-goals)
    (widget-create 'orrient-widget-goal
     :tag (orrient-goal-text goal)
     :goal goal)))

;;;###autoload
(defun orrient-refresh ()
  "Rerender the orrient buffer."
  (interactive)
  (with-current-buffer (get-buffer-create orrient-buffer-name)
    (let ((inhibit-read-only t)
          (prev-line (line-number-at-pos)))
      (erase-buffer)
      (orrient--render)
      (goto-char (point-min))
      (forward-line (1- prev-line))
      (orrient-mode))))

;;;###autoload
(defun orrient ()
  "Open the orrient buffer."
  (interactive)
  (orrient-refresh)
  (when-let ((buffer (get-buffer-create orrient-buffer-name)))
    (if-let ((window (get-buffer-window buffer)))
        (select-window window)
      (switch-to-buffer buffer))))

(define-derived-mode orrient-mode special-mode "GW2 Event Timers"
  "Dashboard for Home Assistant."
  :group 'orrient
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

(provide 'orrient)

;;; orrient.el ends here
