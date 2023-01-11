;;; orrient.el --- Tracks goals in GW2 -*- lexical-binding: t; -*-

;; Homepage: https://github.com/purplg/orrient
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Create and organize goals for yourself in Guild Wars 2

;; --------------------
;; Configuration

;;; Code:
(require 'orrient-api)

(declare-function evil-define-key* "evil-core")

(defcustom orrient-buffer-prefix "orrient-"
  "Prefix for all orrient buffers."
  :group 'orrient
  :type 'string)

(defvar orrient--buffer-list '()
  "List of orrient buffers opened.")

(defun orrient--buffer (name)
  (format "*%s%s*" orrient-buffer-prefix name))

(defun orrient--buffer-p (buffer)
  (with-current-buffer buffer
    (or (derived-mode-p 'orrient-mode)
        (derived-mode-p 'orrient-timers-mode))))

(defun orrient--display-buffer (buffer &optional no-hist)
  (display-buffer buffer (when (and (orrient--buffer-p (current-buffer))
                                    (orrient--buffer-p buffer))
                           '(display-buffer-same-window)))
  (unless no-hist
    (push buffer orrient--buffer-list)))

(defmacro orrient--with-buffer (name &rest body)
  "Like `with-current-buffer' but with a buffer named NAMED.
BODY is evaluated with buffer."
  `(when-let ((buffer (get-buffer-create (orrient--buffer ,name))))
     (with-current-buffer buffer
       ,@body)
     buffer))

(defun orrient--quit ()
  (interactive)
  (let ((current-buffer (pop orrient--buffer-list))
        (prev-buffer (car orrient--buffer-list)))
    (if (buffer-live-p prev-buffer)
        (progn
          (orrient--display-buffer prev-buffer t)
          (kill-buffer current-buffer))
      (kill-buffer-and-window)))
  (setq orrient--buffer-list
        (seq-filter #'buffer-live-p orrient--buffer-list)))

(define-derived-mode orrient-mode special-mode "GW2 Info"
  "View Guild Wars 2 Information."
  :group 'orrient
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

(provide 'orrient)
;;; orrient.el ends here
