;;; orrient.el --- Tracks goals in GW2 -*- lexical-binding: t; -*-

;; Homepage: https://github.com/purplg/orrient
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Create and organize goals for yourself in Guild Wars 2

;; --------------------
;; Configuration

;;; Code:
(defcustom orrient-buffer-prefix "orrient-"
  "Prefix for all orrient buffers."
  :group 'orrient
  :type 'string)

(defun orrient--buffer (name)
  (format "*%s%s*" orrient-buffer-prefix name))

(defun orrient--buffer-p (buffer-or-name)
  (buffer-match-p "^\\*orrient-.*$" buffer-or-name))

(defun orrient--display-buffer (buffer-or-name)
  (if (orrient--buffer-p (current-buffer))
      (display-buffer buffer-or-name '(display-buffer-same-window))
    (pop-to-buffer buffer-or-name)))

(provide 'orrient)

;;; orrient.el ends here
