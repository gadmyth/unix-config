;;; package --- dates.el
;;; Commentary:
;;; Code:

(defun current-timestamp ()
  "."
  (interactive)
  (let ((ct (shell-command-to-string "gdate +%s")))
    (message ct)
    ct))

(defun string-to-timestamp (date-str)
  "DATE-STR: ."
  (interactive "sInput the date string: ")
  (let ((ct (shell-command-to-string (format "gdate -d '%s' +%%s" date-str))))
    (message ct)
    ct))

(defun timestamp-to-string (timestamp)
  "TIMESTAMP: ."
  (interactive "sInput the timestamp: ")
  (let ((date-str (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))
    (message date-str)
    date-str))


(provide 'dates)
;;; dates.el ends here