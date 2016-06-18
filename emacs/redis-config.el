;;; package --- redis-config.el
;;; Commentary:
;;; Code:

(require 'eredis)

(defun r-connect-local ()               ;
  "."
  (interactive)
  (eredis-connect "localhost" 6379))

(defun r-get (key)
  "KEY."
  (interactive "sget redis value for key: ")
  (let ((value (eredis-get key)))
    (message value)
    value))

(defun r-set (key value)
  "KEY: VALUE:."
  (interactive"sKey: \nsValue: \n")
  (eredis-set key value))

(provide 'redis-config)
;;; redis-config.el ends here
