;;; package --- codec.el
;;; Commentary:
;;; Code:

(defun url-encode-region ()
  "."
  (interactive)
  (let ((url-str (delete-and-extract-region (region-beginning) (region-end))))
    (insert (url-hexify-string url-str))))

(defun url-decode-region ()
  "."
  (interactive)
  (let ((url-str (delete-and-extract-region (region-beginning) (region-end))))
    (insert (url-unhex-string url-str))))

(defun md5-encode-region ()
  "."
  (interactive)
  (let ((str (delete-and-extract-region (region-beginning) (region-end))))
    (insert (md5 str))))

(provide 'codec)
;;; codec.el ends here