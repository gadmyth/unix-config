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

(provide 'codec)
;;; codec.el ends here