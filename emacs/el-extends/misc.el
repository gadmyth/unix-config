;;; package --- misc.el
;;; Commentary:
;;; Code:

(defun int-to-binary-string (i)
  "Convert an integer I into it's binary representation in string format."
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(provide 'misc)
;;; misc.el ends here