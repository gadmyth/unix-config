;;; package --- follows.el
;;; Commentary:
;;; Code:

(defun make-follow(number)
  (interactive "nNumber of split window: ")
  (delete-other-windows)
  (dotimes (a (- number 1) [])
    (split-window-right))
  (balance-windows)
  (follow-mode))

(provide 'follows)
;;; follows.el ends here