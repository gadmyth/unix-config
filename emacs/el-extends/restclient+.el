;;; package --- restclient+.el
;;; Commentary:
;;; Code:

(require-if-installed
 'restclient
 (defun restclient-find-vars-before-point ()
   (let ((vars nil)
         (bound (point)))
     (save-excursion
       (goto-char (point-min))
       (while (search-forward-regexp restclient-var-regexp bound t)
         (let* ((name (match-string-no-properties 1))
                (ename (concat "rc--" (substring name 1)))
                (should-eval (> (length (match-string 2)) 0))
                (value (or (restclient-chop (match-string-no-properties 4)) (match-string-no-properties 3)))
                (value (if should-eval (restclient-eval-var value) value)))
           (setq vars (cons (cons name value) vars))
           (setq value (restclient-replace-all-in-string vars value))
           (let ((symbol (intern ename)))
             (set symbol value))))
       vars))))

(provide 'restclient+)
;;; restclient+.el ends here