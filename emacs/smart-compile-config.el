;;; package --- smart-compile-config.el
;;; Commentary:
;;; Code:

(require 'smart-compile)
(add-to-list 'smart-compile-alist '(objc-mode . "clang -fobjc-arc -framework Foundation -w %f && ./a.out"))

(defun objc-rewrite-cpp ()
  "."
  (interactive)
  (with-current-buffer (current-buffer)
    (let* ((command (format "clang -fobjc-arc -framework Foundation -rewrite-objc -w %s" buffer-file-name))
           (result (shell-command-to-string command)))
      (message command)
      (message result))))

(provide 'smart-compile-config)
;;; smart-compile-config.el ends here