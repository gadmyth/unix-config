(eval-when-compile (require 'cl))

(defun switch-proxy (enable)
  (interactive "Senable? ")
  (let ((proxy
		 (if enable
			 "127.0.0.1:8087"
		   nil)))
	(setenv "http_proxy"  proxy)
	(setenv "https_proxy" proxy)))

(setq *find-grep-dired--dir* "~")
(defun find2-grep-dired (dir regexp)
  (interactive (list (read-directory-name "What directory? " 
										  *find-grep-dired--dir*)
					 (read-string "What to search? " (car kill-ring))))
  (setq *find-grep-dired--dir* dir)
  (isearch-update-ring regexp t)
  (find-dired dir
			  (concat "-not \\( -name .svn -prune \\) " "-type f -exec " grep-program " " find-grep-options " -e "
					  (shell-quote-argument regexp)
					  " "
					  (shell-quote-argument "{}")
					  " "
					  ;; Doesn't work with "+".
					  (shell-quote-argument ";"))))

(defun clean-svn (dir buffer)
  (interactive "DDelete-directory: \nbDelete candidates buffer: ")
  (mapcar (lambda (file) (delete-file (concat dir "/" file)))
		  (split-string
		   (with-current-buffer
			   (get-buffer-create buffer)
			 (buffer-substring-no-properties (point-min) (point-max))) "\n")))

(defun switch-default-dir (directory)
  (interactive "DSwitch default directory:")
  (when (and (not (null directory)) (not (equal "" directory)))
	(setq default-directory directory)))
  

(provide 'utility)
