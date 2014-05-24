(eval-when-compile (require 'cl))

(setq def-dir (expand-file-name "~/emacs-workspace"))
(setq *mac-scale-amount* 2
      *linux-scale-amount* 2)

(setq *load-slime* nil
	  *slime-path* "~/emacs/slime"
	  *lisp-bin-path* "/usr/local/bin/sbcl")

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
			  (concat "-type f -exec " grep-program " " find-grep-options " -e "
					  (shell-quote-argument regexp)
					  " "
					  (shell-quote-argument "{}")
					  " "
					  ;; Doesn't work with "+".
					  (shell-quote-argument ";"))))

(provide 'workspace)
