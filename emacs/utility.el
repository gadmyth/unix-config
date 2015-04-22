(eval-when-compile (require 'cl))

;;;###autoload
(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;;;###autoload
(defun switch-proxy (enable)
  (interactive "Senable? ")
  (let ((proxy
		 (if enable
			 "127.0.0.1:8087"
		   nil)))
	(setenv "http_proxy"  proxy)
	(setenv "https_proxy" proxy)))

(setq *find-grep-dired--dir* "~")

;;;###autoload
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

;;;###autoload
(defun clean-svn (dir buffer)
  (interactive "DDelete-directory: \nbDelete candidates buffer: ")
  (mapcar (lambda (file) (delete-file (concat dir "/" file)))
		  (split-string
		   (with-current-buffer
			   (get-buffer-create buffer)
			 (buffer-substring-no-properties (point-min) (point-max))) "\n")))

;;;###autoload
(defun switch-default-dir (dir)
  (interactive "DChoose default directory:")
  (when (not (null dir))
	(setq default-directory dir)))

(defun swap-to-main-window ()
  (interactive)
  (when (>= (count-windows) 2)
	(let* ((first-window (frame-first-window))
		   (first-buffer (window-buffer first-window))
		   (current-window (get-buffer-window))
		   (current-buffer (window-buffer current-window))
		   (first-start (window-start first-window))
		   (current-start (window-start current-window)))
	  (set-window-buffer first-window current-buffer)
	  (set-window-buffer current-window first-buffer)
	  (set-window-start first-window current-start)
	  (set-window-start current-window first-start)
	  (select-window first-window))))

(defun goto-main-window ()
  (interactive)
  (select-window (frame-first-window)))

(defun goto-next-window() 
  (interactive)
  (other-window +1))

(global-set-key (kbd "C-c m") 'goto-main-window)
(global-set-key (kbd "C-c RET") 'swap-to-main-window)
(global-set-key (kbd "C-c n") 'goto-next-window)
(global-set-key (kbd "C-c l") 'helm-buffers-list)
(global-set-key (kbd "C-c f") 'ido-find-file)

(defun get-workspace (index from-end)
  (let* ((frame-num (length (frame-list)))
		 (max-index (- frame-num 1))
		(frame-index (if (not from-end)
						 index
					   (- max-index index))))
	(nth frame-index (frame-list))))

(defun goto-workspace-by-number (index)
  (if (<= index (- (length (frame-list)) 1))
	  (select-frame-set-input-focus (get-workspace index t))
	(message "No workspace found")))

(dotimes (i 10)
  (eval `(defun ,(intern (format "goto-workspace-%s" i)) ()
		   ,(format "goto workspace with number %i." i)
		   (interactive)
		   (goto-workspace-by-number ,(- i 1))))
  (global-set-key (kbd (format "C-c C-%s" i)) (intern (format "goto-workspace-%s" i))))

(setq *must-loading-files*
	  (mapcar (lambda (n) (expand-file-name n))
			  '("~/diary" "~/org/notes.org" "~/org/task.org" "~/unix-config/.emacs")))

(defun ensure-mkdir (dirname)
  (if (not (file-exists-p dirname))
	  (let ((dir (directory-file-name (file-name-directory dirname))))
		(ensure-mkdir dir)))
  (if (not (file-exists-p dirname))
	  (mkdir dirname)))

(defun load-exist-buffer (filename)
  (dolist (buffer (buffer-list))
	(with-current-buffer buffer
	  (if (string-equal buffer-file-name filename)
		  (progn
			(wcy-desktop-load-file buffer)
			(return t)))))
  nil)

;;;###autoload
(defun load-must-files ()
  (interactive)
  (mapc (lambda (filename)
		  (if (not (load-exist-buffer filename))
			  (if (file-exists-p filename)
				  (find-file-noselect filename nil nil nil)
				(progn 
				  (let ((dir (file-name-directory filename)))
					(ensure-mkdir dir))
				  (with-current-buffer (create-file-buffer filename)
					(write-file filename))))))
		*must-loading-files*))

(provide 'utility)
