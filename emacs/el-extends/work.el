;;; package --- work.el
;;; Commentary:
;;; Code:

(require 'org)

(defvar WORK-FILE)
(setq WORK-FILE (expand-file-name "~/work.org"))

(defvar WORK-FILE-HTML)
(setq WORK-FILE-HTML (expand-file-name "~/work.html"))

(defun visit-work-file ()
  "."
  (interactive)
  (if (file-exists-p WORK-FILE)
      (find-file-other-window WORK-FILE)
    (progn
      (new-buffer "work.org")
      (switch-to-buffer-other-window "work.org")
      (write-file WORK-FILE))))

(defun visit-work-html()
  "."
  (interactive)
  (if (file-exists-p WORK-FILE-HTML)
      (shell-command-to-string (format "open %s" WORK-FILE-HTML))))

(defun update-work-html()
  "."
  (interactive)
  (org-html-export-to-html))

(defvar *org-cap-temp*)
(defun org-capture-current-line (description)
  "DESCRIPTION: ."
  (interactive "sSet the line description here: ")
  (re-search-backward "^" nil t)
  (re-search-forward "^ *\\(.*?\\)$" nil t 1)
  (let* ((line (match-string-no-properties 1))
         (encoded-line (url-encode-url line))
         (formatted-line (format "[[file:%s::%s][%s]]" (buffer-file-name) encoded-line description)))
    (setq *org-cap-temp* formatted-line)
    (visit-work-file)))

(defun org-capture-insert-temp ()
  "."
  (interactive)
  (if (not (null *org-cap-temp*))
      (progn
        (insert-string *org-cap-temp*)
        (setq *org-cap-temp* nil))))

(defun org-capture-dired-file (description)
  "DESCRIPTION: ."
  (interactive "sSet the dir description here: ")
  (let* ((file (dired-get-file-for-visit))
        (formatted-line (format "[[file:%s][%s]]" file description)))
    (setq *org-cap-temp* formatted-line)
    ;(visit-work-file)
    ))



(defun org-parse-file-link ()
  "."
  (interactive)
  (save-excursion
    (re-search-backward "^" nil t)
    (if (re-search-forward "^.*?file:\\([^]:]*\\).*$" nil t 1)
        (let* ((org-link-file (match-string-no-properties 1))
               (org-link-dir (if (file-directory-p org-link-file)
                                 org-link-file
                               (file-name-directory org-link-file))))
          (cons org-link-file org-link-dir))
      (cons nil nil))))

(defun org-open-dir ()
  "."
  (interactive)
  (let* ((parse-result (org-parse-file-link))
         (file (car parse-result))
         (dir (cdr parse-result)))
    (when (and (not (null file))
               (not (null dir)))
      (dired-other-window dir)
      (dired-goto-file file)
      (message "Selected file is: %s" file))))

(defun org-show-link ()
  "."
  (interactive)
  (let* ((parse-result (org-parse-file-link))
         (file (car parse-result))
         (dir (cdr parse-result)))
    (if (and (not (null file)) (not (null dir)))
      (message "The file of link is: %s" file))))

(defun org-copy-link ()
  "."
  (interactive)
  (save-excursion
    (re-search-backward "^" nil t)
    (if (re-search-forward "^.*?\\(https?:[^]]*\\).*$" nil t 1)
        (let* ((org-link (match-string-no-properties 1)))
          (kill-new org-link)
          (message org-link)
          org-link))))

(global-set-key (kbd "<f7>") 'org-capture-current-line)
(global-set-key (kbd "<f8>") 'org-capture-insert-temp)
(global-set-key (kbd "C-c d") 'org-open-dir)
(global-set-key (kbd "C-c P") 'org-show-link)

(require 'dired)
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "<f7>") 'org-capture-dired-file)))

(defun open-current-buffer()
  "."
  (interactive)
  (let* ((buffer (current-buffer))
         (file-name (buffer-file-name buffer)))
    (if (and (not (null file-name)) (file-exists-p file-name))
        (progn
          (shell-command-to-string (format "open %s" file-name))
          (message "Open succeed: %s" file-name))
      (message "File doesn't not exists!"))))

(provide 'work)
;;; work.el ends here