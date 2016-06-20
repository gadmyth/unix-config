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

(defun org-capture-dired-file ()
  "."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
        (formatted-line (format "[[file:%s][%s]]" file file)))
    (setq *org-cap-temp* formatted-line)
    (visit-work-file)))

(global-set-key (kbd "<f7>") 'org-capture-current-line)
(global-set-key (kbd "<f8>") 'org-capture-insert-temp)

(require 'dired)
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "<f7>") 'org-capture-dired-file)))

(provide 'work)
;;; work.el ends here