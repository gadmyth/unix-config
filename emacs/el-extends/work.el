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
      (find-file WORK-FILE)
    (progn
      (new-buffer "work.org")
      (switch-to-buffer "work.org")
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


(provide 'work)
;;; work.el ends here