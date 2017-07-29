;;; package --- el-server.el
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl))

(require 'elnode)

(defvar server-dir "~/server")
(defconst my-elnode-editor-webserver-handler (elnode-webserver-handler-maker server-dir))

(defun ex-handler (httpcon)
  "HTTPCON: ."
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (elnode-http-return httpcon
					  "<html><body><h1>Hello World</h1></body></html>"))

;;(elnode-start 'ex-handler :port 8003)
;;(elnode-start (elnode-webserver-handler-maker server-dir) :port 8000)

(defvar my-elnode-editor-buffer (get-buffer-create "*talk*"))
(defun my-elnode-editor-handler (httpcon)
  "HTTPCON: ."
  (elnode-http-start httpcon 200 '("Content-Type" . "text/plain"))
  (elnode-http-return
   httpcon
   (with-current-buffer my-elnode-editor-buffer
     (buffer-substring-no-properties (point-min) (point-max)))))
;;(elnode-start 'my-elnode-editor-handler :port 8001)

(defvar org (get-buffer-create "1.2.2.org"))
(defun my-org-handler (httpcon)
  "HTTPCON: ."
  (elnode-http-start httpcon 200 '("Content-Type" . "text/plain"))
  (elnode-http-return
   httpcon
   (with-current-buffer org
     (buffer-substring-no-properties (point-min) (point-max)))))

(defconst my-org-dir "~/org/doc/")
(defun my-org-dir-handler (httpcon)
  "HTTPCON: ."
  (elnode-docroot-for my-org-dir
	with org-file
	on httpcon
    do
    (with-current-buffer (find-file-noselect org-file)
         (progn
           (setq org-export-show-temporary-export-buffer nil)
           (let ((exported-buffer (org-html-export-as-html)))
             (setq org-export-show-temporary-export-buffer t)
             (with-current-buffer exported-buffer
               (let ((org-html (buffer-substring-no-properties (point-min) (point-max))))
                 (elnode-send-html httpcon org-html))))))))

(defconst my-org-image-dir "~/org/doc/images/")
(setq org-dir-image-handler (elnode-webserver-handler-maker my-org-image-dir))
(defun my-org-dir-image-handler (httpcon)
  "HTTPCON: ."
  (funcall org-dir-image-handler httpcon))

(defun my-elnode-editor-update-handler (httpcon)
  "HTTPCON: ."
  (let ((change-text (elnode-http-param httpcon "change")))
    (with-current-buffer my-elnode-editor-buffer
      (goto-char (point-max))
      (when (stringp change-text)
        (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
        (insert ": ")
        (insert change-text)
        (insert "\n"))))
  (elnode-http-start httpcon 302 '("Location" . "/text/"))
  (elnode-http-return httpcon))

(setq my-elnode-editor-urls
  '(("^/text/$" . my-elnode-editor-handler)
    ("^/update/\\(.*\\)" . my-elnode-editor-update-handler)
	;("^/org/$" . my-org-handler)
    ("^/orgs/images/\\(.*\\)" . my-org-dir-image-handler)
	("^/orgs/\\(.*\\.org\\)" . my-org-dir-handler)
	("/$" . my-elnode-editor-webserver-handler)))

(defun my-elnode-editor-dispatcher-handler (httpcon)
  "HTTPCON: ."
  (elnode-dispatcher httpcon my-elnode-editor-urls))

(add-to-list 'elnode-hostpath-default-table '("/server/" . ex-handler))


(provide 'el-server)
;;; el-server.el ends here