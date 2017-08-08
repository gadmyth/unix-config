;;; package --- el-server.el
;;; Commentary:
;;; Code:

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

(defmacro org-dir-handler-maker (dir)
  "DIR: ."
  "This body must be a macro, because elnode-docroot-for is a macro, the dir should be evaluated."
  (byte-compile
  `(lambda (httpcon)
    (elnode-docroot-for ,dir
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
                (elnode-send-html httpcon org-html))))))))))

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
  `(("^/orgs/\\(.*\\.org\\)$" . ,(org-dir-handler-maker "~/org/doc/"))
    ("^/orgs/\\(.*\\.\\(png\\|gif\\|mp4\\|jpeg\\|jpg\\|ico\\)\\)$" . ,(elnode-webserver-handler-maker "~/org/doc/"))
    ("^/homo/\\(.*\\.org\\)$" . ,(org-dir-handler-maker "~/org/homogenius/"))
    ("^/homo/\\(.*\\.\\(png\\|gif\\|mp4\\|jpeg\\|jpg\\|ico\\)\\)$" . ,(elnode-webserver-handler-maker "~/org/homogenius/"))
    ("^/homo/\\(.*\\.html\\)$" . ,(elnode-webserver-handler-maker "~/org/homo_public_html/"))
    ("^/$" . my-elnode-editor-webserver-handler)
    ("^/text/$" . my-elnode-editor-handler)
    ("^/update/\\(.*\\)$" . my-elnode-editor-update-handler)))

(defun my-elnode-editor-dispatcher-handler (httpcon)
  "HTTPCON: ."
  (elnode-dispatcher httpcon my-elnode-editor-urls))

(add-to-list 'elnode-hostpath-default-table '("/server/" . ex-handler))


(provide 'el-server)
;;; el-server.el ends here