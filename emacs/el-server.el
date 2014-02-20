(eval-when-compile (require 'cl))

(require 'elnode)

(defvar server-dir "~/server")
(defconst my-elnode-editor-webserver-handler (elnode-webserver-handler-maker server-dir))

(defun ex-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (elnode-http-return httpcon
					  "<html><body><h1>Hello World</h1></body></html>"))

;;(elnode-start 'ex-handler :port 8003)
;;(elnode-start (elnode-webserver-handler-maker server-dir) :port 8000)

(defvar my-elnode-editor-buffer (get-buffer-create "*talk*"))
(defun my-elnode-editor-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-Type" . "text/plain"))
  (elnode-http-return 
   httpcon 
   (with-current-buffer my-elnode-editor-buffer
     (buffer-substring-no-properties (point-min) (point-max)))))
;;(elnode-start 'my-elnode-editor-handler :port 8001)

(defun my-elnode-editor-update-handler (httpcon)
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

(defconst my-elnode-editor-urls
  '(("^/text/$" . my-elnode-editor-handler)
    ("^/update/.*$" . my-elnode-editor-update-handler)
	("/$" . my-elnode-editor-webserver-handler)))

(defun my-elnode-editor-dispatcher-handler (httpcon)
  (elnode-dispatcher httpcon my-elnode-editor-urls))

(add-to-list 'elnode-hostpath-default-table '("/server/" . ex-handler))


(provide 'el-server)
