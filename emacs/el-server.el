;;; package --- el-server.el
;;; Commentary:
;;; Code:

(require 'elnode)

(defun current-ip ()
  "."
  (interactive)
  (let* ((ruby-command-string "print Socket.ip_address_list.find { |ai| ai.ipv4? && !ai.ipv4_loopback? }.ip_address")
         (shell-command-string (format "ruby -r socket -e \"%s\"" ruby-command-string))
         (result (shell-command-to-string shell-command-string)))
    (message result)))

(defun elnode--ip-host (ip-addr)
  "IP-ADDR: ."
  (destructuring-bind (a b c d port)
      (mapcar 'identity ip-addr)
    (format "%s.%s.%s.%s" a b c d)))

(defun elnode--ip-port (ip-addr)
  "IP-ADDR: ."
  (destructuring-bind (a b c d port)
      (mapcar 'identity ip-addr)
    port))

(defun elnode-remote-host (httpcon)
  "HTTPCON: ."
  (elnode--ip-host
   (plist-get
    (process-contact httpcon t)
    :remote)))

(defun elnode-remote-port (httpcon)
  "HTTPCON: ."
  (elnode--ip-port
   (plist-get
    (process-contact httpcon t)
    :remote)))

(defun elnode-local-host (httpcon)
  "HTTPCON: ."
  (elnode--ip-host
   (plist-get
    (process-contact (process-get httpcon :server) t)
    :local)))

(defun elnode-local-port (httpcon)
  "HTTPCON: ."
  (elnode--ip-port
   (plist-get
    (process-contact (process-get httpcon :server) t)
    :local)))

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
          (when (string-equal mode-name "not loaded yet")
            (revert-buffer nil t))
          (message "remote: %s" (elnode-get-remote-ipaddr httpcon))
          (message "local: %s" (elnode-server-info httpcon))
          (let ((remote-host (elnode-remote-host httpcon))
                (remote-port (elnode-remote-port httpcon))
                (local-host (elnode-local-host httpcon))
                (local-port (elnode-local-port httpcon)))
            (message (format "remote host: %s, remote port: %s" remote-host remote-port))
            (message (format "local host: %s, local port: %s" local-host local-port))
            (if (or (equal "0.0.0.0" remote-host)
                    (equal "127.0.0.1" remote-host)
                    (equal "localhost" remote-host))
                (let ((new-url (format "http://%s:%s%s" (current-ip) local-port (elnode-http-pathinfo httpcon))))
                  (message "new server: %s" new-url)
                  (elnode-send-redirect httpcon new-url))
              (progn
                (setq org-export-show-temporary-export-buffer nil)
                (let ((exported-buffer (org-html-export-as-html)))
                  (setq org-export-show-temporary-export-buffer t)
                  (with-current-buffer exported-buffer
                    (let ((org-html (buffer-substring-no-properties (point-min) (point-max))))
                      (elnode-send-html httpcon org-html)))))))))))))

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