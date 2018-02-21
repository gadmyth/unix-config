;;; package --- el-server-extend.el
;;; Commentary:
;;; Code:

(require 'el-server)

(defmacro org-dir-handler-maker (dir)
  "DIR: ."
  "This body must be a macro, because elnode-docroot-for is a macro, the dir should be evaluated."
  `(lambda (httpcon)
     (elnode-docroot-for ,dir
       with org-file
       on httpcon
       do
       (message (format "\nrequest: http://%s%s\n" (elnode-http-header httpcon "Host") (elnode-http-pathinfo httpcon)))
       (let ((remote-host (elnode-remote-host httpcon))
             (remote-port (elnode-remote-port httpcon))
             (local-host (elnode-local-host httpcon))
             (local-port (elnode-local-port httpcon))
             (link-org-as-html (elnode-http-param httpcon "link-org-as-html"))
             (force-refresh (elnode-http-param httpcon "force-refresh"))
             (exclude-inner-res (elnode-http-param httpcon "exclude-inner-res")))
         (message (format "remote host: %s, remote port: %s" remote-host remote-port))
         (message (format "local host: %s, local port: %s" local-host local-port))
         (message (format "org-file: %S, is directory: %S" org-file (file-directory-p org-file)))
         (if (and (equal "0.0.0.0" local-host)
                  (or (equal "0.0.0.0" remote-host)
                      (equal "127.0.0.1" remote-host)
                      (equal "localhost" remote-host)))
             (let ((new-url (format "http://%s:%s%s" (current-ip) local-port (elnode-http-pathinfo httpcon))))
               (message "redirect to new-url: %s" new-url)
               (elnode-send-redirect httpcon new-url))
           (if (not (string-suffix-p "org" org-file))
               (if (file-directory-p org-file)
                   (elnode-send-status httpcon 403 "Permission Denied.")
                 (elnode--webserver-handler-proc httpcon ,dir elnode-webserver-extra-mimetypes))
             (let* ((org-file-mtime (elnode-file-modified-time org-file))
                    (html-file (concat (string-remove-suffix "org" org-file) "html"))
                    (html-file-mtime (elnode-file-modified-time html-file)))
               (if (and org-file-mtime
                        html-file-mtime
                        (time-less-p org-file-mtime html-file-mtime)
                        (not force-refresh))
                   (let* ((path (elnode-http-pathinfo httpcon))
                          (new-path (concat (string-remove-suffix "org" path) "html"))
                          (new-url (format "http://%s:%s%s" remote-host local-port new-path)))
                     (message "rediect org file to html file: %s" new-url)
                     (elnode-send-redirect httpcon new-url))
                 (with-current-buffer (find-file-noselect org-file)
                   (progn
                     (when (string-equal mode-name "not loaded yet")
                       (revert-buffer nil t))
                     (progn
                       (setq org-export-show-temporary-export-buffer nil)
                       (setq-local org-html-link-org-files-as-html link-org-as-html)
                       (setq org-html-head (if exclude-inner-res "" org-html-head-default))
                       (let ((exported-buffer (org-html-export-as-html)))
                         (message "exported-buffer: %s" exported-buffer)
                         (message "org-html-head: %S" (length org-html-head))
                         (setq org-export-show-temporary-export-buffer t)
                         (with-current-buffer exported-buffer
                           (let ((org-html (buffer-substring-no-properties (point-min) (point-max))))
                             (write-file html-file)
                             (elnode-send-html httpcon org-html)))))))))))))))

(defmacro org-dir-compiled-handler-maker (dir)
  "DIR: ."
  (byte-compile `(org-dir-handler-maker ,dir)))


(defun elnode-make-org-webserver (docroot port &optional host)
  "DOCROOT: , PORT: , HOST: . Make a webserver parsing org file to html format."
  (interactive
   (let ((docroot (read-directory-name "Docroot: " nil nil t))
         (port (read-from-minibuffer "Port: "))
         (host (if current-prefix-arg
                   (read-from-minibuffer "Host: ")
                 elnode-init-host)))
     (list docroot port host)))
  (let ((webserver-proc (eval `(org-dir-handler-maker ,docroot))))
    (elnode-start
     webserver-proc
     :port (string-to-number (format "%s" port))
     :host host)
    webserver-proc))

(my-elnode-add-handlers
 `(("^/orgs/\\(.*\\)$" . ,(org-dir-compiled-handler-maker "~/org/doc/"))
   ("^/homo/\\(.*\\.html\\)$" . ,(elnode-webserver-handler-maker "~/org/homo_public_html/"))
   ("^/homo/\\(.*\\.*\\)$" . ,(org-dir-compiled-handler-maker "~/org/homogenius/"))))

(provide 'el-server-extend)
;;; el-server-extend.el ends here