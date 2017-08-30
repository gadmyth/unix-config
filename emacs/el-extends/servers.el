;;; package --- servers.el
;;; Commentary:
;;; Code:

(require 'el-server)

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
          (message "remote: %s" (elnode-remote-ipaddr httpcon))
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

(setq my-default-elnode-url-mapping-table
  `(("^/orgs/\\(.*\\.org\\)$" . ,(org-dir-handler-maker "~/org/doc/"))
    ("^/orgs/\\(.*\\.\\(png\\|gif\\|mp4\\|jpeg\\|jpg\\|ico\\)\\)$" . ,(elnode-webserver-handler-maker "~/org/doc/"))
    ("^/homo/\\(.*\\.org\\)$" . ,(org-dir-handler-maker "~/org/homogenius/"))
    ("^/homo/\\(.*\\.\\(png\\|gif\\|mp4\\|jpeg\\|jpg\\|ico\\)\\)$" . ,(elnode-webserver-handler-maker "~/org/homogenius/"))
    ("^/homo/\\(.*\\.html\\)$" . ,(elnode-webserver-handler-maker "~/org/homo_public_html/"))))

(provide 'servers)
;;; servers.el ends here