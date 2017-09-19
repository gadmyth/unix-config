;;; package --- el-server.el
;;; Commentary:
;;; Code:

(require 'elnode)
(require 'mimes)

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

(defvar my-default-elnode-url-mapping-table '())

(defun my-elnode-add-handlers (handlers)
  "HANDLERS: ."
  (dolist (handler handlers)
    (add-to-list 'my-default-elnode-url-mapping-table handler)))

(defun my-default-elnode-dispatcher-handler (httpcon)
  "HTTPCON: ."
  (elnode-dispatcher httpcon my-default-elnode-url-mapping-table))

(elnode-start 'my-default-elnode-dispatcher-handler :host "localhost" :port 8000)

(provide 'el-server)
;;; el-server.el ends here