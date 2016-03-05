(defun new-buffer (name)
  (interactive (list (read-string "Create buffer(*scratch*): " nil nil "*scratch*")))
  (let ((buffer (generate-new-buffer name)))
    (switch-to-buffer buffer)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defun switch-buffer (name)
  (interactive (list (read-string "Switch buffer(*scratch*): " nil nil "*scratch*")))
  (let ((buffer (get-buffer-create name)))
    (switch-to-buffer buffer)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(global-set-key (kbd "<f8>") 'new-buffer)
(global-set-key (kbd "<f9>") 'switch-buffer)

(defun switch-to-scratch-buffer ())

(provide 'files-config)