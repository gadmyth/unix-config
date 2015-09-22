;;; F zone
(setq *MAIN-BUFFER* ".emacs")
(global-set-key (kbd "<f1>") '(lambda () (interactive) (switch-to-buffer *MAIN-BUFFER*)))
(global-set-key (kbd "<f2>") 'evil-buffer)
(global-set-key (kbd "<f3>") '(lambda () (interactive) (with-current-buffer (setq *MAIN-BUFFER* (buffer-name)))))
(global-set-key (kbd "<f5>") '(lambda () (interactive)
                                (let ((index (string-match "\\(.*\\)\\.\\(.\\)" (buffer-name)))
                                      (prename (match-string 1 (buffer-name)))
                                      (suffix (match-string 2 (buffer-name))))
                                  (when (equal index 0)
                                    (if (equal "m" suffix)
                                        (find-file (concatenate 'string prename ".h"))
                                      (if (equal "h" suffix)
                                          (find-file (concatenate 'string prename ".m"))))))))



(provide 'key-bindings)