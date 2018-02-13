;;; package --- key-bindings.el
;;; Commentary:
;;; Code:

(require 'eyebrowse-config)

;;; F zone
(setq *MAIN-BUFFER* ".emacs")
(global-set-key (kbd "<f1>") '(lambda () (interactive) (switch-to-buffer *MAIN-BUFFER*)))
(global-set-key (kbd "<f1>") 'previous-buffer)
(global-set-key (kbd "<f1>") 'ivy-evil-show-marks)
(global-set-key (kbd "<f2>") 'evil-buffer)
(global-set-key (kbd "<f3>") '(lambda () (interactive) (with-current-buffer (setq *MAIN-BUFFER* (buffer-name)))))
;(global-set-key (kbd "<f3>") 'next-buffer)
;(global-set-key (kbd "<f3>") 'bookmark-jump)
(global-set-key (kbd "<f3>") 'eyebrowse-last-window-config)
;(global-set-key (kbd "C-x <f3>") 'bookmark-set)
(global-set-key (kbd "C-x <f3>") 'eyebrowse-list-configs)
;(global-set-key (kbd "C-x c <f3>") 'list-bookmarks)
(global-set-key (kbd "C-x c <f3>") 'eyebrowse-list-actions)
(global-set-key (kbd "<f5>") '(lambda () (interactive)
                                (let ((index (string-match "\\(.*\\)\\.\\(.\\)" (buffer-name)))
                                      (prename (match-string 1 (buffer-name)))
                                      (suffix (match-string 2 (buffer-name))))
                                  (when (equal index 0)
                                    (if (equal "m" suffix)
                                        (find-file (concatenate 'string prename ".h"))
                                      (if (equal "h" suffix)
                                          (find-file (concatenate 'string prename ".m"))))))))

(global-set-key (kbd "<f6>") 'find-library)

(require 'ivy)
(global-set-key (kbd "<f4>") 'ivy-switch-buffer)

(provide 'key-bindings)
;;; key-bindings.el ends here
