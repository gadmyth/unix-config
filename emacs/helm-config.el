(eval-after-load "helm" '(setq helm-split-window-default-side 'right))
(global-set-key (kbd "C-,") 'helm-imenu-anywhere)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(setf helm-buffers-fuzzy-matching t)

(require 'helm-dash)
(setq helm-dash-common-docsets '("Android"))

(provide 'helm-config)