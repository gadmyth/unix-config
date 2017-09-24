;;; hydra-config.el --- config the hydra
(require 'hydra)
(require 'utility)

(defhydra hydra-shell (:exit t)
  "Shell"
  ("m" multi-term "multi-term")
  ("e" eshell "eshell")
  ("i" ielm "ielm")
  ("q" nil "cancel"))
(defhydra hydra-launcher (:exit t)
  "Launch"
  ("s" hydra-shell/body "shell")
  ("q" nil "cancel"))
(global-set-key (kbd "C-c C-r") 'hydra-launcher/body)

(defhydra hydra-buffer (:exit t)
  "buffer"
  ("t" (switch-to-buffer "timeline.org") "timeline")
  ("w" (switch-to-buffer "wd-proj.org") "wd-proj")
  ("q" nil "cancel"))
(global-set-key (kbd "C-c b") 'hydra-buffer/body)

(provide 'hydra-config)
