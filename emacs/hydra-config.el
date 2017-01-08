;;; hydra-config.el --- config the hydra
(require 'hydra)
(require 'utility)

(defhydra hydra-shell (:exit t)
  "Shell"
  ("e" eshell "eshell")
  ("i" ielm "ielm")
  ("q" nil "cancel"))
(defhydra hydra-launcher (:exit t)
  "Launch"
  ("s" hydra-shell/body "shell")
  ("q" nil "cancel"))
(global-set-key (kbd "C-c C-r") 'hydra-launcher/body)

(require 'helm)
(defhydra hydra-helm (:exit t)
  "helm"
  ("o" helm-occur "occur")
  ("r" helm-resume "resume")
  ("m" helm-imenu "imenu")
  ("q" nil "cancel"))
(global-set-key (kbd "C-c h") 'hydra-helm/body)

(provide 'hydra-config)
