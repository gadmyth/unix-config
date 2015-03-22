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


;;; scale-amount has been defined in workspace.el
(defvar *mac-scale-amount* 2)
(defvar *linux-scale-amount* 3)
(defun scale-large (&optional amount)
  (let ((scale-amount
		 (if amount amount
		   (if (eq window-system 'ns) *mac-scale-amount* *linux-scale-amount*))))
	(dotimes (i scale-amount)
	  (ov-double-height))))

(defhydra hydra-enlarge (global-map "C-x" :timeout 1.5)
  ("=" ov-double-height "enlarge")
  ("-" ov-half-height "half")
  ("C-0" ov-reset-height "reset")
  ("q" nil "quit"))

(provide 'hydra-config)
