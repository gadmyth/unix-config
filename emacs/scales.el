(require 'ov)
(defun ov-double-height ()
  (interactive)
  (ov (point-min) (point-max) '(face (:height 1.25))))
(defun ov-half-height ()
  (interactive)
  (ov (point-min) (point-max) '(face (:height 0.8))))
(defun ov-reset-height ()
  (interactive)
  (ov-clear))

(defun manual-scale (delta)
  (if (zerop delta)
      (progn 
        (ov-reset-height)
        (setq *mac-scale-amount* 0
              *linux-scale-amount* 0))
    (if (> delta 0)
        (progn
          (ov-double-height)
          (incf *mac-scale-amount*)
          (incf *linux-scale-amount*))
      (progn
        (ov-half-height)
        (decf *mac-scale-amount*)
        (decf *linux-scale-amount*)))))

(global-set-key (kbd "C-x C-=") (lambda () (interactive) (manual-scale 1)))
(global-set-key (kbd "C-x C--") (lambda () (interactive) (manual-scale -1)))
(global-set-key (kbd "C-x C-0") (lambda () (interactive) (manual-scale 0)))

;; scale-amount has been defined in workspace.el
(defvar *mac-scale-amount* 2)
(defvar *linux-scale-amount* 3)
(defun scale-large (&optional amount)
  (let ((scale-amount
		 (if amount amount
		   (if (eq window-system 'ns) *mac-scale-amount* *linux-scale-amount*))))
	(dotimes (i scale-amount)
	  (ov-double-height))))


(provide 'scales)