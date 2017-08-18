;;; package --- frames.el
;;; Commentary:
;;; Code:

(require 'frame)

(defvar *max-frame-width* 0)
(defvar *max-frame-height* 0)

(defadvice toggle-frame-maximized (before mark-frame-maxsize activate)
  "AFTER: , ACTIVATE: ."
  (message "toggle-frame-maximized advice")
  (async-start
   (lambda ()
     (sleep-for 0.5)
     0.5)
   (lambda (result)
     (message "after toggle-frame-maximized %f seconds" result)
     (let* ((frame (selected-frame))
            (fullscreen-value (frame-parameter frame 'fullscreen)))
       (message "width: %d, height: %d, %s" (frame-width frame) (frame-height frame) fullscreen-value)
       (when (or (eq fullscreen-value 'maximized)
                 (eq fullscreen-value 'fullboth))
         (setq *max-frame-width*  (frame-width frame)
               *max-frame-height* (frame-height frame))
         (message "max-width: %d, max-height: %d" *max-frame-width* *max-frame-height*))))))

(add-to-list 'after-make-frame-functions 'toggle-frame-maximized)

(defun set-suitable-frame-size-inner (frame)
  "FRAME: ."
  (set-frame-size
   frame
   (round (* *max-frame-width* 0.5))
   (round (* *max-frame-height* 0.8))))

(defun set-suitable-frame-size ()
  "."
  (interactive)
  (let ((frame (selected-frame)))
    (if (frame-parameter frame 'fullscreen)
        (progn
          (set-frame-parameter frame 'fullscreen nil)
          (async-start
           (lambda ()
             (sleep-for 0.5))
           (lambda (result)
             (set-suitable-frame-size-inner (selected-frame)))))
      (set-suitable-frame-size-inner frame))))

(global-set-key (kbd "C-x 3") (lambda () (interactive) (select-window (split-window-right))))
(if (boundp 'tool-bar-mode) (tool-bar-mode -1))
(if (boundp 'menu-bar-mode) (menu-bar-mode -1))
(if (boundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
(put 'scroll-left 'disabled nil)

(provide 'frames)
;;; frames.el ends here
