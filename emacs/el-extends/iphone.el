;;; package --- iphone.el
;;; Commentary:
;;; Code:


(defstruct iphone-device
  width
  height
  size
  scale)

(defvar iphone3gs (make-iphone-device
                   :width  320
                   :height 640
                   :size 3.5
                   :scale 1))

(defvar iphone4 (make-iphone-device
                 :width 640
                 :height 960
                 :size 3.5
                 :scale 2))

(defvar iphone5 (make-iphone-device
                 :width 640
                 :height 1136
                 :size 4
                 :scale 2))

(defvar iphone6 (make-iphone-device
                 :width 750
                 :height 1334
                 :size 4.7
                 :scale 2))

(defvar iphone6p (make-iphone-device
                  :width 1080
                  :height 1920
                  :size 5.5
                  :scale 2.46))

(defvar iphone6p-a (make-iphone-device
                  :width 1242
                  :height 2208
                  :size 5.5
                  :scale 3))

(defvar iphone6p-b (make-iphone-device
                  :width 1080
                  :height 1920
                  :size 5.5
                  :scale 3))

(defvar iphone6p-c (make-iphone-device
                  :width 1080
                  :height 1920
                  :size 5.5
                  :scale 2))


(defun get-ppi (i-device)
  "I-DEVICE."
  (let ((width (iphone-device-width i-device))
        (height (iphone-device-height i-device))
        (size (iphone-device-physical-size i-device)))
    (/ (sqrt (+ (* width width) (* height height))) size)))

(defun get-logic-res (i-device)
  "I-DEVICE."
  (let ((width (iphone-device-width i-device))
        (height (iphone-device-height i-device))
        (scale (iphone-device-scale i-device)))
    `(,(/ width scale) . ,(/ height scale))))

(provide 'iphone)
;;; iphone.el ends here