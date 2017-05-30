;;; package --- smart-compile-config.el
;;; Commentary:
;;; Code:

(require 'smart-compile)
(add-to-list 'smart-compile-alist '(objc-mode . "clang -fobjc-arc -framework Foundation -w %f && ./a.out"))

(provide 'smart-compile-config)
;;; smart-compile-config.el ends here