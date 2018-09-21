;;; package --- .emacs.frame.el
;;; Commentary:
;;; Code:
(defvar ts-init (current-time))

;; load path
(add-to-list 'load-path (expand-file-name "~/emacs"))
(add-to-list 'load-path (expand-file-name "el-pre-scripts" "~/emacs"))
(add-to-list 'load-path (expand-file-name "el-extends" "~/emacs"))

(require 'packages)

(require-package
 'warnings
 (setq display-warning-minimum-level :emergency))

;; load script files at first
(require 'script-extends)
(load-pre-script-files)

(install-packages-if-needed)

(require 'frames)
(require 'windows)

(require-package
 'customized-dir
 (customized-dir-init))

(require 'mode-bars)
(require 'fonts)
(require 'themes)

(require 'shells)

(defalias 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(global-unset-key (kbd "C-SPC"))
(require 'irc-config)
(require 'tab-config)
(require 'clipboard)
(require 'codings)
(require 'codec)
(require 'dates)

(require 'version-controll)
(require 'swiper-config)
(eval-after-load "textmate" '(add-to-list '*textmate-project-roots* ".svn"))
(eval-after-load "xcscope" '(ignore-errors (add-to-list 'cscope-indexer-suffixes "*.java")))


(require-if-installed
 'smartparens
 (smartparens-global-mode))

(require-if-installed 'expand-region (global-set-key (kbd "C-=") 'er/expand-region))
(require 'lisping-snippet)

(require-package
 'yas-config
 (yas-global-mode))

(global-auto-complete-mode)

(require 'abbrev-config)
(require 'anythings)
(require 'uniquify-config)

(require-package
 'wcy-desktop
 (wcy-desktop-init))


(require 'workspace)

(switch-proxy nil)

(require-if-installed 'alpha (transparency-set-value *default-trans-value*))
  
(require 'org-config)

(require-if-installed
 'smex
 (global-set-key (kbd "M-x") 'smex))

(require 'ido-config)
(require 'eww-config)

(require 'slime-config)
(require 'projectile-config)
(require 'scales)

(require 'files-config)

;(require 'evil-config)

(require 'el-server)
(require 'el-server-extend)
(require 'servers)
(require 'eyebrowse-config)
(require 'key-bindings)
(require 'holiday-config)
(require 'dired++)

;(require 'annot)

(require 'emoji-config)
(require-if-installed 'eredis (require 'redis-config))
(require 'sudo-edit)
(require 'web-config)
(require 'smart-compile-config)
(require 'frames)

;; languages
(require 'python-config)
	          
;; load must files at last
(load-must-files)

;; load script files at last
(load-extend-script-files)

;; maximize the frame
(toggle-frame-maximized)
(message "end : %.2f" (float-time (time-since ts-init)))

(provide '.emacs.frame)
;;; .emacs.frame.el ends here
