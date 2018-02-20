;;; package --- .emacs.frame.el
;;; Commentary:
;;; Code:
(defvar ts-init (current-time))

;; load path
(add-to-list 'load-path (expand-file-name "~/emacs"))
(add-to-list 'load-path (expand-file-name "el-pre-scripts" "~/emacs"))
(add-to-list 'load-path (expand-file-name "el-extends" "~/emacs"))

(require 'warnings)
(setq display-warning-minimum-level :emergency)

;; load script files at first
(require 'script-extends)
(load-pre-script-files)

(require 'packages)

(require 'frames)
(require 'windows)
(require 'files-config)
(customized-dir-init)

(require 'mode-bars)
(require 'fonts)
(require 'themes)

(require 'shells)

(defalias 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq default-abbrev-mode t)
(setq save-abbrevs t)
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
(setq backup-directory-alist (quote (("." . "~/.backups"))))


(smartparens-global-mode)
(require-if-installed 'expand-region (global-set-key (kbd "C-=") 'er/expand-region))
;(mapc (lambda (key) (delete key sp-trigger-keys)) '("\"" "'" "`"))
(require 'lisping-snippet)
(require 'yas-config)
(yas-global-mode)
(global-auto-complete-mode)
(require 'abbrev-config)
(require 'anythings)

(require 'wcy-desktop)
(wcy-desktop-init)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator " :: ") 
(setq uniquify-after-kill-buffer-p t)
(require 'workspace)
(if (not (file-exists-p def-dir))
  (mkdir def-dir))
(setq default-directory def-dir)
(switch-proxy nil)

(require-if-installed 'alpha (transparency-set-value *default-trans-value*))
  
(require 'org-config)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'eww-config)

(require 'slime-config)
(require 'projectile-config)
(require 'scales)

(add-hook 'find-file-hook
	  (lambda ()
	    (progn
	      ;(scale-large)
          (interactive)
          (revert-buffer t t t)
          (scale-large)
          (require 'textmate)
          (textmate-mode)
          (require 'xcscope)
          (cscope-minor-mode)
          )))

(add-hook 'after-save-hook
		  (lambda () (if (string= (buffer-name) ".emacs")
					(byte-compile-file (expand-file-name "~/.emacs")))))

(require 'evil-config)

(require 'el-server)
(require 'el-server-extend)
(require 'servers)
(require 'eyebrowse-config)
(require 'key-bindings)
(require 'holiday-config)
(add-hook 'after-init-hook 'global-flycheck-mode)
(require 'dired++)

;(require 'annot)

(require 'emoji-config)
(require-if-installed 'redis (require 'redis-config))
(require 'sudo-edit)
(require 'web-config)
(require 'smart-compile-config)
(require 'frames)
	          
;; load must files at last
(load-must-files)

;; load script files at last
(load-extend-script-files)

;; maximize the frame
(toggle-frame-maximized)
(message "end : %.2f" (float-time (time-since ts-init)))

(provide '.emacs.frame)
;;; .emacs.frame.el ends here
