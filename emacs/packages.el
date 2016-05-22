(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))
(package-initialize)
;; the slime should git clone from github
(add-to-list 'load-path (expand-file-name "elpa/slime" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/swiper" user-emacs-directory))
(defvar required-packages
  (list 'alpha 'diff-hl 'windmove 'textmate 'smex 'helm-dash 'smart-compile
		'helm 'xcscope 'org 'evil 'auto-complete 'magit
		'ov 'evil-visualstar 'pretty-mode 'slime
		'projectile 'yasnippet 'smartparens 'multi-term
		'ace-jump-buffer 'ace-jump-mode 'elnode 'flycheck 'anything 'ac-emoji
		'dirtree 'cal-china-x 'hydra 'window-numbering 'google-translate))
(setq generated-autoload-file "~/emacs/autoloads.el")
(update-directory-autoloads "~/emacs")
(kill-buffer "autoloads.el")
(require 'autoloads)
(mapcar #'require-package required-packages)

(provide 'packages)
