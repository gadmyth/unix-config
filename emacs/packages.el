;;; package --- packages.el
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))
(package-initialize)
;; the slime should git clone from github
(add-to-list 'load-path (expand-file-name "elpa/slime" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/swiper" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "el-extends" "~/emacs"))
(defvar required-packages
  (list 'alpha
        'diff-hl
        'windmove
        'textmate
        'smex
        'helm-dash
        'smart-compile
        'helm
        'xcscope
        'org
        'ob-restclient
        'evil
        'auto-complete
        'magit
        'git-timemachine
        'ov
        'evil-visualstar
        'slime
        'projectile
        'yasnippet
        'smartparens
        'multi-term
        'ace-jump-buffer
        'ace-jump-mode
        'elnode
        'flycheck
        'anything
        'ac-emoji
        'dirtree
        'cal-china-x
        'hydra
        'window-numbering
        'google-translate
        'restclient
        'eredis
        'js2-mode
        'js2-refactor
        'web-mode
        'emmet-mode
        'sudo-edit))
(setq generated-autoload-file "~/emacs/autoloads.el")
(update-directory-autoloads "~/emacs")
(kill-buffer "autoloads.el")
(require 'autoloads)

(defvar *sync-package* t)
(if *sync-package*
    (mapcar #'require-package required-packages))

(provide 'packages)
;;; packages.el ends here
