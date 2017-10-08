;;; package --- packages.el
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("org" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))
(add-to-list 'package-archives '("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
(add-to-list 'package-archives '("marmalade" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/"))
(package-initialize)
;; the slime should git clone from github
(add-to-list 'load-path (expand-file-name "elpa/slime" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "el-extends" "~/emacs"))
(defvar required-packages
  (list 'try
        'xcscope
        'async
        'window-numbering
        'evil
        'evil-visualstar
        'smex
        'swiper
        'counsel
        'textmate
        'elnode
        'sudo-edit
        'smart-compile
        'restclient
        'ob-restclient
        'wgrep
        'wgrep-ag
        'htmlize ;; org-export
        'diff-hl
        'auto-complete
        'anything
        'yasnippet
        'smartparens
        'multi-term
        'flycheck
        'vkill
        ))

(defvar tool-packages
  (list 'org-jira
        'emacs-edbi
        'ace-jump-mode
        'sos
        'ov
        'narrow-indirect
        'multifiles
        'lice
        'cal-china-x
        'eredis
        'dirtree
        'foreign-regexp
        'annotate
        'expand-region
        'edit-list
        'projectile
        'js2-mode
        'js2-refactor
        'web-mode
        'emmet-mode
        'gnuplot
        'urlenc
        'url-shortener
        'json-reformat
        'ac-emoji
        'google-translate
        'jq-mode
        'look-mode))

(defvar option-packages
  (list 'alpha
        'elisp-format
        'with-namespace
        'vcomp
        'elisp-sandbox
        'slime
        'magit
        'git-timemachine
        'datetime-format))

(require 'autoload)
(setq generated-autoload-file "~/emacs/autoloads.el")
(update-directory-autoloads "~/emacs")
(kill-buffer "autoloads.el")
(require 'autoloads)

(defvar *sync-package* t)
(if *sync-package*
    (mapcar #'require-package required-packages))

(defmacro require-if-installed (package &rest body)
  `(if (package-installed-p ,package)
       (progn
         (require ,package)
         (message "%S required!" ,package)
         (progn ,@body))
     (message "%S not installed!" ,package)))

(defmacro require-packages-if-installed (packages &rest body)
  `(let ((all-package-installed t))
     (dolist (p ,packages)
       (if (not (package-installed-p p))
           (setq all-package-installed nil)))
     (when all-package-installed
         (dolist (p ,packages)
           (require p))
         (progn ,@body))))

(provide 'packages)
;;; packages.el ends here
