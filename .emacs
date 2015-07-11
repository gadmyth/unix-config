(if (string< (emacs-version) "24")
    (load-file (expand-file-name "~/.emacs.shell"))
  (load-file (expand-file-name "~/.emacs.frame")))
