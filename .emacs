(if (string< (emacs-version) "24")
    (load-file ".emacs.shell")
  (load-file ".emacs.frame"))
