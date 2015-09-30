(require 'evil)

(defun word-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word
          (if bounds
              (buffer-substring-no-properties (car bounds) (cdr bounds))
            nil)))
    word))

(require 'yasnippet)
(defadvice yas-expand (around expand-abbrev-when-word-p)
  (interactive)
  (let ((word (word-at-point)))
    (when word
      (expand-abbrev)))
  ad-do-it)
(ad-activate 'yas-expand)

;; for objc-mode
(add-hook
 'objc-mode-hook
 (lambda ()
   (progn
     (mapc
      (lambda (pair)
        (define-abbrev objc-mode-abbrev-table (car pair) (cdr pair)))
      '(("cd" . "class-def")
        ("ced" . "class-ext-def")
        ("ci" . "class-impl")
        ("ced" . "class-ext-def")
        ("cei" . "class-ext-impl")
        ("pd" . "prop-def")
        ("p2d" . "prop2-def")
        ("pbd" . "prop-block-def")
        ("ai" . "allocinit")
        ("initd" . "init-demo")
        ("cd" . "class-def")
        ("fd" . "func-def")
        ("gfd" . "gfunc-def")
        ("bd" . "block-declare")
        ("bi" . "block-impl")
        ("b1i" . "block1-impl")
        ("bp" . "block-parameter")
        ("ifb" . "if-block")
        ("ieb" . "ifelse-block")
        ("fore" . "for-enum")
        ("fb" . "func-bracket")
        ("sb" . "sync-block")
        ("pm" . "pragma-mark")
        )))))

(add-hook
 'swift-mode-hook
 (lambda ()
   (mapc
    (lambda (pair)
      (define-abbrev swift-mode-abbrev-table (car pair) (cdr pair)))
    '(("pi" . "π")
      ))))

(setq save-abbrevs nil)

(provide 'abbrev-config)