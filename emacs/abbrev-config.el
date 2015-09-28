(require 'evil)

(define-key evil-insert-state-map (kbd "TAB") 'expand-abbrev)

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
        ("ai" . "allocinit")
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
        )))))

(setq save-abbrevs nil)

(provide 'abbrev-config)