(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((result (eval-last-sexp nil)))
    (sp-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0))))))

  
(global-set-key (kbd "C-c e") 'eval-and-replace)

(provide 'lisping-snippet)