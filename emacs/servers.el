(if (and (not (eq window-system 'x))
	   (or (not (boundp 'server-process))
		   (null server-process)))
	(ignore-errors
	 (server-start)))

(provide 'servers)