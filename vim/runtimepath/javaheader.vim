let filename=substitute(bufname("%"), '.java', '', "g")
call setline(1, 'public class '.filename)
