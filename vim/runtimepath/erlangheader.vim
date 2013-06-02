let filename=substitute(bufname("%"), '.erl', '', "g")
call setline(1, '-module('.filename.').')
call setline(2, '-export([]).')
