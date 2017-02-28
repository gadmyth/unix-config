" Vim syntax file
" Language:   ztxlog
" Maintainer: xbeta (http://xbeta.info)
set fdc=2
set lsp=5
syn match zhead "^*.\+" "星号打头算标题
"syn match zhead2 "^\d\(\.\d\d\=\)\=\s\+.*"
syn match zhead2 "^\s*\(<-\|->\).*"
syn match ztxt1 "`1"
syn match day67 "...\/\(六\|日\).*"
hi zhead guifg=green gui=bold
hi link zhead2 zhead
hi ztxt1 guifg=red guibg=red
hi day67 guifg=lightblue
set fdm=expr
set foldexpr=Myindent(v:lnum)
func! Myindent(lnum)
 let s:a=strlen(matchstr(getline(v:lnum), '^\*\+\s\+\|^\d\(\.\d\d\=\)\=\s\+.*'))
 if s:a > 3
     return '>2'
 elseif s:a == 2
     return '>1'
 else
     return '='
 endif
endf