" Set mapleader

" set runtimepath+=~/vim/runtimepath
let root = "/mnt/data"
exec "set runtimepath+=".root."/vim/runtimepath"

let mapleader = ","
let java_highlight_all=1

function! MySys()
	return has("win32") ? "windows" : "linux"
endfunction

if MySys() == 'linux'
	"Fast reloading of the .gvimrc
	map <silent><leader>ss :source ~/.gvimrc<CR>
	"Fast editing of .gvimrc
	map <silent><leader>ee :e ~/.gvimrc<CR>
	"When .vimrc is edited, reload it
	autocmd! bufwritepost .gvimrc source ~/.gvimrc
elseif MySys() == 'windows'
	map <silent><leader>ss :source ~/_gvimrc<CR>
	map <silent><leader>ee :e ~/_gvimrc<CR>
	autocmd! bufwritepost .gvimrc source ~/_gvimrc
endif


" for highlight_current_line
let g:loaded_highlightCurLine=1

filetype plugin on
filetype indent on
syntax on

" no menu
set guioptions-=m
" no tool
set guioptions-=T
" no left roller
set guioptions-=l
set guioptions-=L
" no right roller
set guioptions-=r
set guioptions-=R
"-----------------
" colo evening
set autoindent "set ai
set number "set nu
set nowrap
set hlsearch "set hls
set incsearch "set inc
set cursorline

set ic sc

set nobackup
set nowritebackup "set nowb
set noswapfile
" set acd
" 安静模式，关闭响铃和闪烁
set novisualbell

set noerrorbells
set t_vb=
" 改变文件后自动加载
set autoread
" 在查找的时候，如果到了顶部或底部的话，不进行提示
set shortmess+=s
"set wildmenu
"set ruler " Always show current position

set splitbelow splitright
set matchpairs+=<:>
set fileencodings=ucs-bom,utf-8,cp936
set fileencoding=utf-8
set encoding=utf-8

set tabstop=8
set softtabstop=4
set shiftwidth=4
" expandtab是针对tabstop而言的
set expandtab

" 在命令行用空格补全时，可以看到所有的可以补全的项
set wildmenu

" 文尾不换行
set noeol

" for cscope
set cscopequickfix=e-,d-,c-,g-,i-,t-,f-,s-
set cscopetag
set csto=1
set cspc=3


" default font
map <silent><leader>fs0 :setlocal guifont=<CR>
" bigger font
map <silent><leader>fs1 :setlocal guifont=Courier\ 14<CR>
map <silent><leader>fs2 :setlocal guifont=Courier\ 16<CR>
map <silent><leader>fs3 :setlocal guifont=Courier\ 18<CR>
map <silent><leader>ro :setlocal nomodifiable nowrite readonly<CR>
map <silent><leader>rw :setlocal modifiable write noreadonly<CR>

" Status line
set laststatus=2
set statusline=				     " clear the statusline
set statusline+=b%-0.3n\                      " buffer number
set statusline+=w%{winnr()}\                  " window number
set statusline+=0x%B\                       " character value
set statusline+=%(%l,%c%V%)\               " line, character
set statusline+=%<%P\                         " file position
set statusline+=%t\                          " filename
set statusline+=%h%m%r%w                     " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}] " file type
"set statusline+=%=                           " right align remainder

"for emacs
nnoremap <C-X><C-S> :w<CR>
nnoremap <C-X><C-F> :e 
nnoremap <C-X>b :b 

nnoremap <S-TAB> gt
nnoremap <C-TAB> gT

nnoremap <silent><leader>fn :echo expand("%")<CR>
nnoremap <silent><leader>nh :noh<CR>
nnoremap <silent><leader>ww :w<CR>
" 这就是我一直找的
set showcmd

" 窗口的跳转
command! -nargs=1 GW :exec <args> "wincmd w"
nnoremap <silent><leader>gw :GW()<LEFT>
nnoremap <silent><A-1> :GW(1)<CR>
nnoremap <silent><A-2> :GW(2)<CR>
nnoremap <silent><A-3> :GW(3)<CR>
nnoremap <silent><A-4> :GW(4)<CR>
nnoremap <silent><A-5> :GW(5)<CR>
nnoremap <silent><A-6> :GW(6)<CR>
nnoremap <silent><A-7> :GW(7)<CR>
nnoremap <silent><A-8> :GW(8)<CR>
nnoremap <silent><A-9> :GW(9)<CR>

nnoremap <silent><C-A-J> :wincmd j<CR>
nnoremap <silent><C-A-K> :wincmd k<CR>
nnoremap <silent><C-A-H> :wincmd h<CR>
nnoremap <silent><C-A-L> :wincmd l<CR>

" status line 高亮
hi StatusLine term=bold,reverse cterm=bold,reverse gui=bold,reverse guibg=white guifg=#CC9933
hi Error term=bold,reverse cterm=bold,reverse ctermfg=15 ctermbg=12 gui=bold,reverse guibg=white guifg=Yellow
hi ErrorMsg term=bold,reverse cterm=bold,reverse ctermfg=15 ctermbg=12 gui=bold guibg=gray20 guifg=Yellow
hi Pmenu ctermbg=13 guibg=gray

inoremap ;( ()<LEFT>
inoremap ;[ []<LEFT>
inoremap ;{ {}<LEFT>
inoremap ;4 {<ESC>o}<ESC>O<c-t>
inoremap ;> <><LEFT>
inoremap ;q ''<LEFT>
inoremap ;e ""<LEFT>

" emacs
inoremap <C-B> <LEFT>
inoremap <C-F> <RIGHT>
inoremap <C-E> <END>
inoremap <C-A> <HOME>

" when wrap is set, gj is to line down what you see
nnoremap <C-J> gj
nnoremap <C-K> gk
nnoremap <C-H> 10zh
nnoremap <C-L> 9zl

nnoremap <C-X>k :bd<CR>

" for grep
" nnoremap <C-S-N> :cn<CR>
" nnoremap <C-S-P> :cp<CR>
" nnoremap <C-S-L> :cl<CR>
" vimgrep the cusor word
" nnoremap <C-S-G> :vimgrep /<C-R>=expand("<cword>")<CR>/j **/*.java<CR>
" quickfix的<C-L>与上面的<C-L>冲突了

"nmap <M-j> :m+<cr>
"nmap <M-k> :m-2<cr>
"vmap <M-l> :m'>+<cr>`<my`>mzgv`yo`z
"vmap <M-h> :m'<-2<cr>`>my`<mzgv`yo`z

au BufNewFile,BufRead *.zlg setfiletype ztxlog
au BufNewFile,BufRead *.etd setfiletype easytodo
au BufNewFile *.xml runtime xmlheader.vim
au BufNewFile *.java runtime javaheader.vim
au BufNewFile *.erl runtime erlangheader.vim
map <silent><leader>read :runtime tmpread.vim<CR>

let g:java_input_print=1
let g:java_input_comment=1
au Filetype java runtime .java.vim
au Filetype cpp,c runtime .cpp.vim
au Filetype jsp runtime .jsp.vim

runtime workingset.vim

" for winmanager
let g:winManagerWindowLayout='NERDTree|BufExplorer,TagList'
let g:NERDTree_title="NERD Tree"
func! NERDTree_Start()
	exec "NERDTree"
endfunc
func! NERDTree_IsValid()
	return 1
endfunc

let g:Tagbar_title="__Tagbar__"
func! Tagbar_Start()
	exec "TagbarOpen"
endfunc
func! Tagbar_IsValid()
	return 1
endfunc

let g:tagbar_left = 1

nnoremap <silent><leader>wm :set winwidth=30\|set winminwidth=30 \| WMToggle<CR>
nmap <silent> <leader>hl <Plug>MarkSet

vmap <silent> <leader>hl <Plug>MarkSet
nmap <silent> <leader>hh <Plug>MarkClear
vmap <silent> <leader>hh <Plug>MarkClear
nmap <silent> <leader>hr <Plug>MarkRegex
vmap <silent> <leader>hr <Plug>MarkRegex

hi MarkWord1 ctermbg=Cyan     ctermfg=Black guibg=#8CCBEA    guifg=white
hi MarkWord2 ctermbg=Green    ctermfg=Black guibg=#A4E57E    guifg=white
hi MarkWord3 ctermbg=Yellow   ctermfg=Black guibg=#FFDB72    guifg=white
hi MarkWord4 ctermbg=Red      ctermfg=Black guibg=#FF7272    guifg=white
hi MarkWord5 ctermbg=Magenta ctermfg=Black guibg=#FFB3FF    guifg=white
hi MarkWord6 ctermbg=Blue     ctermfg=Black guibg=#9999FF    guifg=white

function! GuiTabLabel() 
    " add the tab number 
    let label = '['.tabpagenr() 
    " modified since the last save? 
    let buflist = tabpagebuflist(v:lnum) 
    for bufnr in buflist 
    if getbufvar(bufnr, '&modified') 
    let label .= '*' 
    break 
    endif 
    endfor 
    " count number of open windows in the tab 
    let wincount = tabpagewinnr(v:lnum, '$') 
    if wincount > 1 
    let label .= ', '.wincount 
    endif 
    let label .= '] ' 
    " add the file name without path information 
    let n = bufname(buflist[tabpagewinnr(v:lnum) - 1]) 
    let label .= fnamemodify(n, ':t')
    return label
endfunction 
set guitablabel=%-15.15{tabpagenr()}
set showtabline=2
set clipboard=unnamed

fun! IncFunSize(inc)
    if !exists('+guifont')
	    return
    endif
    let s:defaultfont = 'Ubuntu Mono 11'
    if a:inc == 0 || empty(&guifont)
	    left &guifont = s:defaultfont
	    return
    endif
    let &guifont = substitute(&guifont, 'd+$', '=submatch(0)+'.a:inc, '')
endfun
