if exists("java_input_print")
	inoremap ;pt System.out.println("");<LEFT><LEFT><LEFT>
	inoremap ;PT System.out.println();<LEFT><LEFT>
	"nnoremap <leader>pti iSystem.out.println("");<LEFT><LEFT><LEFT>
	"nnoremap <leader>PTI iSystem.out.println();<LEFT><LEFT>
	"nnoremap <leader>ptj oSystem.out.println("");<LEFT><LEFT><LEFT>
	"nnoremap <leader>PTJ oSystem.out.println();<LEFT><LEFT>
	"nnoremap <leader>ptk OSystem.out.println("");<LEFT><LEFT><LEFT>
	"nnoremap <leader>PTK OSystem.out.println();<LEFT><LEFT>
endif

function! ToggleJavaComment()
	let str = getline(".")
	if(str =~ "^\s*//.*$")
		echo "yes"
	else
		echo "no"
	endif
endfunc
if exists("java_input_comment")
	inoremap ;cm /** */<LEFT><LEFT>
	" make to be comment
	nnoremap <silent><leader>cm 0i//<ESC>
	" uncomment current line
	" 会高亮，搜索不到//不提示出错
	nnoremap <silent><leader>uc 0:s/\/\///<CR>
	" 需要为大段代码加/**/，可能的情况是为某个函数加
	" 命令行模式，visual模式下
endif

inoremap ;pc public class  {<cr><cr>}<up>
inoremap ;main public static void main(String... args) {<cr><cr>}<UP>
syn match jFunc "\<[a-zA-Z_][a-zA-Z_0-9]*\>[^()]*)("me=e-2
syn match jFunc "\<[a-zA-Z_][a-zA-Z_0-9]*\>\s*("me=e-1
hi jFunc gui=NONE guifg=#B5A1FF
"move to gvimrc
"hi Normal guifg=#DDDDDD

" for debug highlight
let java_highlight_debug=1

setlocal nocompatible
setlocal omnifunc=javacomplete#Complete
" 实现在vim编辑java文件时当敲入"."时全能补全
" inoremap <buffer> . <C-X><C-O>

syntax region	cBlock		start="{" end="}" transparent fold
set foldmethod=syntax
set foldlevel=99
hi Folded term=standout guibg=grey20 guifg=gold
