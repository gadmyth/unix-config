function! Find_in_cscope(tag, innertag, target)
	let _tag = a:tag
	let _innertag = a:innertag
	let _target = a:target
	try
		let _ct = (_tag != "tab" ? (_tag != "_" ? _tag : "") : "")
		exec _ct."cscope find "._innertag." "._target
	catch /.*/
		echo "Can't cscope find "._target
	endtry
endfunc
command! -complete=tag_listfiles -nargs=+ WrapFuncFIC call Find_in_cscope(<f-args>)

nnoremap ,cf :WrapFuncFIC _ f <C-R>=expand("<cword>")<CR><CR>
nnoremap ,ce :WrapFuncFIC _ e <C-R>=expand("<cword>")<CR><CR>
nnoremap ,cc :WrapFuncFIC _ c <C-R>=expand("<cword>")<CR><CR>
nnoremap ,cg :WrapFuncFIC _ g <C-R>=expand("<cword>")<CR><CR>
nnoremap ,cd :WrapFuncFIC _ d <C-R>=expand("<cword>")<CR><CR>

cabbr CF WrapFuncFIC _ f
cabbr CE WrapFuncFIC _ e
cabbr CC WrapFuncFIC _ c
cabbr CG WrapFuncFIC _ g
cabbr CD WrapFuncFIC _ d

nnoremap ,sf :WrapFuncFIC s f <C-R>=expand("<cword>")<CR><CR>
nnoremap ,se :WrapFuncFIC s e <C-R>=expand("<cword>")<CR><CR>
nnoremap ,sc :WrapFuncFIC s c <C-R>=expand("<cword>")<CR><CR>
nnoremap ,sg :WrapFuncFIC s g <C-R>=expand("<cword>")<CR><CR>
nnoremap ,sd :WrapFuncFIC s d <C-R>=expand("<cword>")<CR><CR>

cabbr SF WrapFuncFIC s f
cabbr SE WrapFuncFIC s e
cabbr SC WrapFuncFIC s c
cabbr SG WrapFuncFIC s g
cabbr SD WrapFuncFIC s d

" find the file under cursor
" ,ff is equal to gf, but when back to a java file, the suffixesadd is auto set to .java only, no .xml
func! SetSuffixesadd()
	set suffixesadd=.xml,.java,.aidl
endfunc

nnoremap ,ff :Superff _ <C-R>=expand("<cword>")<CR><CR>
nnoremap ,sf :Superff s <C-R>=expand("<cword>")<CR><CR>
nnoremap ,tf :Superff tab <C-R>=expand("<cword>")<CR><CR>

cabbr ff Superff _
cabbr sf Superff s
cabbr tf Superff tab

command! -complete=tag_listfiles -nargs=+ Superff call Find_file_in_tags_and_cscope(<f-args>)

function! Find_file_in_tags_and_cscope(tag, file)
	let _tag = a:tag
	let _target = a:file
	try
		let _bt = (_tag != "tab" ? (_tag != "_" ? _tag : "") : "")
		exec _bt."buffer "._target
	catch /.*/
		echo "Can't buf "._target
		try
			call SetSuffixesadd()
			let _ft = (_tag != "_" ? _tag : "")
			exec _ft."find "._target
		catch /.*/
			echo "Can't find "._target
			try
				let _ct = (_tag != "tab" ? (_tag != "_" ? _tag : "") : "")
				exec _ct."cscope find f "._target
			catch /.*/
				echo "Can't cscope find "._target
			endtry
		endtry
	endtry
endfunc

" prevent R.layout.comp_fielditem to be R/layout/comp_fielditem when use gf
"set isfname-=.
"set isfname-=/
"set isfname-=\

