" *********************************************************************************************
" bgrep.vim  
" *********************************************************************************************
" Description : Uses vim's internal grep support to grep through all open
"               files
" Last Change : 7 Apr 2007 
" Created By  : Andrew Ruder <andy@aeruder.net>
" Version     : 0.1
" Usage       : Vim 7 
"               Stick this file in your ~/.vim/plugin directory or 
" Note        : There are no keymappings, but you could do something like
"               nmap <F3> :Bgrep
"               in your .vimrc
"
"               There is only one command, Bgrep, if you specify an argument
"               it searches all open files for that word and if you don't
"               it searches for the word under your cursor.
"
"               All of the results go into the error panel, so do something
"               like 
"               :botr copen
"               and you will see all the matches, and you can just press
"               enter on a match and go to that file/line.
"
" Contact     : For any comments or bug fixes email me at <andy@aeruder.net>
""""
function! s:BufferList()
	let blist = []
	let mynum = bufnr('$')
	let i = 0

	while i <= mynum
		if bufexists(i) && getbufvar(i, '&buftype') == ''
			let fileName = expand('#'.i.':p')
			if filereadable(fileName)
				call add(blist, fileName)
			endif
		endif
		let i += 1
	endwhile

	return blist
endfunction

function! s:GrepBuffers(...)
	let blist = s:BufferList()
	if a:0 < 1 || a:1 == "" 
		normal yiw
		let search = @@ 
	else
		let search = a:1
	endif
	let search = escape(search, '/')
	for i in range(0, len(blist) - 1)
		let blist[i] = escape(blist[i], ' ')
		let start = "vimgrep"
		if i != 0
			let start = "vimgrepadd"
		endif
		let comm=start." /".search."/j ".blist[i]
		execute "silent! ".comm
	endfor
endfunction

command! -nargs=? Bgrep call s:GrepBuffers(<f-args>)
