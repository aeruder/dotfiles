if exists('did_load_filetypes')
  finish
endif

augroup filetypedetect " {{{
    au BufNewFile,BufRead build_log.txt     setf changelog
    au BufNewFile,BufRead *.phys            setf cmphys
    au BufNewFile,BufRead *.vb              setf vbnet
    au BufNewFile,BufRead *.il              setf ilasm
    au BufNewFile,BufRead *.adoc            setf asciidoc
    au BufNewFile,BufRead *.txt             call s:CheckIsNote()
augroup END " }}}

function! s:CheckIsNote()
  let l:thisfile = expand('%:p:h')
  let l:is_note = 0
  for l:n in g:notes_directories
    if l:thisfile == l:n
      let l:is_note = 1
      break
    endif
  endfor
  if l:is_note
    setf votl
  endif
endfunction

" vim: set ts=2 sw=2 tw=80 et fdm=marker :
