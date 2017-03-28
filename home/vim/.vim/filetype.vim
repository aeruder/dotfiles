if exists('did_load_filetypes')
  finish
endif

augroup filetypedetect " {{{
    au BufNewFile,BufRead build_log.txt     set filetype=changelog
    au BufNewFile,BufRead *.phys            set filetype=cmphys
    au BufNewFile,BufRead *.vb              set filetype=vbnet
    au BufNewFile,BufRead *.il              set filetype=ilasm
    au BufNewFile,BufRead *.adoc            set filetype=asciidoc
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
    set filetype=votl
  endif
endfunction

" vim: set ts=2 sw=2 tw=80 et fdm=marker :
