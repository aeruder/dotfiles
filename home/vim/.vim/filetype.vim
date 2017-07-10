if exists('did_load_filetypes')
  finish
endif

augroup filetypedetect " {{{
    au BufNewFile,BufRead build_log.txt     set filetype=changelog
    au BufNewFile,BufRead *.phys            set filetype=cmphys
    au BufNewFile,BufRead *.vb              set filetype=vbnet
    au BufNewFile,BufRead *.il              set filetype=ilasm
    au BufNewFile,BufRead *.adoc            set filetype=asciidoc
augroup END " }}}

" vim: set ts=2 sw=2 tw=80 et fdm=marker :
