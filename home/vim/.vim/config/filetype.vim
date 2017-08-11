augroup MyAutoCmd
  " Some filesystems that are mounted in /tmp (particularly on OS X), cause
  " problems with backup files
  autocmd BufRead /tmp/*
    \ setlocal nobackup |
    \ setlocal nowritebackup

  " Space/tab settings for different filetypes
  autocmd FileType make
    \ Usetabs 8

  autocmd FileType gitcommit
    \ set tw=72

  autocmd FileType ruby
    \ Usespaces 2

  autocmd FileType haml
    \ Usespaces 2

  autocmd FileType vim
    \ Usespaces 2

  autocmd FileType perl
    \ Usespaces 2

  autocmd BufReadCmd a/** exe s:diff_buf_read(expand('<amatch>'))
  autocmd BufReadCmd b/** exe s:diff_buf_read(expand('<amatch>'))
augroup END

function! s:diff_buf_read(path)
  let file = a:path[2:]
  let buf = bufnr('%')
  exe "e" file
  exe "doautocmd BufReadPost" file
  exe "bdelete!" buf
  return ''
endfunction
