if !has('nvim')
  finish
endif

" Write history on idle
augroup MyAutoCmd
  autocmd CursorHold * if exists(':rshada') | rshada | wshada | endif
augroup END
