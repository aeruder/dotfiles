" Write history on idle
augroup MyAutoCmd
  autocmd CursorHold * if exists(':rshada') | rshada | wshada | endif
augroup END

tnoremap <C-g> <C-\><C-n>
