finish

Plug 'aeruder/vim-showmap'
let g:showmap_auto_whatis_all = 1
if !empty(globpath(&rtp, "autoload/showmap.vim"))
  " nmap <expr> <Space> showmap#helper("<lt>Space>", "n")
endif

" Find qt headers
if isdirectory("/usr/local/qt")
    nnoremap <leader>bQ :CtrlP /usr/include/qt<Cr>
elseif isdirectory("/usr/include/qt4")
    nnoremap <leader>bQ :CtrlP /usr/include/qt4<Cr>
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             highlight operator                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! HighlightOnlyOperator(type) abort
  let regsave = @@
  let selsave = &selection
  let &selection = 'inclusive'

  if a:type =~? 'v'
    silent execute "normal! gvy"
  elseif a:type == 'line'
    silent execute "normal! '[V']y"
  else
    silent execute "normal! `[v`]y"
  endif

  let &selection = selsave

  let @/ = "\\V".escape(@@, "\\")
  let @@ = regsave
endfunction
