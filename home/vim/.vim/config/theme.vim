" Enable 256 color terminal
set t_Co=256

if g:vimrc_profile < 0
  set background=dark
  colorscheme elflord
  finish
endif

" Enable true color (only in Neovim, but not in urxvt)
if has('nvim') && $TERM !~# '^rxvt' && exists('+termguicolors')
  set termguicolors
  if &term =~# 'tmux-256color'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  endif
endif

function! s:theme_save(name)
  let l:theme_name = a:name
  if match(l:theme_name, '/') != -1
    let l:theme_name = g:colors_name
  endif

  if match(l:theme_name, '/') == -1 &&
        \ g:applied_colorscheme != l:theme_name
    let g:applied_colorscheme = l:theme_name
    call writefile([g:applied_colorscheme], s:cache)
  endif

  let l:theme_path = PJ(g:vim_maindir, "themes", "aeruder.vim")
  if filereadable(l:theme_path)
    execute 'source' fnameescape(l:theme_path)
  endif
endfunction

autocmd MyAutoCmd ColorScheme * call s:theme_save(expand('<amatch>'))

let s:cache = PJ(g:vim_cachedir, "theme.txt")
if ! exists("g:applied_colorscheme")
  set background=dark
  let g:applied_colorscheme = filereadable(s:cache) ? readfile(s:cache)[0] : 'hybrid'
  execute 'colorscheme' g:applied_colorscheme
endif
