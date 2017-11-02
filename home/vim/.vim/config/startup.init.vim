" Vim Initialization
" ------------------

let g:mapleader="\<Space>"
let g:maplocalleader=';'

" Create missing dirs i.e. cache/{undo,backup}
if ! isdirectory(PJ(g:vim_cachedir, "undo"))
  call mkdir(PJ(g:vim_cachedir, "undo"))
endif
let &undodir=PJ(g:vim_cachedir, "undo")
if ! isdirectory(PJ(g:vim_cachedir, "backup"))
  call mkdir(PJ(g:vim_cachedir, "backup"))
endif
if ! isdirectory(PJ(g:vim_cachedir, "swp"))
  call mkdir(PJ(g:vim_cachedir, "swp"))
endif
let &directory=PJ(g:vim_cachedir, "swp")

if g:vimrc_profile >= 0
  if empty(globpath(&rtp, PJ("autoload", "dein.vim")))
    let s:dein_dir = PJ(g:vim_cachedir, "dein", "repos", "github.com", "Shougo", "dein.vim")
    if ! isdirectory(s:dein_dir)
      execute '!git clone https://github.com/Shougo/dein.vim' fnameescape(PTN(s:dein_dir))
    endif

    execute 'set runtimepath+='.s:dein_dir
  endif
endif

" Disable menu.vim
if has('gui_running')
  set guioptions=Mc
endif

" Disable pre-bundled plugins
let g:loaded_getscript = 1
let g:loaded_getscriptPlugin = 1
let g:loaded_gzip = 1
let g:loaded_logiPat = 1
let g:loaded_matchit = 1
let g:loaded_matchparen = 1
" let g:loaded_netrw = 1
" let g:loaded_netrwPlugin = 1
" let g:loaded_netrwFileHandlers = 1
" let g:loaded_netrwSettings = 1
let g:loaded_rrhelper = 1
let g:loaded_shada_plugin = 1
let g:loaded_spellfile_plugin  = 1
let g:loaded_tar = 1
let g:loaded_tarPlugin = 1
let g:loaded_tutor_mode_plugin = 1
" let g:loaded_2html_plugin = 1
let g:loaded_vimball = 1
let g:loaded_vimballPlugin = 1
let g:loaded_zip = 1
let g:loaded_zipPlugin = 1
