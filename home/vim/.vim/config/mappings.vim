"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              regular mappings                               "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Make j and k go through wrapped lines
" nnoremap j gj
" nnoremap k gk

" Use 'jk' instead of ESC
inoremap jk <ESC>

" Start new line from any cursor position
inoremap <S-Return> <C-o>o

" Quick substitute within selected area
xnoremap s :s//g<Left><Left>

" Improve scroll, credits: https://github.com/Shougo
nnoremap <expr> zz (winline() == (winheight(0)+1) / 2) ?
  \ 'zt' : (winline() == 1) ? 'zb' : 'zz'
noremap <expr> <C-f> max([winheight(0) - 2, 1])
  \ ."\<C-d>".(line('w$') >= line('$') ? "L" : "M")
noremap <expr> <C-b> max([winheight(0) - 2, 1])
  \ ."\<C-u>".(line('w0') <= 1 ? "H" : "M")
noremap <expr> <C-e> (line("w$") >= line('$') ? "j" : "3\<C-e>")
noremap <expr> <C-y> (line("w0") <= 1         ? "k" : "3\<C-y>")

" Select blocks after indenting
xnoremap < <gv
xnoremap > >gv|

" Use tab for indenting in visual mode
vnoremap <Tab> >gv|
vnoremap <S-Tab> <gv

" Navigation in command line
cnoremap <C-j> <Left>
cnoremap <C-k> <Right>
cnoremap <C-h> <Home>
cnoremap <C-l> <End>
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>
cnoremap <C-d> <C-w>

" Drag current line/s vertically and auto-indent
vnoremap mk :m-2<CR>gv=gv
vnoremap mj :m'>+<CR>gv=gv
noremap  mk :m-2<CR>
noremap  mj :m+<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                             hierarchy mappings                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:mapleaders = []

function! MyDeniteDirectoryThenFile(dir)
  let l:ret = denite#start([{'name': 'directory_rec', 'args': [a:dir]}], {'default_action': 'yank'})
  if len(l:ret) > 0
    let l:ret = denite#start([{'name': 'file_rec', 'args': [l:ret[0].action__path]}])
  endif
  return l:ret
endfunction

function! s:push_leader(app)
  if len(s:mapleaders) == 0
    call add(s:mapleaders, a:app)
  else
    call add(s:mapleaders, s:mapleaders[-1].a:app)
  endif
  let g:mapleader = s:mapleaders[-1]
  let l:indentstr = repeat("\\ ", 2*(len(s:mapleaders)-1))
  if empty(l:indentstr)
    execute "nnoremap <leader>? :n +/^".l:indentstr."call\\ s:push_leader ".PJ(g:vim_configdir, "mappings.vim")."<Cr>"
  else
    execute "nnoremap <leader>? :n +/^".l:indentstr."call\\ s:push_leader(\"".a:app."\") ".PJ(g:vim_configdir, "mappings.vim")."<Cr>"
  endif
endfunction

function! s:pop_leader()
  call remove(s:mapleaders, -1)
  if len(s:mapleaders) == 0
    unlet g:mapleader
  else
    let g:mapleader = s:mapleaders[-1]
  end
endfunction

call s:push_leader("\<Space>")
  nnoremap <silent> <SID>(HighlightOnlyOperator) :set opfunc=HighlightOnlyOperator<cr>g@
  xnoremap <silent> <SID>(HighlightOnlyOperator) :<c-u>call HighlightOnlyOperator(visualmode())<cr>
  nmap <leader>y <SID>(HighlightOnlyOperator)
  xmap <leader>y <SID>(HighlightOnlyOperator)
  nnoremap <leader>* :<C-u>DeniteCursorWord grep -buffer-name=grep<CR>

  " WINDOW STUFF
  call s:push_leader("w")
    " window movement
    nnoremap <leader>h     <C-w>h
    nnoremap <leader>j     <C-w>j
    nnoremap <leader>k     <C-w>k
    nnoremap <leader>l     <C-w>l
    nnoremap <leader>d     <C-w>c
    nnoremap <leader>=     <C-w>=
    nnoremap <leader>-     <C-w>s
    nnoremap <leader><bar> <C-w>v
  call s:pop_leader()

  " BUFFERS
  call s:push_leader("b")
    " buffer list and recent files
    nnoremap <leader>b :Denite buffer file_old<Cr>
    " Buffer list
    nnoremap <leader>B :Denite buffer<Cr>
    " Recent file list
    nnoremap <leader>r :Denite file_mru<Cr>
    " Filetype
    nnoremap <leader>t :Denite filetype<Cr>
    " wipeout other buffers
    nnoremap <leader>m :Wipeout<Cr>
    " delete buffer
    nnoremap <leader>d :BufKillD<Cr>
    nnoremap <leader>D :BufKillD!<Cr>
    " Open a scratch buffer
    nnoremap <leader>s :Scratch<Cr>
  call s:pop_leader()

  " SEARCHING
  call s:push_leader("s")
    nnoremap <leader>. :<C-u>Denite grep -buffer-name=grep<CR>
  call s:pop_leader()

  " PROJECT STUFF
  call s:push_leader("p")
    " Recursive from current directory
    nnoremap <leader>f :Denite file_rec<Cr>
    nnoremap <leader>. :Denite file_rec<Cr>
    nnoremap <leader>d :call MyDeniteDirectoryThenFile(".")<Cr>
    nnoremap <leader>D :Denite directory_rec -default-action=cd<Cr>
    nnoremap <leader>l :Denite location_list -buffer-name=list<Cr>
    " Recursive from directory of current file
    nnoremap <leader>% :call denite#start([{'name': 'file_rec', 'args': [expand("%:p:h")]}])<Cr>
    nnoremap <leader>b :call denite#start([{'name': 'file_rec', 'args': [expand("%:p:h")]}])<Cr>
    " Recursive from parent directory of current file
    nnoremap <leader>p :call denite#start([{'name': 'file_rec', 'args': [expand("%:p:h:h")]}])<Cr>
    " Recursive list all notes
    nnoremap <leader>n :call denite#start([{'name': 'file_rec', 'args': [g:notes_directories[0]]}])<Cr>
    " Vim plugins
    nnoremap <leader>v :call MyDeniteDirectoryThenFile(PJN(PTN(g:vim_cachedir), "dein", "repos"))<Cr>
  call s:pop_leader()

  " COLORSCHEME
  call s:push_leader("T")
    nnoremap <leader>c :Denite colorscheme -no-quit<Cr>
  call s:pop_leader()

  " TEXT MANIPULATIONS
  call s:push_leader("x")
    " ALIGNMENT
    call s:push_leader("a")
      " Align comma'd lists like
      "
      " a, b, c, d
      " foo, bar, blah, baz
      "
      " to this when selected and ran through this command:
      "
      " a,   b,   c,    d
      " foo, bar, blah, baz
      vnoremap <leader>, :Tab /[;,]<CR>gv:s/\v(\s+)([,;])\s?/\2\1/g<CR>gv:s/\v\s+$//<CR>:let @/=""<CR>
    call s:pop_leader()
  call s:pop_leader()

  " VISUAL/HIGHLIGHT manipulations
  call s:push_leader("v")
    nnoremap <leader>c :nohlsearch<Cr>
    nnoremap <leader>D :%s/<C-R>///g<CR>
    nnoremap <leader>gD :%g/<C-R>//d<CR>
    nnoremap <leader>g!D :%g!/<C-R>//d<CR>
    vnoremap <leader>D :s/<C-R>///g<CR>
    vnoremap <leader>gD :g/<C-R>//d<CR>
    vnoremap <leader>g!D :g!/<C-R>//d<CR>
    " Highlight spacing errors
    nnoremap <leader><space> /\v(\s+$)\|(<space>\ze<tab>)<CR>
    " Highlight tabs
    nnoremap <leader><Tab> /<Tab><CR>
    " Highlight just pasted text
    nnoremap <expr> <leader>v '`['.strpart(getregtype(), 0, 1).'`]'
  call s:pop_leader()

  " FILE STUFF
  call s:push_leader("f")
    nnoremap <leader>l :lcd %:h<Cr>
    nnoremap <leader>c :cd %:h<Cr>
    nnoremap <leader>eD :n $MYVIMRC<Cr>
    nnoremap <silent><leader>w :write<Cr>:nohlsearch<Cr>
    vnoremap <silent><leader>w <Esc>:write<Cr>:nohlsearch<Cr>
    execute "nnoremap <leader>ed :n ".PJ(g:vim_configdir, "mappings.vim")."<Cr>"
    nnoremap <Leader>y :let @+=expand("%:p")<CR>:echo 'Copied to clipboard.'<CR>
    nnoremap <Leader>o :<C-u>Denite outline<CR>
    nnoremap <Leader>/ :<C-u>Denite line<CR>
    nnoremap <Leader>* :<C-u>DeniteCursorWord line<CR>
  call s:pop_leader()

  " APPLICATIONY THINGS
  call s:push_leader("a")
    " Open undotree
    nnoremap <leader>u :UndotreeToggle<Cr>
    " Toggle in and out of hex dump mode
    nnoremap <leader>h :Hexmode<Cr>
    nnoremap <leader>t :TagbarToggle<Cr>
  call s:pop_leader()

  " GIT
  call s:push_leader("g")
    nnoremap <leader>s :<c-u>Gstatus<Cr>
    nnoremap <leader>l :<c-u>Glog<Cr>
    nnoremap <leader>d :<c-u>Gdiff<Cr>
    nnoremap <leader>b :<c-u>Gblame<Cr>
    nnoremap <leader>B :<c-u>Gblame -M -C -C<Cr>
    nnoremap <leader>r :<c-u>Gread<Cr>
    nnoremap <leader>w :<c-u>Gwrite<Cr>
    nnoremap <leader>c :<c-u>Gcommit<Cr>
    nnoremap <leader>a :<c-u>Gcommit --amend<Cr>
    nnoremap <leader>P :<c-u>Git push-sandbox<Cr>
    nnoremap <leader>v :Gitv --all<Cr>
    nnoremap <leader>V :Gitv! --all<Cr>
    vnoremap <Leader>V :Gitv! --all<cr>
  call s:pop_leader()

  " VIM
  call s:push_leader("V")
    " executing lines
    nnoremap <silent><leader>p :<C-u>Denite dein -no-quit<CR>
    nnoremap <leader>e :exe getline(".")<CR>
    vnoremap <leader>e :<C-w>exe join(getline("'<","'>"),'<Bar>')<CR>
    nnoremap <leader>c :botr copen<Cr>
    nnoremap <leader>h :Denite help<Cr>
    nnoremap <leader>s :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
          \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
          \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
    execute "nnoremap <leader>r :source" PJ(g:vim_configdir, "vimrc") "<cr>"
  call s:pop_leader()

  " QUIT stuff
  call s:push_leader("q")
    nnoremap <leader>q :q<Cr>
  call s:pop_leader()

call s:pop_leader()

if dein#tap('vim-operator-surround') "{{{
  map <silent>sa <Plug>(operator-surround-append)
  map <silent>sd <Plug>(operator-surround-delete)
  map <silent>sr <Plug>(operator-surround-replace)
  nmap <silent>saa <Plug>(operator-surround-append)<Plug>(textobj-multiblock-i)
  nmap <silent>sdd <Plug>(operator-surround-delete)<Plug>(textobj-multiblock-a)
  nmap <silent>srr <Plug>(operator-surround-replace)<Plug>(textobj-multiblock-a)
endif
"}}}
if dein#tap('vim-operator-replace') "{{{
  xmap p <Plug>(operator-replace)
endif

"}}}
if dein#tap('vim-operator-flashy') "{{{
  map y <Plug>(operator-flashy)
  nmap Y <Plug>(operator-flashy)$
endif

"}}}
if dein#tap('vim-niceblock') "{{{
  xmap I  <Plug>(niceblock-I)
  xmap A  <Plug>(niceblock-A)
endif

"}}}
if dein#tap('accelerated-jk') "{{{
  nmap <silent>j <Plug>(accelerated_jk_gj)
  nmap <silent>k <Plug>(accelerated_jk_gk)
endif

"}}}
if dein#tap('vim-asterisk') "{{{
  map *   <Plug>(asterisk-g*)
  map g*  <Plug>(asterisk-*)
  map #   <Plug>(asterisk-g#)
  map g#  <Plug>(asterisk-#)

  map z*  <Plug>(asterisk-z*)
  map gz* <Plug>(asterisk-gz*)
  map z#  <Plug>(asterisk-z#)
  map gz# <Plug>(asterisk-gz#)
endif

"}}}
if dein#tap('vim-expand-region') "{{{
  xmap v <Plug>(expand_region_expand)
  xmap V <Plug>(expand_region_shrink)
endif

"}}}
if dein#tap('dsf.vim') "{{{
  nmap dsf <Plug>DsfDelete
  nmap csf <Plug>DsfChange
  omap af <Plug>DsfTextObjectA
  xmap af <Plug>DsfTextObjectA
  omap if <Plug>DsfTextObjectI
  xmap if <Plug>DsfTextObjectI
endif

"}}}
if dein#tap('sideways.vim') "{{{
  nnoremap <silent> m" :SidewaysJumpLeft<CR>
  nnoremap <silent> m' :SidewaysJumpRight<CR>
  omap <silent> a, <Plug>SidewaysArgumentTextobjA
  xmap <silent> a, <Plug>SidewaysArgumentTextobjA
  omap <silent> i, <Plug>SidewaysArgumentTextobjI
  xmap <silent> i, <Plug>SidewaysArgumentTextobjI
endif

"}}}
if dein#tap('CamelCaseMotion') "{{{
  nmap <silent> e <Plug>CamelCaseMotion_e
  xmap <silent> e <Plug>CamelCaseMotion_e
  omap <silent> e <Plug>CamelCaseMotion_e
  nmap <silent> w <Plug>CamelCaseMotion_w
  xmap <silent> w <Plug>CamelCaseMotion_w
  omap <silent> w <Plug>CamelCaseMotion_w
  nmap <silent> b <Plug>CamelCaseMotion_b
  xmap <silent> b <Plug>CamelCaseMotion_b
  omap <silent> b <Plug>CamelCaseMotion_b
endif

"}}}
if dein#tap('vim-commentary') "{{{
  xmap <Leader>v  <Plug>Commentary
  nmap <Leader>v  <Plug>CommentaryLine
  xmap gc  <Plug>Commentary
  nmap gc  <Plug>Commentary
  omap gc  <Plug>Commentary
  nmap gcc <Plug>CommentaryLine
  nmap cgc <Plug>ChangeCommentary
  nmap gcu <Plug>Commentary<Plug>Commentary
endif
"}}}
if dein#tap('vim-textobj-multiblock') "{{{
  omap <silent> ab <Plug>(textobj-multiblock-a)
  omap <silent> ib <Plug>(textobj-multiblock-i)
  xmap <silent> ab <Plug>(textobj-multiblock-a)
  xmap <silent> ib <Plug>(textobj-multiblock-i)
endif

"}}}
if dein#tap('vim-textobj-function') "{{{
  omap <silent> af <Plug>(textobj-function-a)
  omap <silent> if <Plug>(textobj-function-i)
  xmap <silent> af <Plug>(textobj-function-a)
  xmap <silent> if <Plug>(textobj-function-i)
endif
"}}}
