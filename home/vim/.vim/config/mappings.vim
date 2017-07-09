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

" easymotion to comma
nmap , <Plug>(easymotion-overwin-f)


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

" Main keybindings {{{
call s:push_leader("\<Space>")
  " nnoremap <silent> <SID>(HighlightOnlyOperator) :set opfunc=HighlightOnlyOperator<cr>g@
  " xnoremap <silent> <SID>(HighlightOnlyOperator) :<c-u>call HighlightOnlyOperator(visualmode())<cr>
  " nmap <leader>y <SID>(HighlightOnlyOperator)
  " xmap <leader>y <SID>(HighlightOnlyOperator)
  nnoremap <leader>* :<C-u>DeniteCursorWord grep -buffer-name=grep<CR>

  " APPLICATIONY THINGS
  call s:push_leader("a")
    " Toggle in and out of hex dump mode
    nnoremap <leader>h :Hexmode<Cr>
    nnoremap <leader>t :TagbarToggle<Cr>
    " Open undotree
    nnoremap <leader>u :UndotreeToggle<Cr>
    " Neoterm run last command
    nnoremap <leader>r :<C-u>call neoterm#do("!!\r")<Cr>
  call s:pop_leader()

  " BUFFERS
  call s:push_leader("b")
    nnoremap <leader>b :Denite buffer<Cr>
    nnoremap <leader>d :BufKillD<Cr>
    nnoremap <leader>D :BufKillD!<Cr>
    " wipeout other buffers besides those that are visible
    nnoremap <leader>m :Wipeout<Cr>
    nnoremap <leader>r :Denite file_old<Cr>
    " Open a scratch buffer
    nnoremap <leader>s :Scratch<Cr>
    nnoremap <leader>t :Denite filetype<Cr>
  call s:pop_leader()

  " Denite everything
  call s:push_leader("d")
    nnoremap <leader>% :call denite#start([{'name': 'file_rec', 'args': [expand("%:p:h")]}])<Cr>
    nnoremap <leader>/ :Denite line<Cr>
    nnoremap <leader>b :Denite buffer<Cr>
    nnoremap <leader>d :Denite -resume<Cr>
    nnoremap <leader>f :Denite file_rec<Cr>
    nnoremap <leader>g :<C-u>Denite grep -buffer-name=grep<CR>
    nnoremap <leader>l :Denite location_list -buffer-name=list<Cr>
    " nnoremap <leader>n :call denite#start([{'name': 'file_rec', 'args': [g:notes_directories[0]]}])<Cr>
    nnoremap <leader>o :Denite outline<Cr>
    nnoremap <leader>p :call denite#start([{'name': 'file_rec', 'args': [expand("%:p:h:h")]}])<Cr>
    nnoremap <leader>r :Denite file_old<Cr>
    nnoremap <leader>t :Denite filetype<Cr>
    nnoremap <leader>v :call MyDeniteDirectoryThenFile(PJN(PTN(g:vim_cachedir), "dein", "repos"))<Cr>
    nnoremap <leader>w :<C-u>DeniteCursorWord grep -buffer-name=grep<CR>
  call s:pop_leader()

  nmap <leader>e <Plug>(easymotion-overwin-f)

  " FILE STUFF
  call s:push_leader("f")
    nnoremap <leader>* :<C-u>DeniteCursorWord line<CR>
    nnoremap <leader>/ :<C-u>Denite line<CR>
    nnoremap <leader>c :cd %:h<Cr>:let @+ = fnamemodify(<SID>transform_file("absolute"), ":h")<Cr>
    call s:push_leader("e")
      execute "nnoremap <leader>d :n ".PJ(g:vim_configdir, "mappings.vim")."<Cr>"
      nnoremap <leader>D :n $MYVIMRC<Cr>
      execute "nnoremap <leader>p :n ".PJ(g:vim_configdir, "plugins.yaml")."<Cr>"
    call s:pop_leader()
    nnoremap <leader>l :lcd %:h<Cr>
    nnoremap <leader>o :<C-u>Denite outline<CR>
    nnoremap <leader>u :<C-u>exe "e" fnameescape(PJ(g:vim_maindir, "UltiSnips", &filetype . ".snippets"))<Cr>
    nnoremap <silent><leader>w :write<Cr>:nohlsearch<Cr>
    vnoremap <silent><leader>w <Esc>:write<Cr>:nohlsearch<Cr>
    call s:push_leader("y")
      nnoremap <leader>d :let @+ = fnamemodify(<SID>transform_file("relative"), ":h")<CR>:echo @+<CR>
      nnoremap <leader>D :let @+ = fnamemodify(<SID>transform_file("absolute"), ":h")<CR>:echo @+<CR>
      nnoremap <leader>y :let @+ = <SID>transform_file("relative")<CR>:echo @+<CR>
      nnoremap <leader>Y :let @+ = <SID>transform_file("absolute")<CR>:echo @+<CR>
      nnoremap <leader>l :let @+ = <SID>transform_file("relative,line")<CR>:echo @+<CR>
      nnoremap <leader>L :let @+ = <SID>transform_file("absolute,line")<CR>:echo @+<CR>
      nnoremap <leader>h :let @+ = <SID>transform_file("github")<CR>:echo @+<CR>
      nnoremap <leader>H :let @+ = <SID>transform_file("github,line")<CR>:echo @+<CR>
      nnoremap <leader>g :let @+ = <SID>transform_file("gitlab")<CR>:echo @+<CR>
      nnoremap <leader>G :let @+ = <SID>transform_file("gitlab,line")<CR>:echo @+<CR>
      nnoremap <leader>z :let @+ = <SID>transform_file("ziprecruiter")<CR>:echo @+<CR>
      nnoremap <leader>Z :let @+ = <SID>transform_file("ziprecruiter,line")<CR>:echo @+<CR>
    call s:pop_leader()
  call s:pop_leader()

  " GIT
  call s:push_leader("g")
    nnoremap <leader>a :<c-u>Gcommit --amend<Cr>
    nnoremap <leader>b :<c-u>Gblame<Cr>
    nnoremap <leader>B :<c-u>Gblame -M -C -C<Cr>
    nnoremap <leader>c :<c-u>Gcommit<Cr>
    nnoremap <leader>d :<c-u>Gdiff<Cr>
    nnoremap <leader>l :<c-u>Glog<Cr>
    nnoremap <leader>P :<c-u>Git push-sandbox<Cr>
    nnoremap <leader>r :<c-u>Gread<Cr>
    nnoremap <leader>s :<c-u>Denite gitstatus<Cr>
    nnoremap <leader>S :<c-u>Gstatus<Cr>
    nnoremap <leader>v :Gitv --all<Cr>
    nnoremap <leader>V :Gitv! --all<Cr>
    vnoremap <leader>V :Gitv! --all<cr>
    nnoremap <leader>w :<c-u>Gwrite<Cr>
  call s:pop_leader()

  " HIGHLIGHT manipulations
  call s:push_leader("h")
    " Highlight spacing errors
    nnoremap <leader><space> /\v(\s+$)\|(<space>\ze<tab>)<CR>
    " Highlight tabs
    nnoremap <leader><Tab> /<Tab><CR>
    nnoremap <leader>D :%s/<C-R>///g<CR>
    nnoremap <leader>c :nohlsearch<Cr>
    nnoremap <leader>g!D :%g!/<C-R>//d<CR>
    nnoremap <leader>gD :%g/<C-R>//d<CR>
    vnoremap <leader>D :s/<C-R>///g<CR>
    vnoremap <leader>g!D :g!/<C-R>//d<CR>
    vnoremap <leader>gD :g/<C-R>//d<CR>
    " Highlight just pasted text
    nnoremap <expr> <leader>v '`['.strpart(getregtype(), 0, 1).'`]'
  call s:pop_leader()

  " NOTES STUFF (VIMWIKI)
  call s:push_leader("n")
    let g:vimwiki_map_prefix = "\<Space>n"
    nmap <silent> <Leader>w <Plug>VimwikiIndex
    nmap <silent> <Leader>t <Plug>VimwikiTabIndex
    nmap <silent> <Leader>s <Plug>VimwikiUISelect
    nmap <silent> <Leader>i <Plug>VimwikiDiaryIndex
    " DIARY
    call s:push_leader("d")
      nmap <silent> <Leader>i <Plug>VimwikiDiaryGenerateLinks
      nmap <silent> <Leader>w <Plug>VimwikiMakeDiaryNote
      nmap <silent> <Leader>t <Plug>VimwikiTabMakeDiaryNote
      nmap <silent> <Leader>y <Plug>VimwikiMakeYesterdayDiaryNote
    call s:pop_leader()
  call s:pop_leader()

  " PROJECT STUFF
  call s:push_leader("p")
    nnoremap <leader>% :call denite#start([{'name': 'file_rec', 'args': [expand("%:p:h")]}])<Cr>
    nnoremap <leader>. :Denite file_rec<Cr>
    nnoremap <leader>b :call denite#start([{'name': 'file_rec', 'args': [expand("%:p:h")]}])<Cr>
    nnoremap <leader>d :call MyDeniteDirectoryThenFile(".")<Cr>
    nnoremap <leader>D :Denite directory_rec -default-action=cd<Cr>
    nnoremap <leader>f :Denite file_rec<Cr>
    nnoremap <leader>l :Denite location_list -buffer-name=list<Cr>
    " nnoremap <leader>n :call denite#start([{'name': 'file_rec', 'args': [g:notes_directories[0]]}])<Cr>
    nnoremap <leader>p :call denite#start([{'name': 'file_rec', 'args': [expand("%:p:h:h")]}])<Cr>
    nnoremap <leader>v :call MyDeniteDirectoryThenFile(PJN(PTN(g:vim_cachedir), "dein", "repos"))<Cr>
  call s:pop_leader()

  " QUIT stuff
  call s:push_leader("q")
    nnoremap <leader>q :q<Cr>
  call s:pop_leader()

  " SEARCHING
  call s:push_leader("s")
    nnoremap <leader>* :<C-u>DeniteCursorWord grep -buffer-name=grep<CR>
    nnoremap <leader>. :<C-u>Denite grep -buffer-name=grep<CR>
    nnoremap <leader>r :<C-u>Denite -resume -buffer-name=grep<Cr>
    nnoremap <leader>] :<C-u>Denite -resume -buffer-name=grep -cursor-pos=+1 -immediately<Cr>
    nnoremap <leader>[ :<C-u>Denite -resume -buffer-name=grep -cursor-pos=-1 -immediately<Cr>
    nnoremap <leader>w :<C-u>DeniteCursorWord grep -buffer-name=grep<CR>
  call s:pop_leader()

  " TOGGLES
  call s:push_leader("t")
    nnoremap <leader>c :Denite colorscheme -no-quit<Cr>
  call s:pop_leader()

  " VIM
  call s:push_leader("v")
    nnoremap <leader>c :botr copen<Cr>
    " executing lines
    nnoremap <leader>e :exe getline(".")<CR>
    vnoremap <leader>e :<C-w>exe join(getline("'<","'>"),'<Bar>')<CR>
    nnoremap <leader>h :Denite help<Cr>
    nnoremap <leader>p :<C-u>Denite dein<CR>
    nnoremap <leader>P :<C-u>call <SID>profile_toggle()<Cr>
    nnoremap <leader>r :<C-u>call dein#remote_plugins()<CR>
    nnoremap <leader>s :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
          \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
          \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
    execute "nnoremap <leader>r :source" PJ(g:vim_configdir, "vimrc") "<cr>"
    execute "nnoremap <leader>R :call dein#clear_state() \\\| source" PJ(g:vim_configdir, "vimrc") "<cr>"
    nnoremap <leader>U :call dein#update()<Cr>
  call s:pop_leader()

  " WINDOW STUFF
  call s:push_leader("w")
    nnoremap <leader><bar> <C-w>v
    nnoremap <leader>-     <C-w>s
    nnoremap <leader>=     <C-w>=
    nnoremap <leader>d     <C-w>c
    nnoremap <leader>h     <C-w>h
    nnoremap <leader>j     <C-w>j
    nnoremap <leader>k     <C-w>k
    nnoremap <leader>l     <C-w>l
    nnoremap <leader>s     <C-w>s
    nnoremap <leader>v     <C-w>v
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

      " Align = signs
      vnoremap <leader>= :Tab /=.*<CR>
      " Reindent SQL
      vnoremap <leader>S :!sqlformat -k upper -i lower -r --comma_first COMMA_FIRST -<Cr>
    call s:pop_leader()
    nnoremap <leader>p "*p
    nnoremap <leader>P "*P
    vnoremap <leader>y "*y
  call s:pop_leader()
call s:pop_leader()
" }}}

function! s:profile_toggle()
  if v:profiling
    profile stop
    exe "n" fnameescape(s:profile_toggle_file)
  else
    let s:profile_toggle_file = tempname()
    exe "profile start" fnameescape(s:profile_toggle_file)
  endif
endfunction
function! s:transform_file_sort_helper(a, b)
  return a:a['from'] == a:b['from'] ? 0 : a:a['from'] < a:b['from'] ? 1 : -1
endfunction
function! s:transform_file(opts)
  let options = {
        \ "line": 0,
        \ "remote": "origin",
        \ "branch": "master",
        \ }
  let handler = "relative"
  let handlers = split("relative,absolute", ",")
  let githandlers = {}
  let githandlers.github = {}
  let githandlers.github.url = "https://github.com/{repo}/blob/{branch}/{relfile}"
  let githandlers.gitlab = {}
  let githandlers.gitlab.url = "https://github.com/{repo}/blob/{branch}/{relfile}"
  let githandlers.ziprecruiter = {}
  let githandlers.ziprecruiter.url = "https://git.ziprecruiter.com/{repo}/blob/{branch}/{relfile}"
  let handlers = handlers + keys(githandlers)
  for a in split(a:opts, ',')
    if index(handlers,a) != -1
      let handler = a
    else
      let idx = stridx(a, "=")
      if idx == -1
        let options[a] = 1
      else
        let key = a[0:(idx-1)]
        let val = a[(idx+1):]
        let options[key] = val
      endif
    endif
  endfor

  let absfile = expand("%:p")
  let projroot = projectroot#guess(absfile)
  let relfile = expand("%")
  let idx = stridx(absfile, projroot)
  if idx != -1
    let relfile = absfile[(idx+len(projroot)+1):]
    if stridx(relfile, "/") == 0
      let relfile = relfile[1:]
    endif
  endif

  let ret = relfile
  if handler == "relative"
    let ret = relfile
  elseif handler == "absolute"
    let ret = absfile
  elseif has_key(githandlers, handler)
    try
      let url = system(["git", "--git-dir=".PJ(projroot,".git"), "remote", "get-url", "--push", options['remote']])
      if v:shell_error != 0
        throw "git failed: ".url
      endif
      let url = split(url, "\n", 1)[0]
      let parts = split(url, ":", 1)
      if parts[0] == "http" || parts[1] == "https"
        let parts = split(url, "/", 1)
        let repo = PJ(parts[3], parts[4])
      else
        let repo = parts[1]
      endif
      if repo[-4:] == ".git"
        let repo = repo[:-5]
      endif
      let urlt = githandlers[handler]['url']
      let parts = []
      let parts += [{"from": stridx(urlt, "{repo}"),    "len": 6, "to": repo}]
      let parts += [{"from": stridx(urlt, "{branch}"),  "len": 8, "to": options['branch']}]
      let parts += [{"from": stridx(urlt, "{relfile}"), "len": 9, "to": relfile}]
      call sort(parts, "s:transform_file_sort_helper")
      for part in parts
        let urlt = strcharpart(urlt, 0, part['from']) . part['to'] . strcharpart(urlt, part['from'] + part['len'])
      endfor
      let ret = urlt
    catch
      echom v:exception
    endtry
  endif

  if options['line']
    if has_key(githandlers, handler)
      let ret = ret . "#L" . line('.')
    else
      let ret = ret . ":" . line('.')
    endif
  endif

  return ret
endfunction

if g:vimrc_profile >= 0
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
    nmap <silent>j <Plug>(accelerated_jk_j)
    nmap <silent>k <Plug>(accelerated_jk_k)
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
    xmap <leader>v  <Plug>Commentary
    nmap <leader>v  <Plug>CommentaryLine
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
  if dein#tap('splitjoin.vim') "{{{
    nmap <silent> gJ <Plug>SplitjoinJoin
    nmap <silent> gS <Plug>SplitjoinSplit
  endif
  "}}}
endif
