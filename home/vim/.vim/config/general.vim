""""""""""""""""""""""""
" If we use special characters, ensure we are doing so with
" utf-8
set encoding=utf-8
set fileformats=unix,dos,mac

"""""""
" mouse
set mouse=nv

"""""""
" Allow virtual edit in block mode
set virtualedit=block

" History saving
set history=2000
if has('nvim')
  "  ShaDa/viminfo:
  "   ' - Maximum number of previously edited files marks
  "   < - Maximum number of lines saved for each register
  "   @ - Maximum number of items in the input-line history to be
  "   s - Maximum size of an item contents in KiB
  "   h - Disable the effect of 'hlsearch' when loading the shada
  set shada='300,<50,@50,s100,h
else
  let &viminfo="'300,<10,@50,h,n".PJ(g:vim_cachedir, "viminfo")
endif

""""""""""""""""""""""""
" Line wrap settings
set nowrap
set textwidth=79
set formatoptions=qrn1
set colorcolumn=
set breakat=\ \ ;:,!?           " Long lines break chars
set nostartofline               " Cursor in same column for few commands
set whichwrap+=h,l,<,>,[,],~    " Move to following line on certain keys
set switchbuf=useopen,usetab    " Jump to the first open window in any tab
set switchbuf+=vsplit           " Switch buffer behavior to vsplit


set showfulltag                 " Show tag and tidy search in completion
set complete=.                  " No wins, buffs, tags, include scanning
set completeopt=menuone         " Show menu even for one item
set completeopt+=noselect       " Do not select a match in the menu
if has('patch-7.4.775')
  set completeopt+=noinsert
endif

if exists('+inccommand')
  set inccommand=nosplit
endif

""""""""""""""""""""""""
" Default split directions
set splitbelow
set splitright

""""""""""""""""""""""""
" Give us some context around the cursor when scrolling
set sidescroll=10 sidescrolloff=10
set scrolloff=4

""""""""""""""""""""""""
" Set how we indent new lines, autoindent seems most
" sane to me (except when filetype indent is available)
set smartindent
set nocindent
set autoindent
set shiftround
filetype plugin indent on

""""""""""""""""""""""""
" Set up our status bar with lots of good information
set noshowmode
set showcmd
set noruler
set report=1
set shortmess=aoOTI     " Shorten messages and don't show intro
set showtabline=2
set tabpagemax=15       " Maximum number of tab pages
set winwidth=75         " Minimum width for current window
set winminwidth=8       " Minimum width for inactive windows
set winheight=10        " Minimum height for active window
set winminheight=4      " Minimum height for inactive windows
set pumheight=20        " Pop-up menu's line height
set helpheight=12       " Minimum help window height
set previewheight=10    " Completion preview height

" Do not display completion messages
" Patch: https://groups.google.com/forum/#!topic/vim_dev/WeBBjkXE8H8
if has('patch-7.4.314')
  set shortmess+=c
endif

" Do not display message when editing files
if has('patch-7.4.1570')
  set shortmess+=F
endif

" For snippet_complete marker
if has('conceal') && v:version >= 703
  set conceallevel=2 concealcursor=niv
endif

""""""""""""""""""""""""
" Make our backspace sane (we want to be able to backspace over everything)
set backspace=indent,eol,start

""""""""""""""""""""""""
" Tab completion in command mode
set wildmenu
set wildmode=list:longest
set wildignore=*.o,*.obj,*.d,*.P,GTAGS,GPATH,GRTAGS

""""""""""""""""""""""""
" When beeping, do it visually not audibly (which half the time
" doesn't work anyway)
set visualbell
set errorbells

""""""""""""""""""""""""
" Highlight the entire line we are on
set cursorline

""""""""""""""""""""""""
" Always enable the status line for the window, even if there is just
" one window.
set laststatus=2

""""""""""""""""""""""""
" Display the line numbers relative to the current line
set relativenumber
set number

""""""""""""""""""""""""
" Persistent undo even if we quit vim.
set undofile

" Time stuff
set timeout ttimeout
set timeoutlen=750  " Time out on mappings
set updatetime=2000 " Idle time to write swap and trigger CursorHold

" Time out on key codes
if has('nvim')
  " https://github.com/neovim/neovim/issues/2017
  set ttimeoutlen=-1
else
  set ttimeoutlen=250
endif

""""""""""""""""""""""""
" Search settings
" If we search for Abc, it is case-sensitive, abc is case-insensitive
set ignorecase
set smartcase
set infercase
" Search as we type
set incsearch
" Highlight our searches
set hlsearch
" Wrap at the end
set nowrapscan

" match stuff
set showmatch       " Jump to matching bracket
set matchpairs+=<:> " Add HTML brackets to pair matching
set matchtime=1
set cpoptions-=m    " showmatch will wait 0.5s or until a char is typed

" magic regex
set magic
set path=.,,**

""""""""""""""""""""""""
" CScope settings
set cscopequickfix=s-,c-,d-,i-,t-,e-,g-
set cscopetag

""""""""""""""""""""""""
" Allow us to switch away from an unsaved buffer
set hidden

""""""""""""""""""""""""
" When we yank text into the unnamed register, automatically put
" it on the clipboard.
" set clipboard=unnamed

""""""""""""""""""""""""
" When doing folds, base it on the syntax
set foldenable
set foldmethod=syntax
set foldnestmax=2
set foldlevelstart=99
set foldtext=s:FoldText()

" Improved Vim fold-text
" See: http://www.gregsexton.org/2011/03/improving-the-text-displayed-in-a-fold/
function! s:FoldText()
  " Get first non-blank line
  let fs = v:foldstart
  while getline(fs) =~? '^\s*$' | let fs = nextnonblank(fs + 1)
  endwhile
  if fs > v:foldend
    let line = getline(v:foldstart)
  else
    let line = substitute(getline(fs), '\t', repeat(' ', &tabstop), 'g')
  endif

  let w = winwidth(0) - &foldcolumn - (&number ? 8 : 0)
  let foldSize = 1 + v:foldend - v:foldstart
  let foldSizeStr = ' ' . foldSize . ' lines '
  let foldLevelStr = repeat('+--', v:foldlevel)
  let lineCount = line('$')
  let foldPercentage = printf('[%.1f', (foldSize*1.0)/lineCount*100) . '%] '
  let expansionString = repeat('.', w - strwidth(foldSizeStr.line.foldLevelStr.foldPercentage))
  return line . expansionString . foldSizeStr . foldPercentage . foldLevelStr
endfunction

""""""""""""""""""""""""
" When using vimdiff, ignore white-space changes
" set diffopt=iwhite,filler
set diffopt=filler
""""""""""""""""""""""""
" Show tabs, trailing spaces, etc..
set list listchars=tab:»·,trail:·,extends:>,precedes:<
set colorcolumn=80,120,121
set display=lastline

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                  vim-notes                                  "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if !exists("g:notes_directories")
  if has('mac')
    let g:notes_directories = [ expand('~/Documents/Notes') ]
  else
    let g:notes_directories = [ expand('~/notes') ]
  endif
endif

let g:notes_suffix = '.txt'
let g:notes_smart_quotes = 1
