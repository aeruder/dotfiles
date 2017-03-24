""""""""""""""""""""""""
" Use with :Usespaces <num> and we
" are configured with indentation of <num>
" using spaces.
com! -nargs=1 Usespaces
            \ setlocal tabstop=<args> |
            \ setlocal expandtab |
            \ setlocal shiftwidth=<args> |
            \ setlocal softtabstop=<args> |
            \ setlocal nocopyindent

""""""""""""""""""""""""
" Use with :Usetabs <num> and we are
" configured with indentation of <num> using
" tabs.
com! -nargs=1 Usetabs
            \ setlocal tabstop=<args> |
            \ setlocal noexpandtab |
            \ setlocal shiftwidth=<args> |
            \ setlocal softtabstop=<args> |
            \ setlocal copyindent

""""""""""""""""""""""""
" GNU indentation style is a mess
com! -nargs=0 Gnuindent
            \ setlocal tabstop=8 |
            \ setlocal shiftwidth=2 |
            \ setlocal softtabstop=2 |
            \ setlocal noexpandtab |
            \ setlocal nocopyindent

set tabstop=4
set expandtab
set shiftwidth=4
set softtabstop=4
set nocopyindent
