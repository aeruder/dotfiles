let g:tagbar_map_showproto = '.'
let g:tagbar_width = 40
if executable('jsctags')
  let g:tagbar_type_javascript = { 'ctagsbin': 'jsctags' }
endif
" Tagbar settings to recognise Perl Moo..Moose
let g:tagbar_type_perl = {
    \ 'ctagstype'   : 'Perl',
    \ 'kinds' : [
        \ 'p:packages:1:0',
        \ 'u:uses:1:0',
        \ 'r:requires:1:0',
        \ 'e:extends',
        \ 'w:roles',
        \ 'o:ours:1:0',
        \ 'c:constants:1:0',
        \ 'f:formats:1:0',
        \ 'a:attributes',
        \ 's:subroutines',
        \ 'x:around:1:0',
        \ 'l:aliases',
        \ 'd:pod:1:0',
    \ ],
\ }
