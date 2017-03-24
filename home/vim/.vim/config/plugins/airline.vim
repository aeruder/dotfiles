function! MyAirlineRefresh()
  let spc = g:airline_symbols.space
  let g:airline_section_a = airline#section#create_left(['mode', 'crypt', 'paste', 'spell', 'capslock', 'xkblayout', 'iminsert'])
  let g:airline_section_b = airline#section#create(['hunks', 'branch'])
  if exists("+autochdir") && &autochdir == 1
    let g:airline_section_c = airline#section#create(['%<', 'path', spc, 'readonly'])
  else
    let g:airline_section_c = airline#section#create(['%<', 'file', spc, 'readonly'])
  endif
  let g:airline_section_gutter = airline#section#create(['%='])
  let g:airline_section_x = airline#section#create_right(['tagbar', 'filetype'])
  let g:airline_section_y = airline#section#create_right(['ffenc'])
  if winwidth(0) > 80
    let g:airline_section_z = airline#section#create(['windowswap', 'obsession', '%3p%%'.spc, 'linenr', 'maxlinenr', spc.':%3v'])
  else
    let g:airline_section_z = airline#section#create(['%3p%%'.spc, 'linenr',  ':%3v'])
  endif
  let g:airline_section_error = airline#section#create(['ycm_error_count', 'syntastic', 'eclim', 'neomake_error_count', 'ale_error_count'])
  let g:airline_section_warning = airline#section#create(['ycm_warning_count',  'neomake_warning_count', 'ale_warning_count', 'whitespace'])
  AirlineRefresh
endfunction

comm! -bar MyAirlineRefresh call MyAirlineRefresh()
