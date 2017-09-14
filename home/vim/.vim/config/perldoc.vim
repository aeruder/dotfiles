augroup perldoc_files
  autocmd!
  autocmd FileReadCmd perldoc://**              exe s:perldoc_file_read([], expand('<amatch>'))
  autocmd BufReadCmd  perldoc://**              exe s:perldoc_buf_read([], expand('<amatch>'))
  autocmd FileReadCmd perlfunc://**             exe s:perldoc_file_read(['-f'], expand('<amatch>'))
  autocmd BufReadCmd  perlfunc://**             exe s:perldoc_buf_read(['-f'], expand('<amatch>'))
augroup END

function! s:extract_perldoc(url)
  return a:url[10:]
endfunction

" largely stolen from perldoc.vim
function! s:perldoc_view()
  let cwd = getcwd()

  let split_modifier = get(g:, 'perldoc_split_modifier', '')
  if !bufexists(s:buf_nr)
    exe 'leftabove ' . split_modifier . 'new'
    file `="[Perldoc]"`
    let s:buf_nr = bufnr('%')
  elseif bufwinnr(s:buf_nr) == -1
    exe 'leftabove ' . split_modifier . 'split'
    execute s:buf_nr . 'buffer'
    delete _
  elseif bufwinnr(s:buf_nr) != bufwinnr('%')
    execute bufwinnr(s:buf_nr) . 'wincmd w'
  endif

  " countermeasure for auto-cd script
  execute ':lcd ' . cwd
  call s:setup_buffer()
  call s:read_into_buffer(url)
endfunction

function! s:read_into_buffer(url)
  setlocal modifiable
  normal ggdG
  silent execute "0".s:perldoc_file_read(a:url)
  normal gg
  setlocal filetype=man
  setlocal nomodifiable
endfunction

function! s:setup_buffer()
  setlocal filetype=
  setlocal bufhidden=delete
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal nobuflisted
  setlocal modifiable
  setlocal nocursorline
  setlocal nocursorcolumn
  setlocal iskeyword+=:
  setlocal iskeyword-=-
  let b:man_sect = "ignore"
endfunction

function! s:is_perldoc_file(file)
  if stridx(a:file, "::") != -1
    return 0
  elseif stridx(a:file, "/") != -1
    return 1
  endif
  return 0
endfunction

function! s:perldoc_file_read(url) abort
  let filename = s:extract_perldoc(a:url)
  if s:is_perldoc_file(filename)
    return "read !perldoc -T -otext -F ".shellescape(filename)
  else
    return "read !perldoc -T -otext ".shellescape(filename)
  endif
endfunction

function! s:perldoc_buf_read(url) abort
  call s:setup_buffer()
  call s:read_into_buffer(a:url)
  return ''
endfunction
