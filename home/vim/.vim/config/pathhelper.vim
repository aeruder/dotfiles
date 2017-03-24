function! PJH(sep, ...)
  let l:list_type = type([])
  let l:joined = []
  for l:v in a:000
    if type(l:v) == l:list_type
      call extend(l:joined, l:v)
    else
      call add(l:joined, l:v)
    endif
  endfor

  return join(l:joined, a:sep)
endfunction

" Some little path utilities similar to os.path.join
" path join
function! PJ(...)
  return call('PJH', ['/'] + a:000)
endfunction

" path join native (win32 uses backslashes)
if has("win32")
  function! PJN(...)
    return call('PJH', ['\'] + a:000)
  endfunction
else
  function! PJN(...)
    return call('PJH', ['/'] + a:000)
  endfunction
end

" convert to native
if has("win32")
  function! PTN(path)
    return substitute(a:path, '/', "\\", "g")
  endfunction
else
  function! PTN(path)
    return a:path
  endfunction
endif

" convert from native
if has("win32")
  function! PFN(path)
    return substitute(a:path, "\\\\", '/', 'g')
  endfunction
else
  function! PFN(path)
    return a:path
  endfunction
endif
