if !exists("g:scratchfiles_dir")
  if isdirectory(expand('~/Project'))
    let g:scratchfiles_dir = expand('~/Project/scratch')
  elseif isdirectory(expand('~/proj'))
    let g:scratchfiles_dir = expand('~/proj/scratch')
  elseif has('mac')
    let g:scratchfiles_dir = expand('~/Documents/Scratch')
  else
    let g:scratchfiles_dir = expand('~/scratch')
  endif
endif
if !exists("g:scratchfiles_cmd")
  let g:scratchfiles_cmd = "e"
endif

function! s:new_scratchfile(name) abort
  let wheredot = stridx(a:name, ".")
  if wheredot < 0
    let basename = "untitled"
    let extension = a:name
  elseif wheredot == 0
    let basename = "untitled"
    let extension = a:name[1:]
  else
    let basename = a:name[0:wheredot-1]
    let extension = a:name[wheredot+1:]
  endif

  if extension == ""
    let extension = "txt"
  endif

  let curtime = strftime("%Y-%m")
  let directory = PJ(g:scratchfiles_dir, curtime)
  if !isdirectory(directory)
    call mkdir(directory)
  endif

  let i = -1
  while 1
    if i < 0
      let testfile = printf("%s.%s", basename, extension)
    else
      let testfile = printf("%s.%03d.%s", basename, i, extension)
    endif
    let testfile = PJ(directory, testfile)
    if !bufexists(testfile) && !isdirectory(testfile) && !filereadable(testfile) && filewritable(testfile) == 0
      break
    endif
    let i = i + 1
  endwhile

  exe g:scratchfiles_cmd fnameescape(testfile)
endfunction

command! -nargs=* ScratchFileNew call s:new_scratchfile(<f-args>)
