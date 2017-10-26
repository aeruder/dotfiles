function! AddPerlModule(mod)
  let last_line = line('$')
  let i = 1
  let lastlinewithmod = 1
  while i <= last_line
    let line = getline(i)
    let matches = matchlist(getline(i), '^\(use\|require\|package\)\s\+\([a-zA-Z0-9:]\+\)')
    if len(matches) > 0
      if matches[2] !~ '^namespace::'
        let lastlinewithmod = i
      endif
      if matches[1] == 'use' || matches[1] == 'require'
        if matches[2] == a:mod
          return 1
        endif
      endif
    endif
    let i = i + 1
  endwhile

  call append(lastlinewithmod, 'use '.a:mod.' ();')
  return 1
endfunction
