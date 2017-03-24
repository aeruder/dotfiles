function! ZshNamedDirsLoad()
  if (exists("$HOME"))
    let l:namedirs = $HOME . "/.znameddirs"
    if (filereadable(l:namedirs))
      let l:lines = readfile(l:namedirs)
      for l:a in l:lines
        let l:splitdir = split(l:a, " ")
        if (len(l:splitdir) == 2 && match(l:splitdir[0], '[^a-z0-9_A-Z]') == -1)
          exec "let $" . l:splitdir[0] . " = '" . fnameescape(l:splitdir[1]) ."'"
        endif
      endfor
    endif
  endif
endfunction
comm! ZshNamedDirsLoad call ZshNamedDirsLoad()
ZshNamedDirsLoad
