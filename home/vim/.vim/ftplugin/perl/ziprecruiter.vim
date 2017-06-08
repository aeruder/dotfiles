function! <SID>StartTest()
  let myfile = expand("%:p")
  let prove = "SCREEN_LOG_LEVEL=debug zrperl -MCarp::Always /usr/local/zrperl/bin/prove -v"

  " Editing a TCM file
  let matched = matchlist(myfile, '/app/t/tests/\(TestsFor/.*\.pm\)')
  if len(matched) > 0
    call neoterm#do(prove . " app/t/tcm.t :: " . matched[1])
    return 1
  endif

  " Editing a .t file
  let matched = matchlist(myfile, '/\(app/t/.*\.t\)')
  if len(matched) > 0
    call neoterm#do(prove . " " . matched[1])
    return 1
  endif

  " TODO Figure out a way to get zr-run-tests to run the right tests for
  " a specific named file.
  return 0
endfunction

nmap <buffer> <Space>mt :call <SID>StartTest()<Cr>
