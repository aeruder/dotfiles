highlight ExtraWhitespace ctermbg=red guibg=red
augroup MyAutoCmd
  autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
  autocmd ColorScheme * match ExtraWhitespace /\(\s\+$\)\|\(\($\n\s*\)\+\%$\)/
  autocmd BufWinEnter * match ExtraWhitespace /\(\s\+$\)\|\(\($\n\s*\)\+\%$\)/
  autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
  autocmd InsertLeave * match ExtraWhitespace /\(\s\+$\)\|\(\($\n\s*\)\+\%$\)/
  autocmd BufWinLeave * call clearmatches()
augroup END
