call denite#custom#map('insert', '<C-n>', '<denite:move_to_next_line>', 'noremap')
call denite#custom#map('insert', '<C-p>', '<denite:move_to_previous_line>', 'noremap')
call denite#custom#map('insert', '<C-g>', '<denite:quit>', 'noremap')

" Ag command on grep source
call denite#custom#var('grep', 'command', ['ag'])
call denite#custom#var('grep', 'default_opts',
    \ ['--smart-case', '--ignore', '.git', '--ignore', '.svn', '--skip-vcs-ignores', '--hidden'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', [])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])

call denite#custom#source('_', 'matchers', ['matcher_substring'])
call denite#custom#source('grep', 'matchers', ['matcher_substring2'])
" call denite#custom#option('_', 'direction', 'aboveleft')
" call denite#custom#option('_', 'direction', 'bottomright')

" Add custom menus
let s:menus = {}

let s:menus.zsh = {
      \ 'description': 'Edit your import zsh configuration',
      \ 'shortcut': 'z',
      \ }
let s:menus.zsh.file_candidates = [
      \ ['zshrc', '~/.config/zsh/.zshrc'],
      \ ['zshenv', '~/.zshenv'],
      \ ]

let s:menus.my_commands = {
      \ 'description': 'Example commands'
      \ }
let s:menus.my_commands.command_candidates = [
      \ ['Split the window', 'vnew'],
      \ ['Open zsh menu', 'Denite menu:zsh'],
      \ ]

call denite#custom#var('menu', 'menus', s:menus)
