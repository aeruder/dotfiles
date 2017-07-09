let g:vimwiki_path_html = ''
let g:vimwiki_index = 'index'
let g:vimwiki_syntax = 'default'
let g:vimwiki_ext = '.wiki'
let g:vimwiki_template_path = PJ(g:vim_maindir, 'wiki-templates')
let g:vimwiki_template_default = 'default'
let g:vimwiki_template_ext = '.html'
let g:vimwiki_css_name = 'style.css'
let g:vimwiki_hl_headers = 1

if !exists("g:vimwiki_list")
  let s:mainwiki = {}
  if has('mac')
    let s:mainwiki.path = expand('~/Documents/Notes')
  else
    let s:mainwiki.path = expand('~/notes')
  endif
  let s:mainwiki.auto_export = 0
  let s:mainwiki.auto_toc = 0
  let s:mainwiki.auto_tags = 1
  let g:vimwiki_list = [s:mainwiki]
endif
