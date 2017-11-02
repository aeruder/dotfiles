if has("win32")
  let g:vim_cachedir=PJ(g:vim_maindir, "cache")
else
  let g:vim_cachedir=PJ(expand(($XDG_CACHE_HOME ? $XDG_CACHE_HOME : '~/.cache')), 'vim')
endif

let g:vim_pconfigdir=PJ(g:vim_configdir, "plugins")
