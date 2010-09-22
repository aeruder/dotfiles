" Vim syntax file
" Language:        CMBuild phys file
" Maintainer:      Andrew Ruder <Andrew.E.Ruder@raytheon.com>
" Latest Revision: 2009-08-31

if exists("b:current_syntax")
    finish
endif

syn keyword cmphysCommand foreach setenv unsetenv
syn keyword cmphysVariable inputs outputs

syn match  cmBadBracket    +^\s*\zs[{}]+

syn match  cmphysPreproc   +^\s*#\(ifdef\|ifndef\|endif\).*+ contains=cmphysComment
syn match  cmphysVar1      +[$@][a-zA-Z0-9_]\++
syn match  cmphysVar2      +[$@]{[a-zA-Z0-9_]\+}+
syn match  cmphysAutoarray +%\.[a-zA-Z0-9_]\++

syn region cmphysComment  matchgroup=NONE start="--" end="$" keepend
syn region cmphysString   matchgroup=NONE start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=cmphysVar1,cmphysVar2,cmphysComment
syn region cmphysPhase    matchgroup=cmphysPhaseName start=+^\s*\w\+\s*{+ end="^\s*}" contains=ALLBUT,cmBadBracket transparent fold
syn sync fromstart

hi def link cmphysCommand     Keyword
hi def link cmphysPreproc     PreProc
hi def link cmphysVariable    Keyword
hi def link cmphysVar1        Identifier
hi def link cmphysVar2        Identifier
hi def link cmphysString      String
hi def link cmphysPhaseName   Label
hi def link cmphysComment     Comment
hi def link cmphysAutoarray   Special
hi def link cmBadBracket      Error
