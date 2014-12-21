" Vim syntax file
" Language:	Sage
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com
" Last Change:	2014 Dec 21

" Python syntax as a base
:runtime! syntax/python.vim
:unlet b:current_syntax


" Docstrings
" ----------------------------------------------

" clear the pythonDoctest and pythonDoctestValue syntax groups
syntax clear pythonDoctest
syntax clear pythonDoctestValue

syn region sageDocstring
    \ start=+^\s*[uU]\=[rR]\=\z('''\|"""\)+
    \ end="\z1" keepend
    \ contains=sageDoctestInput,sageDocstringTitle,sagePrompt

syntax region sageDocstringTitle
    \ start=/^\s\+[A-Z]\+[A-Z ]*:\s*/
    \ end=/$/
    \ oneline
    \ containedin=sageDocstring

syntax region sageDocstringLiteral
    \ start=/``/
    \ end=/``/
    \ oneline
    \ containedin=sageDocstring

" TODO: make sagePrompt work
syntax region sageDoctestInput
    \ matchgroup=sagePrompt
    \ start=/^\s*sage:\s/
    \ end=/$/
    \ oneline
    \ containedin=sageDocstring
    \ contains=ALLBUT,sageDoctestInput

" TODO: make sageDoctestOutput



" Sage
" ----------------------------------------------

" TODO: cython keywords, etc.




" Highlighting
" ----------------------------------------------

hi def link sagePrompt            Identifier
hi def link sageDocstring         Comment

hi link sageDocstringTitle        Statement
hi link sageDocstringLiteral      String

let b:current_syntax = "sage"
